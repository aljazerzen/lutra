use itertools::Itertools;
use std::collections::HashMap;

use lutra_bin::ir;
use sqlparser::ast as sql_ast;

use crate::sql::utils::{ExprOrSource, ExprScoped, RelScoped};
use crate::sql::{cr, utils};
use crate::sql::{COL_ARRAY_INDEX, COL_VALUE};
use crate::utils::NameGenerator;

pub fn compile(rel: cr::RelExpr, types: HashMap<&ir::Path, &ir::Ty>) -> sql_ast::Query {
    let mut ctx = Context::new(types);

    let rel_ty = rel.ty.clone();
    let query = ctx.compile_re(rel);

    if rel_ty.kind.is_array() {
        // wrap into a top-level projection to remove array indexes
        let mut query = ctx.scope_into_query_without_index(query, &rel_ty);
        query.order_by = Some(utils::order_by_one(utils::ident(
            None::<&str>,
            COL_ARRAY_INDEX,
        )));
        query
    } else {
        match query.into_sub_rel() {
            Ok(sub_rel) => sub_rel,
            Err(query) => ctx.scope_into_query(query, &rel_ty),
        }
    }
}

pub(super) struct Context<'a> {
    types: HashMap<&'a ir::Path, &'a ir::Ty>,

    pub(super) rel_name_gen: NameGenerator,
    pub(super) rvars: HashMap<usize, (String, ir::Ty)>,
}

impl<'a> Context<'a> {
    pub(super) fn new(types: HashMap<&'a ir::Path, &'a ir::Ty>) -> Self {
        Self {
            types,
            rel_name_gen: NameGenerator::new("r"),
            rvars: Default::default(),
        }
    }

    pub(super) fn get_ty_mat(&self, ty: &'a ir::Ty) -> &'a ir::Ty {
        match &ty.kind {
            ir::TyKind::Ident(path) => self.types.get(path).unwrap(),
            _ => ty,
        }
    }

    fn find_rvar(&self, id: usize) -> &(String, ir::Ty) {
        match self.rvars.get(&id) {
            Some(x) => x,
            None => panic!("cannot find scope id: {id}"),
        }
    }

    fn compile_re(&mut self, rel: cr::RelExpr) -> RelScoped {
        let r = match rel.kind {
            cr::RelExprKind::From(cr::From::Construction(rows)) => {
                self.compile_rel_constructed(rows, &rel.ty)
            }

            cr::RelExprKind::From(cr::From::Table(table_name)) => {
                let mut select = utils::select_empty();

                select
                    .from
                    .push(utils::from(utils::new_table(vec![table_name], None)));

                let ty_tuple = self.get_ty_mat(&rel.ty).kind.as_array().unwrap();
                let ty_fields = self.get_ty_mat(ty_tuple).kind.as_tuple().unwrap();

                let values = Some(utils::value(sql_ast::Value::Null)) // index
                    .into_iter()
                    .chain(
                        ty_fields
                            .iter()
                            .map(|field| utils::ident(None::<&str>, field.name.as_ref().unwrap())),
                    );

                select.projection = self.projection(&rel.ty, values);

                RelScoped::new(utils::sub_rel(
                    utils::query_select(select),
                    Some(self.rel_name_gen.next()),
                ))
            }

            cr::RelExprKind::From(cr::From::Iterator(scope_id)) => {
                let (rel_name, _) = self.find_rvar(scope_id);

                RelScoped {
                    rel_name: rel_name.clone(),
                    rel_vars: Vec::new(),
                }
            }

            cr::RelExprKind::Join(left, right, condition) => {
                let left_ty = left.ty.clone();
                let left = self.compile_re(*left);
                let left_name = left.get_name().to_string();

                let right_ty = right.ty.clone();
                let right = self.compile_re(*right);
                let right_name = right.get_name().to_string();

                let mut scope = left;
                scope.extend(right);

                let mut scope = self.wrap_scoped_rel(scope, &rel.ty);
                let select = self.rel_as_mut_select(&mut scope, &rel.ty);
                select.projection.clear();

                dbg!(self.rel_cols(&rel.ty, true).collect_vec());
                dbg!(self.rel_cols(&left_ty, true).collect_vec());
                dbg!(self.rel_cols(&right_ty, true).collect_vec());

                select.projection.extend(
                    self.projection(
                        &rel.ty,
                        Iterator::chain(
                            self.rel_cols(&left_ty, true)
                                .map(|col| utils::ident(Some(&left_name), col)),
                            self.rel_cols(&right_ty, true)
                                .map(|col| utils::ident(Some(&right_name), col)),
                        ),
                    ),
                );

                if let Some(condition) = condition {
                    let expr = self.compile_expr(*condition);
                    select.selection = Some(expr.expr.into_expr());
                    scope.rel_vars.extend(expr.rel_vars);
                }

                scope
            }

            cr::RelExprKind::BindCorrelated(bound, main) => {
                let mut scope = self.compile_re(*bound);
                scope.extend_lateral(self.compile_re(*main));

                scope
            }

            cr::RelExprKind::Transform(input, transform) => {
                self.compile_rel_transform(*input, transform, &rel.ty)
            }

            cr::RelExprKind::Bind(val, main) => {
                let name = format!("t{}", val.id);

                let val_ty = val.ty.clone();
                let val = self.compile_re(*val);
                let val = self.scope_into_query(val, &val_ty);

                let main_ty = main.ty.clone();
                let main = self.compile_re(*main);
                let mut main = self.scope_into_query(main, &main_ty);

                main.with = Some(utils::with());
                let ctes = &mut main.with.as_mut().unwrap().cte_tables;
                ctes.insert(0, utils::cte(name, val));

                RelScoped::new(utils::sub_rel(main, Some(self.rel_name_gen.next())))
            }
            cr::RelExprKind::From(cr::From::Binding(id)) => {
                let name = format!("t{}", id);

                RelScoped::new(utils::new_table(vec![name], Some(self.rel_name_gen.next())))
            }
        };
        let r_name = r.get_name().to_string();
        self.rvars.insert(rel.id, (r_name, rel.ty.clone()));
        r
    }

    fn compile_rel_transform(
        &mut self,
        input: cr::RelExpr,
        transform: cr::Transform,
        ty: &ir::Ty,
    ) -> RelScoped {
        let input_ty = input.ty.clone();
        let mut scope = self.compile_re(input);

        match transform {
            cr::Transform::Project(columns) => {
                scope = self.wrap_scoped_rel(scope, &input_ty);

                let select = self.rel_as_mut_select(&mut scope, &input_ty);

                let column_exprs = self.compile_columns(columns);
                select.projection = self.projection(ty, column_exprs);

                // scope.rel_vars.extend(rels);
            }
            cr::Transform::ProjectRetain(cols) => {
                let must_wrap = scope.as_sub_rel().is_none_or(|q| q.order_by.is_some());
                if must_wrap {
                    scope = self.wrap_scoped_rel(scope, &input_ty);
                }

                let select = self.rel_as_mut_select(&mut scope, &input_ty);

                utils::retain_by_position(&mut select.projection, cols);

                // apply new column names
                dbg!(&select.projection);
                let old_values = select.projection.drain(..).map(utils::unwrap_select_item);
                dbg!(self.rel_cols(ty, true).collect_vec());
                select.projection = self.projection(ty, old_values);
            }

            cr::Transform::ProjectDiscard(cols) => {
                let must_wrap = scope.as_sub_rel().is_none_or(|q| q.order_by.is_some());
                if must_wrap {
                    scope = self.wrap_scoped_rel(scope, &input_ty);
                }

                let select = self.rel_as_mut_select(&mut scope, &input_ty);

                utils::drop_by_position(&mut select.projection, cols);

                // apply new column names
                let old_values = select.projection.drain(..).map(utils::unwrap_select_item);
                select.projection = self.projection(ty, old_values);
            }

            cr::Transform::Aggregate(columns) => {
                scope = self.wrap_scoped_rel(scope, &input_ty);

                let select = self.rel_as_mut_select(&mut scope, &input_ty);

                let (column_exprs, rels) = self.compile_columns_scoped(columns);
                select.projection = self.projection(ty, column_exprs);

                scope.rel_vars.extend(rels);
            }
            cr::Transform::Where(cond) => {
                scope = self.wrap_scoped_rel(scope, &input_ty);

                let select = self.rel_as_mut_select(&mut scope, &input_ty);

                let cond = self.compile_expr(cond);
                select.selection = Some(cond.expr.into_expr());

                scope.rel_vars.extend(cond.rel_vars);
            }
            cr::Transform::Limit(limit) => {
                let must_wrap = scope
                    .as_sub_rel()
                    .is_none_or(|q| q.limit.is_some() || q.offset.is_some());
                if must_wrap {
                    scope = self.wrap_scoped_rel(scope, &input_ty);
                }

                let query = scope.as_mut_sub_rel().unwrap();
                let limit = self.compile_expr(limit);
                query.limit = Some(limit.expr.into_expr());
                if query.order_by.is_none() {
                    query.order_by = Some(utils::order_by_one(utils::ident(
                        None::<&str>,
                        COL_ARRAY_INDEX,
                    )));
                }

                scope.rel_vars.extend(limit.rel_vars);
            }
            cr::Transform::Offset(offset) => {
                let must_wrap = scope
                    .as_sub_rel()
                    .is_none_or(|q| q.limit.is_some() || q.offset.is_some());
                if must_wrap {
                    scope = self.wrap_scoped_rel(scope, &input_ty);
                }

                let query = scope.as_mut_sub_rel().unwrap();
                let offset = self.compile_expr(offset);
                query.offset = Some(sql_ast::Offset {
                    value: offset.expr.into_expr(),
                    rows: sql_ast::OffsetRows::None,
                });
                if query.order_by.is_none() {
                    query.order_by = Some(utils::order_by_one(utils::ident(
                        None::<&str>,
                        COL_ARRAY_INDEX,
                    )));
                }

                scope.rel_vars.extend(offset.rel_vars);
            }
            cr::Transform::OrderBy(key) => {
                // wrap into a new query
                scope = self.wrap_scoped_rel(scope, &input_ty);

                // overwrite array index
                let select = self.rel_as_mut_select(&mut scope, &input_ty);
                let key = self.compile_expr(key);
                select.projection[0] = sql_ast::SelectItem::ExprWithAlias {
                    expr: key.expr.into_expr(),
                    alias: sql_ast::Ident::new(COL_ARRAY_INDEX),
                };

                scope.rel_vars.extend(key.rel_vars);
            }
            cr::Transform::JsonUnpack(col) => scope = self.compile_json_unpack(scope, *col),
        }
        scope
    }

    fn compile_rel_constructed(&mut self, rows: Vec<Vec<cr::ColExpr>>, ty: &ir::Ty) -> RelScoped {
        let mut res = None;
        let mut rows = rows.into_iter();

        // first row with aliases
        if let Some(row) = rows.next() {
            let mut select = utils::select_empty();
            let columns = self.compile_columns(row);
            select.projection = self.projection(ty, columns);
            res = Some(sql_ast::SetExpr::Select(Box::new(select)));
        }

        // all following rows
        for row in rows {
            let mut select = utils::select_empty();

            let columns = self.compile_columns(row);
            select.projection = columns
                .into_iter()
                .map(sql_ast::SelectItem::UnnamedExpr)
                .collect();

            res = Some(utils::union(
                res.unwrap(),
                sql_ast::SetExpr::Select(Box::new(select)),
            ))
        }

        let query = utils::query_new(res.unwrap_or_else(|| {
            // construct a rel without rows
            let mut values = Vec::new();

            if ty.kind.is_array() {
                values.push(utils::number("0"));
            }
            let item_ty = match &ty.kind {
                ir::TyKind::Primitive(_) | ir::TyKind::Tuple(_) => ty,
                ir::TyKind::Array(item_ty) => item_ty,
                ir::TyKind::Enum(_) | ir::TyKind::Function(_) | ir::TyKind::Ident(_) => todo!(),
            };
            match &self.get_ty_mat(item_ty).kind {
                ir::TyKind::Primitive(_) => {
                    values.push(self.null(item_ty));
                }
                ir::TyKind::Tuple(ty_fields) => {
                    values.extend(ty_fields.iter().map(|ty_field| self.null(&ty_field.ty)));
                }
                _ => todo!(),
            }

            let mut select = utils::select_empty();
            select.projection = self.projection(ty, values);
            select.selection = Some(utils::bool(false));
            sql_ast::SetExpr::Select(Box::new(select))
        }));

        let name = self.rel_name_gen.next();
        let rel = utils::sub_rel(query, Some(name.clone()));
        RelScoped::new(rel)
    }

    fn compile_columns_scoped(
        &mut self,
        columns: Vec<cr::ColExpr>,
    ) -> (Vec<sql_ast::Expr>, Vec<sql_ast::TableFactor>) {
        let mut column_exprs = Vec::with_capacity(columns.len());
        let mut rels = Vec::new();
        for col in columns {
            let expr = self.compile_expr(col);
            column_exprs.push(expr.expr.into_expr());
            rels.extend(expr.rel_vars);
        }
        (column_exprs, rels)
    }

    fn compile_columns(&mut self, columns: Vec<cr::ColExpr>) -> Vec<sql_ast::Expr> {
        let mut column_exprs = Vec::with_capacity(columns.len());
        for col in columns {
            column_exprs.push(self.compile_expr(col).into_subquery());
        }
        column_exprs
    }

    fn compile_expr(&mut self, expr: cr::ColExpr) -> ExprScoped {
        ExprScoped::new(match expr.kind {
            cr::ColExprKind::Null => ExprOrSource::Expr(self.null(&expr.ty)),
            cr::ColExprKind::Literal(literal) => self.compile_literal(literal, &expr.ty),
            cr::ColExprKind::FuncCall(func_name, args) => {
                let mut rels = Vec::new();
                let args: Vec<_> = args
                    .into_iter()
                    .map(|e| {
                        let expr = self.compile_expr(e);
                        rels.extend(expr.rel_vars);
                        expr.expr
                    })
                    .collect();
                let mut expr = ExprScoped::new(self.compile_func_call(&func_name, expr.ty, args));
                expr.rel_vars.extend(rels);
                return expr;
            }
            cr::ColExprKind::InputRelCol(scope_id, col_position) => {
                let (scope_name, scope_ty) = self.find_rvar(scope_id);
                let col = self.rel_cols(scope_ty, true).nth(col_position).unwrap();
                ExprOrSource::Expr(utils::ident(Some(scope_name), col))
            }
            cr::ColExprKind::Subquery(input) => {
                // compile subquery
                let scope = self.compile_re(*input);

                // pack to JSON if needed
                if expr.ty.kind.is_array() {
                    let mut scoped =
                        ExprScoped::new(self.compile_json_pack(&scope.rel_name, &expr.ty));
                    scoped.requires(scope);
                    return scoped;
                } else if scope.rel_vars.is_empty() {
                    let col = self.rel_cols(&expr.ty, true).next().unwrap();
                    ExprOrSource::Expr(utils::ident(Some(scope.rel_name), col))
                } else {
                    let query = self.scope_into_query(scope, &expr.ty);
                    ExprOrSource::Expr(sql_ast::Expr::Subquery(Box::new(query)))
                }
            }

            cr::ColExprKind::Param(param_index) => ExprOrSource::Source(format!(
                "${}::{}",
                param_index + 1,
                self.compile_ty_name(&expr.ty)
            )),
        })
    }

    fn compile_json_pack(&mut self, input: &str, ty: &ir::Ty) -> ExprOrSource {
        let cols = self.rel_cols(ty, false);

        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Array(ty_item) => {
                let index = format!("{input}.{COL_ARRAY_INDEX}");

                match &self.get_ty_mat(ty_item).kind {
                    ir::TyKind::Primitive(_) => ExprOrSource::Source(format!(
                        "COALESCE(jsonb_agg({input}.{COL_VALUE} ORDER BY {index}), '[]'::jsonb)",
                    )),
                    ir::TyKind::Tuple(_) => {
                        let fields = cols.map(|c| format!("{input}.{c}")).join(", ");
                        ExprOrSource::Source(format!(
                            "COALESCE(jsonb_agg(jsonb_build_array({fields}) ORDER BY {index}), '[]'::jsonb)"
                        ))
                    }
                    ir::TyKind::Array(_) => todo!(),
                    _ => todo!(),
                }
            }
            ir::TyKind::Tuple(_) => {
                let fields = cols.map(|c| format!("{input}.{c}")).join(", ");

                ExprOrSource::Source(format!("jsonb_build_array({fields})"))
            }
            _ => unreachable!("{:?}", ty),
        }
    }

    fn compile_json_unpack(&mut self, mut scope: RelScoped, col: cr::ColExpr) -> RelScoped {
        let col_ty = col.ty.clone();

        match &self.get_ty_mat(&col_ty).kind {
            ir::TyKind::Array(ty_item) => {
                let mut query = utils::select_empty();

                /*
                FROM json_array_elements(...col...) j
                SELECT ROW_NUMBER(), j.value
                */

                let col = scope.put_expr(self.compile_expr(col));

                query.from.push(utils::from(utils::lateral(utils::rel_func(
                    sql_ast::Ident::new("jsonb_array_elements"),
                    vec![col.into_expr()],
                    Some("j".into()),
                ))));

                query.projection = vec![sql_ast::SelectItem::ExprWithAlias {
                    expr: ExprOrSource::Source("(ROW_NUMBER() OVER ())::int4".into()).into_expr(),
                    alias: sql_ast::Ident::new("index"),
                }];

                let item_ty = self.get_ty_mat(ty_item);
                match &item_ty.kind {
                    ir::TyKind::Primitive(_) => {
                        let value =
                            ExprOrSource::Source(self.compile_json_item_cast("j.value", item_ty));
                        query.projection.push(sql_ast::SelectItem::ExprWithAlias {
                            expr: value.into_expr(),
                            alias: sql_ast::Ident::new(COL_VALUE),
                        });
                    }
                    ir::TyKind::Array(_) => {
                        query.projection.push(sql_ast::SelectItem::ExprWithAlias {
                            expr: utils::ident(None::<&str>, "j.value"),
                            alias: sql_ast::Ident::new(COL_VALUE),
                        });
                    }
                    ir::TyKind::Tuple(fields) => {
                        for (position, field) in fields.iter().enumerate() {
                            let value = ExprOrSource::Source(self.compile_json_item_cast(
                                &format!("(j.value->{position})"),
                                self.get_ty_mat(&field.ty),
                            ))
                            .into_expr();
                            query.projection.push(sql_ast::SelectItem::ExprWithAlias {
                                expr: value,
                                alias: sql_ast::Ident::new(format!("_{position}")),
                            });
                        }
                    }

                    _ => todo!(),
                }
                let factor =
                    utils::sub_rel(utils::query_select(query), Some(self.rel_name_gen.next()));
                scope.push(utils::lateral(factor));
            }
            ir::TyKind::Tuple(_) => todo!(),
            _ => unreachable!("{:?}", col_ty),
        }
        scope
    }

    fn compile_json_item_cast(&self, item_ref: &str, ty: &ir::Ty) -> String {
        if let ir::TyKind::Primitive(ir::TyPrimitive::text) = &ty.kind {
            // we could use `ref #>> '{}'` here instead, but formatter breaks for the operator
            // for now, let's use this much more verbose way
            format!("jsonb_build_array({item_ref}) ->> 0")
        } else {
            format!("{item_ref}::text::{}", self.compile_ty_name(ty))
        }
    }

    fn compile_func_call(
        &self,
        id: &str,
        ty: ir::Ty,
        args: impl IntoIterator<Item = ExprOrSource>,
    ) -> ExprOrSource {
        match id {
            "std::mul" => utils::new_bin_op("*", args),
            "std::div" => utils::new_bin_op("/", args),
            "std::mod" => match ty.kind.as_primitive().unwrap() {
                ir::TyPrimitive::float32 | ir::TyPrimitive::float64 => {
                    let mut args = args.into_iter();
                    ExprOrSource::Source(format!(
                        "MOD({}::numeric, {}::numeric)::float8",
                        args.next().unwrap(),
                        args.next().unwrap(),
                    ))
                }
                _ => utils::new_bin_op("%", args),
            },
            "std::add" => utils::new_bin_op("+", args),
            "std::sub" => utils::new_bin_op("-", args),
            "std::neg" => utils::new_un_op("-", args),

            "std::eq" => utils::new_bin_op("=", args),
            "std::ne" => utils::new_bin_op("<>", args),

            "std::gt" => utils::new_bin_op(">", args),
            "std::lt" => utils::new_bin_op("<", args),
            "std::gte" => utils::new_bin_op(">=", args),
            "std::lte" => utils::new_bin_op("<=", args),

            "std::and" => utils::new_bin_op("AND", args),
            "std::or" => utils::new_bin_op("OR", args),
            "std::not" => utils::new_un_op("NOT", args),

            "std::text_ops::length" => {
                let mut args = args.into_iter();
                let text = args.next().unwrap();
                ExprOrSource::Source(format!("LENGTH({text})::int8"))
            }

            "std::min" => utils::new_func_call("MIN", args),
            "std::max" => utils::new_func_call("MAX", args),
            "std::sum" => {
                let arg = args.into_iter().next().unwrap();
                let ty = self.compile_ty_name(&ty);
                ExprOrSource::Source(format!("COALESCE(SUM({arg}), 0)::{ty}"))
            }
            "std::average" => {
                let arg = args.into_iter().next().unwrap();
                let ty = self.compile_ty_name(&ty);
                ExprOrSource::Source(format!("AVG({arg})::{ty}"))
            }
            "std::count" => ExprOrSource::Source("COUNT(*)".into()),
            "std::any" => ExprOrSource::Source(format!(
                "COALESCE({}, FALSE)",
                utils::new_func_call("BOOL_OR", args)
            )),
            "std::all" => ExprOrSource::Source(format!(
                "COALESCE({}, TRUE)",
                utils::new_func_call("BOOL_AND", args)
            )),
            "std::contains" => {
                let mut args = args.into_iter();
                let haystack = args.next().unwrap();
                let needle = args.next().unwrap();
                ExprOrSource::Source(format!("COALESCE(BOOL_OR({needle} = {haystack}), FALSE)"))
            }

            "std::row_number" => ExprOrSource::Source("(ROW_NUMBER() OVER () - 1)".to_string()),
            "std::lead" => {
                let mut args = args.into_iter();
                let arg = args.next().unwrap();
                let offset = args.next().unwrap();

                let filler = get_lead_lag_filler(&ty);
                ExprOrSource::Source(format!(
                    "COALESCE(LEAD({arg}, {offset}::int4) OVER (ORDER BY index), {filler})"
                ))
            }
            "std::lag" => {
                let mut args = args.into_iter();
                let arg = args.next().unwrap();
                let offset = args.next().unwrap();

                let filler = get_lead_lag_filler(&ty);

                ExprOrSource::Source(format!(
                    "COALESCE(LAG({arg}, {offset}::int4) OVER (ORDER BY index), {filler})"
                ))
            }

            "std::greatest" => {
                let mut args = args.into_iter();
                ExprOrSource::Source(format!(
                    "GREATEST({}, {})::int8",
                    args.next().unwrap(),
                    args.next().unwrap(),
                ))
            }

            _ => todo!("sql impl for {id}"),
        }
    }

    fn null(&mut self, ty: &ir::Ty) -> sql_ast::Expr {
        sql_ast::Expr::Cast {
            kind: sql_ast::CastKind::DoubleColon,
            expr: Box::new(utils::value(sql_ast::Value::Null)),
            data_type: self.compile_ty_name(ty),
            format: None,
        }
    }

    fn compile_ty_name(&self, ty: &ir::Ty) -> sql_ast::DataType {
        match self.get_ty_mat(ty).kind {
            ir::TyKind::Primitive(prim) => {
                let name = match prim {
                    ir::TyPrimitive::bool => "bool",
                    ir::TyPrimitive::int8 => "\"char\"",
                    ir::TyPrimitive::int16 => "int2",
                    ir::TyPrimitive::int32 => "int4",
                    ir::TyPrimitive::int64 => "int8",
                    ir::TyPrimitive::uint8 => todo!(),
                    ir::TyPrimitive::uint16 => todo!(),
                    ir::TyPrimitive::uint32 => todo!(),
                    ir::TyPrimitive::uint64 => todo!(),
                    ir::TyPrimitive::float32 => "float4",
                    ir::TyPrimitive::float64 => "float8",
                    ir::TyPrimitive::text => "text",
                };
                sql_ast::DataType::Custom(
                    sql_ast::ObjectName(vec![sql_ast::Ident::new(name)]),
                    vec![],
                )
            }
            ir::TyKind::Tuple(ref fields) if fields.is_empty() => {
                // unit type (holds no data) does not exist in sql so we use a type with
                // the least amount of data
                sql_ast::DataType::Boolean
            }
            ir::TyKind::Tuple(_) => todo!(),
            ir::TyKind::Array(_) => todo!(),
            ir::TyKind::Enum(_) => todo!(),
            ir::TyKind::Function(_) => todo!(),
            ir::TyKind::Ident(_) => todo!(),
        }
    }

    fn compile_literal(&self, lit: ir::Literal, ty: &ir::Ty) -> ExprOrSource {
        match lit {
            ir::Literal::Bool(b) if b => ExprOrSource::Source("TRUE".into()),
            ir::Literal::Bool(_) => ExprOrSource::Source("FALSE".into()),

            ir::Literal::Int(i) => {
                ExprOrSource::Source(format!("{i}::{}", self.compile_ty_name(ty)))
            }
            ir::Literal::Float(f) => {
                ExprOrSource::Source(format!("{f}::{}", self.compile_ty_name(ty)))
            }

            ir::Literal::Text(s) => {
                ExprOrSource::Expr(sql_ast::Expr::Value(sql_ast::Value::SingleQuotedString(s)))
            }
        }
    }
}

fn get_lead_lag_filler(ty: &ir::Ty) -> &str {
    let item_ty = ty.kind.as_array().unwrap();
    match &item_ty.kind {
        ir::TyKind::Primitive(
            ir::TyPrimitive::int8
            | ir::TyPrimitive::int16
            | ir::TyPrimitive::int32
            | ir::TyPrimitive::int64,
        ) => "0",
        ir::TyKind::Primitive(ir::TyPrimitive::bool) => "FALSE",
        ir::TyKind::Primitive(ir::TyPrimitive::text) => "''",
        _ => todo!(),
    }
}
