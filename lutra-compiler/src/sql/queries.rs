use itertools::Itertools;
use std::collections::HashMap;

use lutra_bin::ir;
use sqlparser::ast as sql_ast;

use crate::sql::utils::{ExprOrSource, Scoped};
use crate::sql::{cr, utils};
use crate::sql::{COL_ARRAY_INDEX, COL_VALUE};
use crate::utils::NameGenerator;

pub fn compile(rel: cr::RelExpr, types: HashMap<&ir::Path, &ir::Ty>) -> sql_ast::Query {
    let mut ctx = Context::new(types);

    let rel_ty = rel.ty.clone();
    let query = ctx.compile_rel(&rel);

    if rel_ty.kind.is_array() {
        // wrap into a top-level projection to remove array indexes
        let mut query = ctx.scoped_into_query_ext(query, &rel_ty, false);
        query.order_by = Some(utils::order_by_one(utils::ident(
            None::<&str>,
            COL_ARRAY_INDEX,
        )));
        query
    } else {
        ctx.scoped_into_query(query, &rel_ty)
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

    fn register_rvar(&mut self, input: &cr::RelExpr, output: &Scoped) {
        let name = output.expr.as_rel_var().unwrap();
        self.rvars
            .insert(input.id, (name.to_string(), input.ty.clone()));
    }

    fn unregister_rvar(&mut self, input: &cr::RelExpr) {
        self.rvars.remove(&input.id);
    }

    fn compile_rel(&mut self, rel: &cr::RelExpr) -> Scoped {
        match &rel.kind {
            cr::RelExprKind::From(cr::From::Construction(rows)) => {
                self.compile_rel_constructed(rows, &rel.ty)
            }

            cr::RelExprKind::From(cr::From::Table(table_name)) => {
                let mut select = utils::select_empty();

                select.from.push(utils::from(utils::new_table(
                    vec![table_name.clone()],
                    None,
                )));

                let ty_tuple = self.get_ty_mat(&rel.ty).kind.as_array().unwrap();
                let ty_fields = self.get_ty_mat(ty_tuple).kind.as_tuple().unwrap();

                let values = Some(utils::value(sql_ast::Value::Null)) // index
                    .into_iter()
                    .chain(
                        ty_fields
                            .iter()
                            .map(|field| utils::ident(None::<&str>, field.name.as_ref().unwrap())),
                    )
                    .map(ExprOrSource::Expr);

                select.projection = self.projection(&rel.ty, values);

                self.query_into_scoped(utils::query_select(select))
            }

            cr::RelExprKind::From(cr::From::Iterator(scope_id)) => {
                let (rel_name, _) = self.find_rvar(*scope_id);

                Scoped {
                    expr: ExprOrSource::RelVar(rel_name.clone()),
                    rel_vars: Vec::new(),
                }
            }

            cr::RelExprKind::Join(left_in, right_in, condition) => {
                let left_ty = left_in.ty.clone();
                let left = self.compile_rel(left_in);
                let left_name = left.expr.as_rel_var().unwrap().to_string();

                let right_ty = right_in.ty.clone();
                let right = self.compile_rel(right_in);
                let right_name = right.expr.as_rel_var().unwrap().to_string();

                self.register_rvar(left_in, &left);
                self.register_rvar(right_in, &right);

                let mut scope = left;
                scope.expr = right.expr;
                scope.rel_vars.extend(right.rel_vars);

                let mut scope = self.wrap_scoped(scope, &rel.ty);
                let select = self.scoped_as_mut_select(&mut scope, &rel.ty);
                select.projection.clear();

                select.projection.extend(
                    self.projection(
                        &rel.ty,
                        Iterator::chain(
                            self.rel_cols(&left_ty, true)
                                .map(|col| utils::ident(Some(&left_name), col)),
                            self.rel_cols(&right_ty, true)
                                .map(|col| utils::ident(Some(&right_name), col)),
                        )
                        .map(ExprOrSource::Expr),
                    ),
                );

                if let Some(condition) = condition {
                    let expr = self.compile_expr(condition);
                    select.selection = Some(expr.expr.into_expr());
                    scope.rel_vars.extend(expr.rel_vars);
                }

                self.unregister_rvar(left_in);
                self.unregister_rvar(right_in);

                scope
            }

            cr::RelExprKind::BindCorrelated(bound_in, main_in) => {
                // compile inner-to-outer
                let bound = self.compile_rel(bound_in);

                self.register_rvar(bound_in, &bound);
                let main = self.compile_rel(main_in);
                self.unregister_rvar(bound_in);

                Scoped {
                    expr: main.expr,
                    rel_vars: std::iter::Iterator::chain(
                        bound.rel_vars.into_iter(),
                        main.rel_vars.into_iter().map(utils::lateral),
                    )
                    .collect(),
                }
            }

            cr::RelExprKind::Transform(input, transform) => {
                let input_sql = self.compile_rel(input);

                self.register_rvar(input, &input_sql);
                let r = self.compile_rel_transform(input_sql, transform, &input.ty, &rel.ty);
                self.unregister_rvar(input);

                r
            }

            cr::RelExprKind::Bind(val_in, main_in) => {
                let name = format!("t{}", val_in.id);

                let val_ty = val_in.ty.clone();
                let val = self.compile_rel(val_in);

                self.register_rvar(val_in, &val);
                let main_ty = main_in.ty.clone();
                let main = self.compile_rel(main_in);
                self.unregister_rvar(val_in);

                let val = self.scoped_into_query(val, &val_ty);
                let mut main = self.scoped_into_query(main, &main_ty);

                main.with = Some(utils::with());
                let ctes = &mut main.with.as_mut().unwrap().cte_tables;
                ctes.insert(0, utils::cte(name, val));

                self.query_into_scoped(main)
            }
            cr::RelExprKind::From(cr::From::Binding(id)) => {
                let name = format!("t{}", id);

                let alias = self.rel_name_gen.next();
                let rel_var = utils::new_table(vec![name], Some(alias.clone()));
                Scoped::new(alias, vec![rel_var])
            }
        }
    }

    fn compile_rel_transform(
        &mut self,
        mut scoped: Scoped,
        transform: &cr::Transform,
        input_ty: &ir::Ty,
        ty: &ir::Ty,
    ) -> Scoped {
        match transform {
            cr::Transform::Project(columns) => {
                scoped = self.wrap_scoped(scoped, input_ty);

                let select = self.scoped_as_mut_select(&mut scoped, input_ty);

                let column_exprs = self.compile_columns(columns);
                select.projection = self.projection(ty, column_exprs);
            }
            cr::Transform::ProjectRetain(cols) => {
                let must_wrap = scoped.as_sub_rel().is_none_or(|q| q.order_by.is_some());
                if must_wrap {
                    scoped = self.wrap_scoped(scoped, input_ty);
                }

                let select = self.scoped_as_mut_select(&mut scoped, input_ty);

                utils::retain_by_position(&mut select.projection, cols);

                // apply new column names
                let old_values = select
                    .projection
                    .drain(..)
                    .map(utils::unwrap_select_item)
                    .map(ExprOrSource::Expr);
                select.projection = self.projection(ty, old_values);
            }

            cr::Transform::ProjectDiscard(cols) => {
                let must_wrap = scoped.as_sub_rel().is_none_or(|q| q.order_by.is_some());
                if must_wrap {
                    scoped = self.wrap_scoped(scoped, input_ty);
                }

                let select = self.scoped_as_mut_select(&mut scoped, input_ty);

                utils::drop_by_position(&mut select.projection, cols);

                // apply new column names
                let old_values = select
                    .projection
                    .drain(..)
                    .map(utils::unwrap_select_item)
                    .map(ExprOrSource::Expr);
                select.projection = self.projection(ty, old_values);
            }

            cr::Transform::Aggregate(columns) => {
                scoped = self.wrap_scoped(scoped, input_ty);

                let select = self.scoped_as_mut_select(&mut scoped, input_ty);

                let (column_exprs, rels) = self.compile_columns_scoped(columns);
                select.projection = self.projection(ty, column_exprs);
                select
                    .from
                    .extend(rels.into_iter().map(utils::lateral).map(utils::from));
            }
            cr::Transform::Where(cond) => {
                scoped = self.wrap_scoped(scoped, input_ty);

                let select = self.scoped_as_mut_select(&mut scoped, input_ty);

                let cond = self.compile_expr(cond);
                select.selection = Some(cond.expr.into_expr());

                scoped.rel_vars.extend(cond.rel_vars);
            }
            cr::Transform::Limit(limit) => {
                let must_wrap = scoped
                    .as_sub_rel()
                    .is_none_or(|q| q.limit.is_some() || q.offset.is_some());
                if must_wrap {
                    scoped = self.wrap_scoped(scoped, input_ty);
                }

                let query = scoped.as_mut_sub_rel().unwrap();
                let limit = self.compile_expr(limit);
                query.limit = Some(limit.expr.into_expr());
                if query.order_by.is_none() {
                    query.order_by = Some(utils::order_by_one(utils::ident(
                        None::<&str>,
                        COL_ARRAY_INDEX,
                    )));
                }

                scoped.rel_vars.extend(limit.rel_vars);
            }
            cr::Transform::Offset(offset) => {
                let must_wrap = scoped
                    .as_sub_rel()
                    .is_none_or(|q| q.limit.is_some() || q.offset.is_some());
                if must_wrap {
                    scoped = self.wrap_scoped(scoped, input_ty);
                }

                let query = scoped.as_mut_sub_rel().unwrap();
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

                scoped.rel_vars.extend(offset.rel_vars);
            }
            cr::Transform::OrderBy(key) => {
                // wrap into a new query
                scoped = self.wrap_scoped(scoped, input_ty);

                // overwrite array index
                let select = self.scoped_as_mut_select(&mut scoped, input_ty);
                let key = self.compile_expr(key);
                select.projection[0] = sql_ast::SelectItem::ExprWithAlias {
                    expr: key.expr.into_expr(),
                    alias: sql_ast::Ident::new(COL_ARRAY_INDEX),
                };

                scoped.rel_vars.extend(key.rel_vars);
            }
            cr::Transform::JsonUnpack => scoped = self.compile_json_unpack(scoped, ty),
        }
        scoped
    }

    fn compile_rel_constructed(&mut self, rows: &[Vec<cr::ColExpr>], ty: &ir::Ty) -> Scoped {
        let mut res = None;
        let mut rows = rows.iter();

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
                .map(ExprOrSource::into_expr)
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
            select.projection = self.projection(ty, values.into_iter().map(ExprOrSource::Expr));
            select.selection = Some(utils::bool(false));
            sql_ast::SetExpr::Select(Box::new(select))
        }));

        let name = self.rel_name_gen.next();
        let rel = utils::sub_rel(query, name.clone());
        Scoped::new(name, vec![rel])
    }

    fn compile_columns_scoped(
        &mut self,
        columns: &[cr::ColExpr],
    ) -> (Vec<ExprOrSource>, Vec<sql_ast::TableFactor>) {
        let mut exprs = Vec::with_capacity(columns.len());
        let mut rel_vars = Vec::new();
        for col in columns {
            let expr = self.compile_expr(col);
            exprs.push(expr.expr);
            rel_vars.extend(expr.rel_vars);
        }
        (exprs, rel_vars)
    }

    fn compile_columns(&mut self, columns: &[cr::ColExpr]) -> Vec<ExprOrSource> {
        let mut column_exprs = Vec::with_capacity(columns.len());
        for col in columns {
            let e = self.compile_expr(col);
            column_exprs.push(self.scoped_into_expr(e, &col.ty));
        }
        column_exprs
    }

    fn compile_expr(&mut self, expr: &cr::ColExpr) -> Scoped {
        match &expr.kind {
            cr::ColExprKind::Null => ExprOrSource::Expr(self.null(&expr.ty)).into(),
            cr::ColExprKind::Literal(literal) => self.compile_literal(literal, &expr.ty).into(),
            cr::ColExprKind::FuncCall(func_name, args) => {
                let (args, rel_vars) = self.compile_columns_scoped(args);
                Scoped {
                    expr: self.compile_func_call(func_name, expr.ty.clone(), args),
                    rel_vars,
                }
            }
            cr::ColExprKind::InputRelCol(scope_id, col_position) => {
                let (scope_name, scope_ty) = self.find_rvar(*scope_id);
                let col = self.rel_cols(scope_ty, true).nth(*col_position).unwrap();
                Scoped::from(ExprOrSource::Expr(utils::ident(Some(scope_name), col)))
            }
            cr::ColExprKind::Subquery(input) => {
                // compile subquery
                let rel = self.compile_rel(input);

                // pack to JSON if needed
                if expr.ty.kind.is_array() {
                    Scoped {
                        expr: self.compile_json_pack(rel.expr.as_rel_var().unwrap(), &expr.ty),
                        rel_vars: rel.rel_vars,
                    }
                } else if rel.rel_vars.is_empty() {
                    let col = self.rel_cols(&expr.ty, true).next().unwrap();
                    Scoped::from(ExprOrSource::Expr(utils::ident(
                        Some(rel.expr.as_rel_var().unwrap()),
                        col,
                    )))
                } else {
                    let query = self.scoped_into_query(rel, &expr.ty);
                    Scoped::from(ExprOrSource::Expr(sql_ast::Expr::Subquery(Box::new(query))))
                }
            }

            cr::ColExprKind::Param(param_index) => Scoped::from(ExprOrSource::Source(format!(
                "${}::{}",
                param_index + 1,
                self.compile_ty_name(&expr.ty)
            ))),
        }
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

    fn compile_json_unpack(&mut self, mut scoped: Scoped, ty: &ir::Ty) -> Scoped {
        if let Some(simplified) = scoped.as_simplified() {
            dbg!(scoped);
            dbg!(&simplified);
            scoped = simplified;
        }

        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Array(ty_item) => {
                let mut query = utils::select_empty();

                /*
                FROM json_array_elements(...expr...) j
                SELECT ROW_NUMBER(), j.value
                */

                query.from.push(utils::from(utils::rel_func(
                    sql_ast::Ident::new("jsonb_array_elements"),
                    vec![scoped.expr.into_expr()],
                    Some("j".into()),
                )));

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
                let rel_var_name = self.rel_name_gen.next();
                let rel_var = utils::sub_rel(utils::query_select(query), rel_var_name.clone());
                scoped.rel_vars.push(utils::lateral(rel_var));
                scoped.expr = ExprOrSource::RelVar(rel_var_name);
            }
            ir::TyKind::Tuple(_) => todo!(),
            _ => unreachable!("{:?}", ty),
        }
        scoped
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

    fn compile_literal(&self, lit: &ir::Literal, ty: &ir::Ty) -> ExprOrSource {
        match lit {
            ir::Literal::Bool(b) if *b => ExprOrSource::Source("TRUE".into()),
            ir::Literal::Bool(_) => ExprOrSource::Source("FALSE".into()),

            ir::Literal::Int(i) => {
                ExprOrSource::Source(format!("{i}::{}", self.compile_ty_name(ty)))
            }
            ir::Literal::Float(f) => {
                ExprOrSource::Source(format!("{f}::{}", self.compile_ty_name(ty)))
            }

            ir::Literal::Text(s) => ExprOrSource::Expr(sql_ast::Expr::Value(
                sql_ast::Value::SingleQuotedString(s.clone()),
            )),
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
