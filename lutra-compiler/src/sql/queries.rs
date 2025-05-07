use std::collections::HashMap;

use super::utils::ExprOrSource;
use super::COL_ARRAY_INDEX;
use super::{cr, utils};
use lutra_bin::ir;
use sqlparser::ast as sql_ast;

pub fn compile(rel: cr::RelExpr, types: HashMap<&ir::Path, &ir::Ty>) -> sql_ast::Query {
    let mut ctx = Context::new(types);

    let rel_ty = rel.ty.clone();
    let query = ctx.compile_re(rel);

    if rel_ty.kind.is_array() {
        // wrap into a top-level projection to remove array indexes
        let mut query = ctx.query_wrap(utils::sub_rel(query, None), &rel_ty, false);
        query.order_by = Some(utils::order_by_one(utils::ident(
            None::<&str>,
            COL_ARRAY_INDEX,
        )));
        query
    } else {
        query
    }
}

pub(super) struct Context<'a> {
    types: HashMap<&'a ir::Path, &'a ir::Ty>,

    scope_stack: Vec<(usize, String, ir::Ty)>,
}

impl<'a> Context<'a> {
    pub(super) fn new(types: HashMap<&'a ir::Path, &'a ir::Ty>) -> Self {
        Self {
            types,
            scope_stack: Vec::new(),
        }
    }

    pub(super) fn get_ty_mat(&self, ty: &'a ir::Ty) -> &'a ir::Ty {
        match &ty.kind {
            ir::TyKind::Ident(path) => self.types.get(path).unwrap(),
            _ => ty,
        }
    }

    fn find_scope_name(&self, scope_id: usize) -> &str {
        (self.scope_stack.iter().rev())
            .find(|(id, _, _)| *id == scope_id)
            .map(|(_, name, _)| name)
            .unwrap()
    }

    fn find_scope(&self, scope_id: usize) -> (&str, &ir::Ty) {
        (self.scope_stack.iter().rev())
            .find(|(id, _, _)| *id == scope_id)
            .map(|(_, name, ty)| (name.as_str(), ty))
            .unwrap()
    }

    fn inherit_scope_iterator(&mut self, input_name: &mut Option<String>) -> bool {
        if self.scope_stack.len() < 2 {
            return false;
        }
        input_name.take();

        let (_, prev_name, _) = self.scope_stack.get(self.scope_stack.len() - 2).unwrap();
        let prev_name = prev_name.clone();

        let (_, last_name, _) = self.scope_stack.last_mut().unwrap();
        *last_name = prev_name;

        true
    }

    fn compile_re(&mut self, rel: cr::RelExpr) -> sql_ast::Query {
        match rel.kind {
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

                utils::query_select(select)
            }
            cr::RelExprKind::From(cr::From::Binding(name)) => {
                let mut select = utils::select_empty();

                select
                    .from
                    .push(utils::from(utils::new_table(vec![name], None)));
                select.projection = self.projection_noop(None, &rel.ty, true);
                utils::query_select(select)
            }
            cr::RelExprKind::Join(left, right) => {
                let mut select = utils::select_empty();

                let left_ty = left.ty.clone();
                let left = self.compile_re(*left);

                select
                    .from
                    .push(utils::from(utils::sub_rel(left, Some("l".into()))));

                let right_ty = right.ty.clone();
                let right = self.compile_re(*right);
                select
                    .from
                    .push(utils::from(utils::sub_rel(right, Some("r".into()))));

                select.projection.extend(
                    self.projection(
                        &rel.ty,
                        Iterator::chain(
                            self.rel_cols(&left_ty, true)
                                .map(|col| utils::ident(Some("l"), col)),
                            self.rel_cols(&right_ty, true)
                                .map(|col| utils::ident(Some("r"), col)),
                        ),
                    ),
                );

                utils::query_select(select)
            }
            cr::RelExprKind::From(cr::From::Iterator(scope_id)) => {
                let mut select = utils::select_empty();
                let rel_name = self.find_scope_name(scope_id).to_string();
                select.projection = self.projection_noop(Some(&rel_name), &rel.ty, true);
                utils::query_select(select)
            }

            cr::RelExprKind::Transform(input, scope_id, transform) => {
                self.compile_rel_transform(*input, scope_id, transform, rel.ty)
            }

            cr::RelExprKind::Bind(name, val, main) => {
                let val = self.compile_re(*val);

                let mut main = self.compile_re(*main);

                if main.with.is_none() {
                    main.with = Some(utils::with());
                }
                let ctes = &mut main.with.as_mut().unwrap().cte_tables;
                ctes.insert(0, utils::cte(name, val));

                main
            }
        }
    }

    fn compile_rel_transform(
        &mut self,
        input: cr::RelExpr,
        scope_id: usize,
        transform: cr::Transform,
        ty: ir::Ty,
    ) -> sql_ast::Query {
        let input_ty = input.ty.clone();
        let mut query = self.compile_re(input);

        let input_name = format!("r{scope_id}");
        self.scope_stack
            .push((scope_id, input_name.clone(), input_ty.clone()));
        let mut input_name = Some(input_name);

        let r = match transform {
            cr::Transform::Project(columns) => {
                let mut select = utils::select_empty();

                select
                    .from
                    .push(utils::from(utils::sub_rel(query, input_name.take())));

                let column_exprs = self.compile_columns(columns);
                select.projection = self.projection(&ty, column_exprs);

                utils::query_select(select)
            }
            cr::Transform::ProjectRetain(cols) => {
                input_name.take();

                if query.order_by.is_some() {
                    query = self.query_wrap(utils::sub_rel(query, None), &input_ty, true);
                }
                let select = self.query_as_mut_select(&mut query, &input_ty);

                utils::retain_by_position(&mut select.projection, cols);

                query
            }

            cr::Transform::ProjectDiscard(cols) => {
                input_name.take();

                if query.order_by.is_some() {
                    query = self.query_wrap(utils::sub_rel(query, None), &input_ty, true);
                }
                let select = self.query_as_mut_select(&mut query, &input_ty);

                utils::drop_by_position(&mut select.projection, cols);
                query
            }

            cr::Transform::Aggregate(columns) => {
                let mut select = utils::select_empty();

                select
                    .from
                    .push(utils::from(utils::sub_rel(query, input_name.take())));

                let columns: Vec<_> = columns
                    .into_iter()
                    .map(|x| self.compile_expr(x))
                    .map(|e| e.into_expr())
                    .collect();
                select.projection = self.projection(&ty, columns);

                utils::query_select(select)
            }
            cr::Transform::Where(cond) => {
                let mut query =
                    self.query_wrap(utils::sub_rel(query, input_name.take()), &ty, true);

                let select = self.query_as_mut_select(&mut query, &ty);
                select.selection = Some(self.compile_expr(cond).into_expr());

                query
            }
            cr::Transform::Limit(limit) => {
                if query.limit.is_some()
                    || query.offset.is_some()
                    || !self.inherit_scope_iterator(&mut input_name)
                {
                    query = self.query_wrap(utils::sub_rel(query, input_name.take()), &ty, true);
                }

                query.limit = Some(self.compile_expr(limit).into_expr());
                if query.order_by.is_none() {
                    query.order_by = Some(utils::order_by_one(utils::ident(
                        Some(self.find_scope_name(scope_id)),
                        COL_ARRAY_INDEX,
                    )));
                }
                query
            }
            cr::Transform::Offset(offset) => {
                if query.limit.is_some()
                    || query.offset.is_some()
                    || !self.inherit_scope_iterator(&mut input_name)
                {
                    query = self.query_wrap(utils::sub_rel(query, input_name.take()), &ty, true);
                }

                query.offset = Some(sql_ast::Offset {
                    value: self.compile_expr(offset).into_expr(),
                    rows: sql_ast::OffsetRows::None,
                });
                if query.order_by.is_none() {
                    query.order_by = Some(utils::order_by_one(utils::ident(
                        Some(self.find_scope_name(scope_id)),
                        COL_ARRAY_INDEX,
                    )));
                }

                query
            }
            cr::Transform::OrderBy(key) => {
                // wrap into a new query
                let mut select = utils::select_empty();

                select
                    .from
                    .push(utils::from(utils::sub_rel(query, input_name.take())));
                select.projection = self.projection_noop(None, &ty, true);

                // overwrite array index
                select.projection[0] = sql_ast::SelectItem::ExprWithAlias {
                    expr: self.compile_expr(key).into_expr(),
                    alias: sql_ast::Ident::new(COL_ARRAY_INDEX),
                };

                utils::query_select(select)
            }
            cr::Transform::JsonUnpack(col) => {
                self.compile_json_unpack(query, input_name.take().unwrap(), *col)
            }
        };
        self.scope_stack.pop();

        // assert we have either used the name of the transform input
        // or overwritten it with previous input
        assert!(input_name.is_none(), "input {input_name:?} not used");

        r
    }

    fn compile_rel_constructed(
        &mut self,
        rows: Vec<Vec<cr::ColExpr>>,
        ty: &ir::Ty,
    ) -> sql_ast::Query {
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

        utils::query_new(res.unwrap_or_else(|| {
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
        }))
    }

    fn compile_columns(&mut self, columns: Vec<cr::ColExpr>) -> Vec<sql_ast::Expr> {
        let mut column_exprs = Vec::with_capacity(columns.len());
        for col in columns {
            column_exprs.push(self.compile_expr(col).into_expr());
        }
        column_exprs
    }

    fn compile_expr(&mut self, expr: cr::ColExpr) -> ExprOrSource {
        match expr.kind {
            cr::ColExprKind::Null => ExprOrSource::Expr(self.null(&expr.ty)),
            cr::ColExprKind::Literal(literal) => self.compile_literal(literal, &expr.ty),
            cr::ColExprKind::FuncCall(func_name, args) => {
                let args: Vec<_> = args.into_iter().map(|x| self.compile_expr(x)).collect();
                self.compile_func_call(&func_name, expr.ty, args)
            }
            cr::ColExprKind::InputRelCol(scope_id, col_position) => {
                let (scope_name, scope_ty) = self.find_scope(scope_id);
                let col = self.rel_cols(scope_ty, true).nth(col_position).unwrap();
                ExprOrSource::Expr(utils::ident(Some(scope_name), col))
            }
            cr::ColExprKind::Subquery(input) => {
                // compile subquery
                let query = self.compile_re(*input);
                let mut query = ExprOrSource::Expr(sql_ast::Expr::Subquery(Box::new(query)));

                // pack to JSON if needed
                if expr.ty.kind.is_array() {
                    query = self.compile_json_pack(query, &expr.ty);
                }

                query
            }

            cr::ColExprKind::Param(param_index) => ExprOrSource::Source(format!(
                "${}::{}",
                param_index + 1,
                self.compile_ty_name(&expr.ty)
            )),
        }
    }

    fn compile_json_pack(&mut self, subquery: ExprOrSource, ty: &ir::Ty) -> ExprOrSource {
        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Array(ty_item) => match &self.get_ty_mat(ty_item).kind {
                ir::TyKind::Primitive(_) => ExprOrSource::Source(format!(
                    "(SELECT json_agg(value ORDER BY index) FROM {subquery})",
                )),
                ir::TyKind::Tuple(_) => {
                    let fields: Vec<_> = self.rel_cols(ty, false).collect();
                    let fields = fields.join(", ");

                    let objects = format!(
                        "SELECT index, jsonb_build_array({fields}) as value FROM {subquery}",
                    );

                    ExprOrSource::Source(format!(
                        "(SELECT json_agg(value ORDER BY index) FROM ({objects}))"
                    ))
                }
                ir::TyKind::Array(_) => todo!(),
                _ => todo!(),
            },
            ir::TyKind::Tuple(_) => {
                let fields: Vec<_> = self.rel_cols(ty, false).collect();
                let fields = fields.join(", ");

                ExprOrSource::Source(format!(
                    "(SELECT jsonb_build_array({fields}) as value FROM {subquery})"
                ))
            }
            _ => unreachable!("{:?}", ty),
        }
    }

    fn compile_json_unpack(
        &mut self,
        input: sql_ast::Query,
        input_name: String,
        col: cr::ColExpr,
    ) -> sql_ast::Query {
        let col_ty = col.ty.clone();

        match &self.get_ty_mat(&col_ty).kind {
            ir::TyKind::Array(ty_item) => {
                let mut query = utils::select_empty();

                /*
                FROM json_array_elements((SELECT ...col... FROM (...input...)) j
                SELECT ROW_NUMBER(), j.value
                */

                let subquery = {
                    let mut subquery = utils::select_empty();
                    subquery
                        .from
                        .push(utils::from(utils::sub_rel(input, Some(input_name))));

                    let col_expr = self.compile_expr(col);
                    subquery
                        .projection
                        .push(sql_ast::SelectItem::UnnamedExpr(col_expr.into_expr()));

                    utils::query_select(subquery)
                };

                query.from.push(utils::from(utils::lateral(utils::rel_func(
                    sql_ast::Ident::new("json_array_elements"),
                    vec![sql_ast::Expr::Subquery(Box::new(subquery))],
                    Some("j".into()),
                ))));

                query.projection = vec![sql_ast::SelectItem::ExprWithAlias {
                    expr: ExprOrSource::Source("(ROW_NUMBER() OVER ())::int4".into()).into_expr(),
                    alias: sql_ast::Ident::new("index"),
                }];

                let item_ty = self.get_ty_mat(ty_item);
                match &item_ty.kind {
                    ir::TyKind::Primitive(_) => {
                        let value = ExprOrSource::Source(format!(
                            "j.value::text::{}",
                            self.compile_ty_name(item_ty)
                        ));
                        query
                            .projection
                            .push(sql_ast::SelectItem::UnnamedExpr(value.into_expr()));
                    }
                    ir::TyKind::Array(_) => {
                        query
                            .projection
                            .push(sql_ast::SelectItem::UnnamedExpr(utils::ident(
                                None::<&str>,
                                "j.value",
                            )));
                    }
                    ir::TyKind::Tuple(fields) => {
                        for (index, field) in fields.iter().enumerate() {
                            let value = ExprOrSource::Source(format!(
                                "(j.value->{index})::text::{}",
                                self.compile_ty_name(&field.ty)
                            ))
                            .into_expr();
                            query.projection.push(sql_ast::SelectItem::ExprWithAlias {
                                expr: value,
                                alias: sql_ast::Ident::new(format!("_{index}")),
                            });
                        }
                    }

                    _ => todo!(),
                }
                utils::query_select(query)
            }
            ir::TyKind::Tuple(_) => todo!(),
            _ => unreachable!("{:?}", col_ty),
        }
    }

    fn compile_func_call(
        &mut self,
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
                ExprOrSource::Source(format!(
                    "COALESCE(LEAD({arg}, {offset}::int4) OVER (ORDER BY index), 0)"
                ))
            }
            "std::lag" => {
                let mut args = args.into_iter();
                let arg = args.next().unwrap();
                let offset = args.next().unwrap();
                ExprOrSource::Source(format!(
                    "COALESCE(LAG({arg}, {offset}::int4) OVER (ORDER BY index), 0)"
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
