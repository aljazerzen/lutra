use itertools::Itertools;
use std::collections::HashMap;

use lutra_bin::ir;
use sqlparser::ast as sql_ast;

use crate::sql::utils::{ExprOrSource, RelCols, Scoped};
use crate::sql::{COL_ARRAY_INDEX, COL_VALUE};
use crate::sql::{cr, utils};
use crate::utils::NameGenerator;

pub fn compile(rel: cr::Expr, types: HashMap<&ir::Path, &ir::Ty>) -> sql_ast::Query {
    let mut ctx = Context::new(types);

    let rel_ty = rel.ty.clone();
    let query = ctx.compile_rel(&rel);

    if rel_ty.kind.is_array() {
        // wrap into a top-level projection to remove array indexes
        let rel_name = query.expr.as_rel_var().unwrap().to_string();
        let mut query = ctx.scoped_into_query_ext(query, &rel_ty, false);
        query.order_by = Some(utils::order_by_one(utils::ident(
            Some(rel_name),
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

    rel_vars: HashMap<usize, RelRef>,
}

struct RelRef {
    /// Name that can be used to refer to the relation
    name: String,

    /// True iff reference requires a FROM clause
    is_cte: bool,
}

impl<'a> Context<'a> {
    pub(super) fn new(types: HashMap<&'a ir::Path, &'a ir::Ty>) -> Self {
        Self {
            types,
            rel_name_gen: NameGenerator::new("r"),
            rel_vars: Default::default(),
        }
    }

    pub(super) fn get_ty_mat(&self, ty: &'a ir::Ty) -> &'a ir::Ty {
        match &ty.kind {
            ir::TyKind::Ident(path) => self.types.get(path).unwrap(),
            _ => ty,
        }
    }

    fn find_rel_var(&self, id: usize) -> &RelRef {
        match self.rel_vars.get(&id) {
            Some(x) => x,
            None => panic!("cannot find scope id: {id}"),
        }
    }

    fn register_rel_var(&mut self, input: &cr::BoundExpr, output: &Scoped) {
        let name = output.expr.as_rel_var().unwrap().to_string();
        let r = RelRef {
            name,
            is_cte: false,
        };
        self.rel_vars.insert(input.id, r);
    }

    fn register_cte(&mut self, input: &cr::BoundExpr, name: String) {
        let r = RelRef { name, is_cte: true };
        self.rel_vars.insert(input.id, r);
    }

    fn unregister_rel_var(&mut self, input: &cr::BoundExpr) {
        self.rel_vars.remove(&input.id);
    }

    fn compile_rel(&mut self, rel: &cr::Expr) -> Scoped {
        match &rel.kind {
            cr::ExprKind::From(from) => self.compile_from(from, &rel.ty),

            cr::ExprKind::Join(left_in, right_in, condition) => {
                let left = self.compile_rel(&left_in.rel);
                let left_name = left.expr.as_rel_var().unwrap().to_string();

                let right = self.compile_rel(&right_in.rel);
                let right_name = right.expr.as_rel_var().unwrap().to_string();

                self.register_rel_var(left_in, &left);
                self.register_rel_var(right_in, &right);

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
                            self.rel_cols(&left_in.rel.ty, true)
                                .map(|col| utils::ident(Some(&left_name), col)),
                            self.rel_cols(&right_in.rel.ty, true)
                                .map(|col| utils::ident(Some(&right_name), col)),
                        )
                        .map(ExprOrSource::new_expr),
                    ),
                );

                if let Some(condition) = condition {
                    let expr = self.compile_column(condition);
                    select.selection = Some(expr.expr.into_expr());
                    select.from.extend(
                        expr.rel_vars
                            .into_iter()
                            .map(utils::lateral)
                            .map(utils::from),
                    );
                }

                self.unregister_rel_var(left_in);
                self.unregister_rel_var(right_in);

                scope
            }

            cr::ExprKind::BindCorrelated(bound_in, main_in) => {
                // compile inner-to-outer
                let bound = self.compile_rel(&bound_in.rel);

                self.register_rel_var(bound_in, &bound);
                let main = self.compile_rel(main_in);
                self.unregister_rel_var(bound_in);

                if let Some(row) = main.as_row() {
                    // optimization: if main is a simple projection, place bound into FROM clause and
                    // produce a single query
                    let mut bound = self.wrap_scoped(bound, &bound_in.rel.ty);
                    let select = self.scoped_as_mut_select(&mut bound, &bound_in.rel.ty);
                    select.projection = row.to_vec();
                    bound
                } else {
                    // lateral
                    main.merge_input(bound)
                }
            }

            cr::ExprKind::Transform(input, transform) => {
                let input_sql = self.compile_rel(&input.rel);

                self.register_rel_var(input, &input_sql);
                let r = self.compile_rel_transform(input_sql, transform, &input.rel.ty, &rel.ty);
                self.unregister_rel_var(input);

                r
            }

            cr::ExprKind::Bind(val_in, main_in) => {
                let name = self.rel_name_gen.next();

                let val = self.compile_rel(&val_in.rel);

                self.register_cte(val_in, name.clone());
                let main = self.compile_rel(main_in);
                self.unregister_rel_var(val_in);

                let val = self.scoped_into_query(val, &val_in.rel.ty);
                let mut main = self.scoped_into_query(main, &main_in.ty);

                main.with = Some(utils::with());
                let ctes = &mut main.with.as_mut().unwrap().cte_tables;
                ctes.insert(0, utils::cte(name, val));

                self.query_into_scoped(main)
            }

            cr::ExprKind::Union(parts) => {
                let mut res = None;
                for part_in in parts {
                    let part = self.compile_rel(part_in);

                    let part = if let Some(query) = part.as_query() {
                        query.clone()
                    } else {
                        self.scoped_into_query(part, &part_in.ty)
                    };

                    if let Some(r) = res {
                        res = Some(utils::union(r, utils::query_into_set_expr(part)))
                    } else {
                        res = Some(utils::query_into_set_expr(part))
                    }
                }

                let query =
                    utils::query_new(res.unwrap_or_else(|| self.construct_empty_rel(&rel.ty)));

                let name = self.rel_name_gen.next();
                let rel = utils::sub_rel(query, name.clone());
                Scoped::new(name, vec![rel])
            }
        }
    }

    fn compile_from(&mut self, from: &cr::From, ty: &ir::Ty) -> Scoped {
        match from {
            cr::From::Row(row) => {
                let mut select = utils::select_empty();
                let columns = self.compile_columns(row);
                select.projection = self.projection(ty, columns);

                self.query_into_scoped(utils::query_select(select))
            }

            cr::From::RelRef(scope_id) => {
                let rel_ref = self.find_rel_var(*scope_id);
                let rel_name = rel_ref.name.clone();

                if rel_ref.is_cte {
                    let alias = self.rel_name_gen.next();
                    let rel_var = utils::new_table(vec![rel_name], Some(alias.clone()));
                    Scoped::new(alias, vec![rel_var])
                } else {
                    Scoped {
                        expr: ExprOrSource::RelVar(rel_name),
                        rel_vars: Vec::new(),
                    }
                }
            }

            cr::From::Table(table_name) => {
                let mut select = utils::select_empty();

                select.from.push(utils::from(utils::new_table(
                    vec![table_name.clone()],
                    None,
                )));

                let ty_tuple = self.get_ty_mat(ty).kind.as_array().unwrap();
                let ty_fields = self.get_ty_mat(ty_tuple).kind.as_tuple().unwrap();

                let values = Some(utils::value(sql_ast::Value::Null)) // index
                    .into_iter()
                    .chain(
                        ty_fields
                            .iter()
                            .map(|field| utils::ident(None::<&str>, field.name.as_ref().unwrap())),
                    )
                    .map(ExprOrSource::new_expr);

                select.projection = self.projection(ty, values);

                self.query_into_scoped(utils::query_select(select))
            }

            cr::From::Null => ExprOrSource::new_expr(self.null(ty)).into(),
            cr::From::Literal(literal) => self.compile_literal(literal, ty).into(),
            cr::From::FuncCall(func_name, args) => {
                let (args, rel_vars) = self.compile_columns_scoped(args);
                Scoped {
                    expr: self.compile_func_call(func_name, ty, args),
                    rel_vars,
                }
            }
            cr::From::Param(param_index) => Scoped::from(ExprOrSource::Source(format!(
                "${}::{}",
                param_index + 1,
                self.compile_ty_name(ty)
            ))),

            cr::From::JsonUnpack(input) => {
                let input = self.compile_rel(input);
                self.compile_json_unpack(input, ty)
            }

            cr::From::Case(cases) => {
                let mut conditions = Vec::new();
                let mut results = Vec::new();
                for (cond, value) in cases {
                    let cond_s = self.compile_column(cond);
                    let cond = self.scoped_into_expr(cond_s, &cond.ty);
                    conditions.push(cond.into_expr());

                    let value_s = self.compile_column(value);
                    let value = self.scoped_into_expr(value_s, &value.ty);
                    results.push(value.into_expr());
                }
                Scoped::from(ExprOrSource::Expr(Box::new(sql_ast::Expr::Case {
                    operand: None,
                    conditions,
                    results,
                    else_result: None,
                })))
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
            cr::Transform::ProjectRetain(cols) => {
                let must_wrap = scoped.as_query().is_none_or(|q| q.order_by.is_some());
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
                    .map(ExprOrSource::new_expr);
                select.projection = self.projection(ty, old_values);
            }

            cr::Transform::ProjectDiscard(cols) => {
                let must_wrap = scoped.as_query().is_none_or(|q| q.order_by.is_some());
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
                    .map(ExprOrSource::new_expr);
                select.projection = self.projection(ty, old_values);
            }

            cr::Transform::Aggregate(columns) => {
                scoped = self.wrap_scoped(scoped, input_ty);
                let select = self.scoped_as_mut_select(&mut scoped, input_ty);

                let (values, rels) = self.compile_columns_scoped(columns);
                select.projection = self.projection(ty, values);
                select
                    .from
                    .extend(rels.into_iter().map(utils::lateral).map(utils::from));
            }
            cr::Transform::Window(columns) => {
                scoped = self.wrap_scoped(scoped, input_ty);
                let select = self.scoped_as_mut_select(&mut scoped, input_ty);

                let (values, rels) = self.compile_rels_scoped(columns);
                select.projection = self.projection(ty, values);
                select
                    .from
                    .extend(rels.into_iter().map(utils::lateral).map(utils::from));
            }

            cr::Transform::Where(cond) => {
                scoped = self.wrap_scoped(scoped, input_ty);
                let select = self.scoped_as_mut_select(&mut scoped, input_ty);

                let cond = self.compile_column(cond);
                select.selection = Some(cond.expr.into_expr());

                select.from.extend(
                    cond.rel_vars
                        .into_iter()
                        .map(utils::lateral)
                        .map(utils::from),
                );
            }
            cr::Transform::Limit(limit) => {
                let must_wrap = scoped
                    .as_query()
                    .is_none_or(|q| q.limit.is_some() || q.offset.is_some());
                if must_wrap {
                    scoped = self.wrap_scoped(scoped, input_ty);
                }

                let query = scoped.as_mut_query().unwrap();
                let limit = self.compile_column(limit);
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
                    .as_query()
                    .is_none_or(|q| q.limit.is_some() || q.offset.is_some());
                if must_wrap {
                    scoped = self.wrap_scoped(scoped, input_ty);
                }

                let query = scoped.as_mut_query().unwrap();
                let offset = self.compile_column(offset);
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
                let key = self.compile_column(key);
                select.projection[0] = sql_ast::SelectItem::ExprWithAlias {
                    expr: key.expr.into_expr(),
                    alias: sql_ast::Ident::new(COL_ARRAY_INDEX),
                };

                select.from.extend(
                    key.rel_vars
                        .into_iter()
                        .map(utils::lateral)
                        .map(utils::from),
                );
            }

            cr::Transform::Group(key) => {
                let input = scoped.clone();

                // wrap into a new query
                scoped = self.wrap_scoped(scoped, input_ty);
                let select = self.scoped_as_mut_select(&mut scoped, input_ty);

                let key = self.compile_columns(key);

                let mut projection = vec![
                    // index
                    ExprOrSource::Source("(ROW_NUMBER() OVER ())::int4".into()),
                ];
                // key
                projection.extend(key.clone());
                // values (JSON packed)
                let values = self.compile_json_pack(input, input_ty);
                projection.push(values.expr);
                select.projection = self.projection(ty, projection);

                select.group_by = sql_ast::GroupByExpr::Expressions(
                    key.into_iter().map(ExprOrSource::into_expr).collect(),
                    vec![],
                );
            }
        }
        scoped
    }

    fn compile_rels_scoped(
        &mut self,
        columns: &[cr::Expr],
    ) -> (Vec<ExprOrSource>, Vec<sql_ast::TableFactor>) {
        let mut exprs = Vec::with_capacity(columns.len());
        let mut rel_vars = Vec::new();
        for col in columns {
            let expr = self.compile_rel(col);
            exprs.push(expr.expr);
            rel_vars.extend(expr.rel_vars);
        }
        (exprs, rel_vars)
    }

    fn compile_columns_scoped(
        &mut self,
        columns: &[cr::Expr],
    ) -> (Vec<ExprOrSource>, Vec<sql_ast::TableFactor>) {
        let mut exprs = Vec::with_capacity(columns.len());
        let mut rel_vars = Vec::new();
        for col in columns {
            let expr = self.compile_column(col);
            exprs.push(expr.expr);
            rel_vars.extend(expr.rel_vars);
        }
        (exprs, rel_vars)
    }

    fn compile_columns(&mut self, columns: &[cr::Expr]) -> Vec<ExprOrSource> {
        let mut column_exprs = Vec::with_capacity(columns.len());
        for col in columns {
            let e = self.compile_column(col);
            column_exprs.push(self.scoped_into_expr(e, &col.ty));
        }
        column_exprs
    }

    fn compile_column(&mut self, expr: &cr::Expr) -> Scoped {
        let mut rel = self.compile_rel(expr);

        if let Some(simplified) = rel.as_simplified_expr() {
            rel = simplified;
        }

        // pack to JSON if needed
        if expr.ty.kind.is_array() {
            rel = self.compile_json_pack(rel, &expr.ty);
        }

        rel
    }

    fn compile_json_pack(&mut self, mut scoped: Scoped, ty: &ir::Ty) -> Scoped {
        let Some(input) = scoped.expr.as_rel_var() else {
            panic!("JSON pack expected a relation, got: {scoped:?}")
        };
        let cols = self.rel_cols(ty, false);

        scoped.expr = match &self.get_ty_mat(ty).kind {
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
        };
        scoped
    }

    fn compile_json_unpack(&mut self, mut scoped: Scoped, ty: &ir::Ty) -> Scoped {
        if let Some(simplified) = scoped.as_simplified_expr() {
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
        ty: &ir::Ty,
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
                let ty = self.compile_ty_name(ty);
                ExprOrSource::Source(format!("COALESCE(SUM({arg}), 0)::{ty}"))
            }
            "std::average" => {
                let arg = args.into_iter().next().unwrap();
                let ty = self.compile_ty_name(ty);
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

                let filler = get_default_value_for_ty(ty);
                ExprOrSource::Source(format!(
                    "COALESCE(LEAD({arg}, {offset}::int4) OVER (ORDER BY index), {filler})"
                ))
            }
            "std::lag" => {
                let mut args = args.into_iter();
                let arg = args.next().unwrap();
                let offset = args.next().unwrap();

                let filler = get_default_value_for_ty(ty);

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

            "std::text_ops::concat" => utils::new_bin_op("||", args),

            _ => todo!("sql impl for {id}"),
        }
    }

    fn null(&self, ty: &ir::Ty) -> sql_ast::Expr {
        sql_ast::Expr::Cast {
            kind: sql_ast::CastKind::DoubleColon,
            expr: Box::new(utils::value(sql_ast::Value::Null)),
            data_type: self.compile_ty_name(ty),
            format: None,
        }
    }

    fn compile_ty_name(&self, ty: &ir::Ty) -> sql_ast::DataType {
        let type_name = match self.get_ty_mat(ty).kind {
            ir::TyKind::Primitive(prim) => match prim {
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
            },
            ir::TyKind::Tuple(ref fields) if fields.is_empty() => {
                // unit type (holds no data) does not exist in sql so we use a type with
                // the least amount of data
                "bool"
            }
            ir::TyKind::Tuple(_) => todo!(),
            ir::TyKind::Array(_) => "text",
            ir::TyKind::Enum(_) => panic!("null value for enum does not make sense"),
            ir::TyKind::Function(_) => todo!(),
            ir::TyKind::Ident(_) => todo!(),
        };
        sql_ast::DataType::Custom(
            sql_ast::ObjectName(vec![sql_ast::Ident::new(type_name)]),
            vec![],
        )
    }

    fn compile_literal(&self, lit: &ir::Literal, ty: &ir::Ty) -> ExprOrSource {
        ExprOrSource::Source(match lit {
            ir::Literal::bool(true) => "TRUE".to_string(),
            ir::Literal::bool(false) => "FALSE".to_string(),
            ir::Literal::int8(v) => format!("{v}::{}", self.compile_ty_name(ty)),
            ir::Literal::int16(v) => format!("{v}::{}", self.compile_ty_name(ty)),
            ir::Literal::int32(v) => format!("{v}::{}", self.compile_ty_name(ty)),
            ir::Literal::int64(v) => format!("{v}::{}", self.compile_ty_name(ty)),
            ir::Literal::uint8(v) => format!("{v}::{}", self.compile_ty_name(ty)),
            ir::Literal::uint16(v) => format!("{v}::{}", self.compile_ty_name(ty)),
            ir::Literal::uint32(v) => format!("{v}::{}", self.compile_ty_name(ty)),
            ir::Literal::uint64(v) => format!("{v}::{}", self.compile_ty_name(ty)),
            ir::Literal::float32(v) => format!("{v}::{}", self.compile_ty_name(ty)),
            ir::Literal::float64(v) => format!("{v}::{}", self.compile_ty_name(ty)),
            ir::Literal::text(s) => {
                return ExprOrSource::new_expr(sql_ast::Expr::Value(
                    sql_ast::Value::SingleQuotedString(s.clone()),
                ));
            }
        })
    }

    fn construct_empty_rel(&self, ty: &ir::Ty) -> sql_ast::SetExpr {
        // construct a rel without rows
        let mut values = Vec::new();

        if ty.kind.is_array() {
            values.push(utils::number("0"));
        }
        let item_ty = match &ty.kind {
            ir::TyKind::Primitive(_) | ir::TyKind::Tuple(_) => ty,
            ir::TyKind::Array(item_ty) => item_ty,
            ir::TyKind::Enum(_) | ir::TyKind::Function(_) | ir::TyKind::Ident(_) => {
                todo!()
            }
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
        select.projection = self.projection(ty, values.into_iter().map(ExprOrSource::new_expr));
        select.selection = Some(utils::bool(false));
        sql_ast::SetExpr::Select(Box::new(select))
    }
}

fn get_default_value_for_ty(ty: &ir::Ty) -> &str {
    let item_ty = ty.kind.as_array().unwrap();
    match &item_ty.kind {
        ir::TyKind::Primitive(ir::TyPrimitive::int8) => "0::\"char\"",
        ir::TyKind::Primitive(ir::TyPrimitive::int16) => "0::int2",
        ir::TyKind::Primitive(ir::TyPrimitive::int32) => "0::int4",
        ir::TyKind::Primitive(ir::TyPrimitive::int64) => "0::int8",
        ir::TyKind::Primitive(ir::TyPrimitive::float32) => "0.0::float4",
        ir::TyKind::Primitive(ir::TyPrimitive::float64) => "0.0::float8",
        ir::TyKind::Primitive(ir::TyPrimitive::bool) => "FALSE",
        ir::TyKind::Primitive(ir::TyPrimitive::text) => "''",
        _ => todo!(),
    }
}
