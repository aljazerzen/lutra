use std::collections::HashMap;

use lutra_bin::ir;

use crate::sql::COL_ARRAY_INDEX;
use crate::sql::utils::{ExprOrRelVar, RelCols, Scoped};
use crate::sql::{cr, utils};
use crate::utils::NameGenerator;

pub fn compile(rel: cr::Expr, types: HashMap<&ir::Path, &ir::Ty>) -> sql_ast::Query {
    let mut ctx = Context::new(types);

    let rel_ty = rel.ty.clone();
    let query = ctx.compile_rel(&rel);

    if rel_ty.kind.is_array() {
        // wrap into a top-level projection to remove array indexes & apply the order
        let rel_name = query.expr.as_rel_var().map(|s| s.to_string());
        let mut query = ctx.scoped_into_select_query_ext(query, &rel_ty, false);
        query.order_by = Some(utils::order_by_one(utils::identifier(
            rel_name,
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

    fn register_rel_var(&mut self, input: &cr::BoundExpr, output: &mut Scoped) {
        let name = self.scoped_as_rel_var(output, &input.rel.ty).to_string();
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

    #[tracing::instrument(name = "r", skip_all)]
    fn compile_rel(&mut self, rel: &cr::Expr) -> Scoped {
        match &rel.kind {
            cr::ExprKind::From(from) => self.compile_from(from, &rel.ty),

            cr::ExprKind::Join(left_in, right_in, condition) => {
                tracing::debug!("Join");

                let mut left = self.compile_rel(&left_in.rel);
                let left_name = self
                    .scoped_as_rel_var(&mut left, &left_in.rel.ty)
                    .to_string();

                let mut right = self.compile_rel(&right_in.rel);
                let right_name = self
                    .scoped_as_rel_var(&mut right, &right_in.rel.ty)
                    .to_string();

                self.register_rel_var(left_in, &mut left);
                self.register_rel_var(right_in, &mut right);

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
                                .map(|col| utils::identifier(Some(&left_name), col)),
                            self.rel_cols(&right_in.rel.ty, true)
                                .map(|col| utils::identifier(Some(&right_name), col)),
                        ),
                    ),
                );

                if let Some(condition) = condition {
                    select.selection = Some(self.compile_column(condition));
                }

                self.unregister_rel_var(left_in);
                self.unregister_rel_var(right_in);

                scope
            }

            cr::ExprKind::BindCorrelated(bound_in, main_in) => {
                tracing::debug!("BindCorrelated");

                // compile inner-to-outer
                let mut bound = self.compile_rel(&bound_in.rel);

                self.register_rel_var(bound_in, &mut bound);
                let mut main = self.compile_rel(main_in);
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
                    assert!(matches!(bound.expr, ExprOrRelVar::RelVar(_)));

                    main.rel_vars = std::iter::Iterator::chain(
                        bound.rel_vars.into_iter(),
                        main.rel_vars.into_iter().map(utils::lateral),
                    )
                    .collect();

                    main
                }
            }

            cr::ExprKind::Transform(input, transform) => {
                tracing::debug!("Transform");

                let mut input_sql = self.compile_rel(&input.rel);

                self.register_rel_var(input, &mut input_sql);
                let r = self.compile_rel_transform(input_sql, transform, &input.rel.ty, &rel.ty);
                self.unregister_rel_var(input);

                r
            }

            cr::ExprKind::Bind(val_in, main_in) => {
                tracing::debug!("Bind");

                let name = self.rel_name_gen.next();

                let val = self.compile_rel(&val_in.rel);

                self.register_cte(val_in, name.clone());
                let main = self.compile_rel(main_in);
                self.unregister_rel_var(val_in);

                let val = self.scoped_into_query(val, &val_in.rel.ty);
                let mut main = self.scoped_into_select_query(main, &main_in.ty);

                main.with = Some(utils::with());
                let ctes = &mut main.with.as_mut().unwrap().cte_tables;
                ctes.insert(0, utils::cte(name, val));

                self.query_into_scoped(main)
            }

            cr::ExprKind::Union(parts) => {
                tracing::debug!("Union");

                let mut res = None;
                for part_in in parts {
                    let part = self.compile_rel(part_in);

                    let part = self.scoped_into_query(part, &part_in.ty);

                    if let Some(r) = res {
                        res = Some(utils::union(r, utils::query_into_set_expr(part)))
                    } else {
                        res = Some(utils::query_into_set_expr(part))
                    }
                }

                let query =
                    utils::query_new(res.unwrap_or_else(|| self.construct_empty_rel(&rel.ty)));

                self.query_into_scoped(query)
            }

            cr::ExprKind::Iteration(initial_in, step_in) => {
                let initial = self.compile_rel(initial_in);

                let re_name = self.rel_name_gen.next();

                self.register_cte(step_in, re_name.clone());
                let step = self.compile_rel(&step_in.rel);
                self.unregister_rel_var(step_in);

                let cte_query = utils::union(
                    utils::query_into_set_expr(self.scoped_into_query(initial, &initial_in.ty)),
                    utils::query_into_set_expr(self.scoped_into_query(step, &step_in.rel.ty)),
                );

                let mut main = utils::select_empty();
                main.projection = self.projection_noop(None, &rel.ty, true);
                main.from = vec![utils::new_table(
                    utils::new_object_name([re_name.clone()]),
                    None,
                )];

                let mut main = utils::query_select(main);
                main.with = Some(utils::with());

                let with = main.with.as_mut().unwrap();
                with.recursive = true;
                with.cte_tables
                    .push(utils::cte(re_name, utils::query_new(cte_query)));

                self.query_into_scoped(main)
            }
        }
    }

    fn compile_from(&mut self, from: &cr::From, ty: &ir::Ty) -> Scoped {
        tracing::debug!("From::{}", from.as_ref());
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
                    let rel_var =
                        utils::new_table(utils::new_object_name([rel_name]), Some(alias.clone()));
                    Scoped::new(alias, vec![rel_var])
                } else {
                    Scoped {
                        expr: ExprOrRelVar::RelVar(rel_name),
                        rel_vars: Vec::new(),
                    }
                }
            }

            cr::From::Table(table_ident) => {
                let mut select = utils::select_empty();

                select
                    .from
                    .push(utils::new_table(translate_table_ident(table_ident), None));

                let columns = itertools::chain(
                    // index
                    Some("NULL".to_string()),
                    // table columns
                    self.get_table_columns(ty).iter().map(|f| {
                        let name = utils::identifier(None::<&str>, f.name.clone().unwrap());
                        let ty = self.compile_ty_name(&f.ty);
                        format!("{name}::{ty}")
                    }),
                )
                .map(sql_ast::Expr::Source);

                select.projection = self.projection(ty, columns);

                self.query_into_scoped(utils::query_select(select))
            }

            cr::From::Null => ExprOrRelVar::new_expr(self.null(ty)).into(),
            cr::From::Literal(literal) => Scoped::from(self.compile_literal(literal, ty)),
            cr::From::FuncCall(func_name, args_in) => {
                self.compile_func_call(func_name, args_in, ty)
            }
            cr::From::Param(param_index) => Scoped::from(ExprOrRelVar::new(format!(
                "${}::{}",
                param_index + 1,
                self.compile_ty_name(ty)
            ))),

            cr::From::JsonUnpack(input) => {
                let input = self.compile_rel(input);
                self.deserialize_json(input, ty)
            }

            cr::From::JsonPack(expr_in) => {
                let expr = self.compile_rel(expr_in);
                self.serialize_json(expr, &expr_in.ty)
            }

            cr::From::Case(cases) => {
                let mut sql_cases = Vec::new();
                for (condition_in, result_in) in cases.iter().take(cases.len() - 1) {
                    let condition = self.compile_column(condition_in);

                    let result = self.compile_column(result_in);

                    sql_cases.push(sql_ast::CaseWhen { condition, result });
                }

                // else
                let (_, else_value_in) = cases.last().unwrap();
                let else_value = self.compile_column(else_value_in);

                Scoped::from(ExprOrRelVar::Expr(Box::new(sql_ast::Expr::Case {
                    operand: None,
                    cases: sql_cases,
                    else_result: Some(Box::new(else_value)),
                })))
            }

            cr::From::SQLSource(source) => Scoped::from(ExprOrRelVar::new(format!("({source})"))),
        }
    }

    /// Returns columns of the table, described by a type.
    /// This is used in FROM and INSERT, not the in-query relation repr.
    fn get_table_columns<'t>(&'t self, ty: &'t ir::Ty) -> &'t [ir::TyTupleField] {
        let ty_tuple = self.get_ty_mat(ty).kind.as_array().unwrap();
        self.get_ty_mat(ty_tuple).kind.as_tuple().unwrap()
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
                let old_values = select.projection.drain(..).map(utils::unwrap_select_item);
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
                let old_values = select.projection.drain(..).map(utils::unwrap_select_item);
                select.projection = self.projection(ty, old_values);
            }

            cr::Transform::Aggregate(columns) | cr::Transform::Window(columns) => {
                scoped = self.wrap_scoped(scoped, input_ty);
                let select = self.scoped_as_mut_select(&mut scoped, input_ty);

                let (values, rel_vars) = self.compile_rels_scoped(columns);
                select.projection = self.projection(ty, values);
                select.from.extend(rel_vars.into_iter().map(utils::lateral));
            }

            cr::Transform::Where(cond_in) => {
                scoped = self.wrap_scoped(scoped, input_ty);
                let select = self.scoped_as_mut_select(&mut scoped, input_ty);

                let cond = self.compile_column(cond_in);
                select.selection = Some(cond);
            }
            cr::Transform::Limit(limit_in) => {
                let must_wrap = scoped
                    .as_query()
                    .is_none_or(|q| q.limit.is_some() || q.offset.is_some());
                if must_wrap {
                    scoped = self.wrap_scoped(scoped, input_ty);
                }

                let query = scoped.as_mut_query().unwrap();
                let limit = self.compile_column(limit_in);
                query.limit = Some(limit);

                if input_ty.kind.is_array() {
                    // apply order from index column
                    if query.order_by.is_none() {
                        query.order_by = Some(utils::order_by_one(utils::identifier(
                            None::<&str>,
                            COL_ARRAY_INDEX,
                        )));
                    }
                }
            }
            cr::Transform::Offset(offset_in) => {
                let must_wrap = scoped
                    .as_query()
                    .is_none_or(|q| q.limit.is_some() || q.offset.is_some());
                if must_wrap {
                    scoped = self.wrap_scoped(scoped, input_ty);
                }

                let query = scoped.as_mut_query().unwrap();
                let offset = self.compile_column(offset_in);
                query.offset = Some(offset);
                if query.order_by.is_none() {
                    query.order_by = Some(utils::order_by_one(utils::identifier(
                        None::<&str>,
                        COL_ARRAY_INDEX,
                    )));
                }
            }
            cr::Transform::IndexBy(key) => {
                // wrap into a new query
                scoped = self.wrap_scoped(scoped, input_ty);

                // overwrite array index
                let select = self.scoped_as_mut_select(&mut scoped, input_ty);
                let key = if let Some(key_in) = key {
                    self.compile_column(key_in)
                } else {
                    sql_ast::Expr::Source("(ROW_NUMBER() OVER ())::int4".into())
                };
                select.projection[0] = sql_ast::SelectItem {
                    expr: key,
                    alias: Some(utils::new_ident(COL_ARRAY_INDEX)),
                };
            }
            cr::Transform::Order => {
                let must_wrap = scoped.as_query().is_none();
                if must_wrap {
                    scoped = self.wrap_scoped(scoped, input_ty);
                }

                // overwrite ORDER BY
                let query = scoped.as_mut_query().unwrap();
                query.order_by = Some(utils::order_by_one(utils::identifier(
                    None::<&str>,
                    COL_ARRAY_INDEX,
                )));
            }

            cr::Transform::Group(key, values_in) => {
                // wrap into a new query
                scoped = self.wrap_scoped(scoped, input_ty);
                let select = self.scoped_as_mut_select(&mut scoped, input_ty);

                let key = self.compile_columns(key);

                let mut projection = vec![
                    // index
                    sql_ast::Expr::Source("(ROW_NUMBER() OVER ())::int4".into()),
                ];

                // value
                projection.extend(self.compile_columns(values_in));

                select.group_by = sql_ast::GroupByExpr::Expressions(key, vec![]);
                select.projection = self.projection(ty, projection);
            }

            cr::Transform::Insert(table_ident) => {
                let source = self.scoped_into_select_query_ext(scoped, input_ty, false);

                let table = translate_table_ident(table_ident);

                let columns = self
                    .get_table_columns(input_ty)
                    .iter()
                    .map(|field| field.name.as_ref().unwrap())
                    .map(utils::new_ident)
                    .collect();

                let insert = sql_ast::Insert {
                    source: Box::new(source),
                    table,
                    columns,
                };
                let query = utils::query_new(sql_ast::SetExpr::Insert(insert));
                scoped = self.query_into_scoped(query);
            }
        }
        scoped
    }

    fn compile_rels_scoped(
        &mut self,
        columns: &[cr::Expr],
    ) -> (Vec<sql_ast::Expr>, Vec<sql_ast::RelVar>) {
        let mut exprs = Vec::with_capacity(columns.len());
        let mut rel_vars = Vec::new();
        for col in columns {
            let scoped = self.compile_rel(col);
            exprs.push(match scoped.expr {
                ExprOrRelVar::Expr(expr) => *expr,
                ExprOrRelVar::RelVar(rel_var) => utils::identifier(Some(rel_var), "value"),
            });
            rel_vars.extend(scoped.rel_vars);
        }
        (exprs, rel_vars)
    }

    fn compile_columns(&mut self, columns: &[cr::Expr]) -> Vec<sql_ast::Expr> {
        let mut column_exprs = Vec::with_capacity(columns.len());
        for col in columns {
            column_exprs.push(self.compile_column(col));
        }
        column_exprs
    }

    /// Compile an expression as relation and then heavily try to fit the result into a column.
    /// In the worst case, this creates a subquery.
    fn compile_column(&mut self, expr: &cr::Expr) -> sql_ast::Expr {
        let mut rel = self.compile_rel(expr);

        if let Some(simplified) = rel.as_simplified_expr() {
            rel = simplified;
        }

        if rel.rel_vars.is_empty() {
            return match rel.expr {
                // happy case: this is just a single expr
                ExprOrRelVar::Expr(expr) => *expr,

                // this is a reference to a relation that we don't need to emit here
                // so we can just return identifier
                ExprOrRelVar::RelVar(rel_var) => {
                    let c_name = self.rel_cols(&expr.ty, true).next().unwrap();
                    utils::identifier(Some(rel_var), c_name)
                }
            };
        }

        if let Some(query) = rel.as_query() {
            return sql_ast::Expr::Subquery(Box::new(query.clone()));
        }

        let query = self.scoped_into_select_query(rel, &expr.ty);
        sql_ast::Expr::Subquery(Box::new(query))
    }

    fn compile_func_call(&mut self, id: &str, args_in: &[cr::Expr], ty: &ir::Ty) -> Scoped {
        let args = self.compile_columns(args_in);
        let expr = match id {
            "std::mul" => utils::new_bin_op("*", args),
            "std::div" => utils::new_bin_op("/", args),
            "std::mod" => match ty.kind.as_primitive().unwrap() {
                ir::TyPrimitive::float32 | ir::TyPrimitive::float64 => {
                    let [l, r] = unpack_args(args);
                    sql_ast::Expr::Source(format!("MOD({l}::numeric, {r}::numeric)::float8"))
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

            "std::min" => utils::new_func_call("MIN", args),
            "std::max" => utils::new_func_call("MAX", args),
            "std::sum" => {
                let [arg] = unpack_args(args);
                let ty = self.compile_ty_name(ty);
                sql_ast::Expr::Source(format!("COALESCE(SUM({arg}), 0)::{ty}"))
            }
            "std::average" => {
                let [arg] = unpack_args(args);
                let ty = self.compile_ty_name(ty);
                sql_ast::Expr::Source(format!("AVG({arg})::{ty}"))
            }
            "std::count" => sql_ast::Expr::Source("COUNT(*)".into()),
            "std::any" => sql_ast::Expr::Source(format!(
                "COALESCE({}, FALSE)",
                utils::new_func_call("BOOL_OR", args)
            )),
            "std::all" => sql_ast::Expr::Source(format!(
                "COALESCE({}, TRUE)",
                utils::new_func_call("BOOL_AND", args)
            )),
            "std::contains" => {
                let [haystack, needle] = unpack_args(args);
                sql_ast::Expr::Source(format!("COALESCE(BOOL_OR({needle} = {haystack}), FALSE)"))
            }

            "std::row_number" => sql_ast::Expr::Source("(ROW_NUMBER() OVER () - 1)".to_string()),
            "std::lead" => {
                let [arg, offset] = unpack_args(args);

                let filler = get_default_value_for_ty(ty);
                sql_ast::Expr::Source(format!(
                    "COALESCE(LEAD({arg}, {offset}::int4) OVER (ORDER BY index), {filler})"
                ))
            }
            "std::lag" => {
                let [arg, offset] = unpack_args(args);

                let filler = get_default_value_for_ty(ty);

                sql_ast::Expr::Source(format!(
                    "COALESCE(LAG({arg}, {offset}::int4) OVER (ORDER BY index), {filler})"
                ))
            }

            "std::text::length" => {
                let [text] = unpack_args(args);
                sql_ast::Expr::Source(format!("LENGTH({text})::int4"))
            }
            "std::text::concat" => utils::new_bin_op("||", args),

            "std::math::abs" => {
                let [text] = unpack_args(args);
                sql_ast::Expr::Source(format!("ABS({text})"))
            }
            "std::math::pow" => {
                let [operand, exponent] = unpack_args(args);
                sql_ast::Expr::Source(format!("POW({operand}, {exponent})"))
            }

            "greatest" => {
                let mut args = args.into_iter();
                sql_ast::Expr::Source(format!(
                    "GREATEST({}, {})::int8",
                    args.next().unwrap(),
                    args.next().unwrap(),
                ))
            }

            "is_null" => {
                let [arg] = unpack_args(args);
                sql_ast::Expr::Source(format!("{arg} IS NULL"))
            }
            "is_not_null" => {
                let [arg] = unpack_args(args);
                sql_ast::Expr::Source(format!("{arg} IS NOT NULL"))
            }

            "std::to_int8" | "std::to_int16" | "std::to_int32" | "std::to_int64"
            | "std::to_uint8" | "std::to_uint16" | "std::to_uint32" | "std::to_uint64" => {
                let [arg] = unpack_args(args);
                let ty = self.compile_ty_name(ty);
                match args_in[0].ty.kind.as_primitive().unwrap() {
                    ir::TyPrimitive::int8
                    | ir::TyPrimitive::int16
                    | ir::TyPrimitive::int32
                    | ir::TyPrimitive::int64
                    | ir::TyPrimitive::uint8
                    | ir::TyPrimitive::uint16
                    | ir::TyPrimitive::uint32
                    | ir::TyPrimitive::uint64 => sql_ast::Expr::Source(format!("{arg}::{ty}")),
                    ir::TyPrimitive::float32 | ir::TyPrimitive::float64 => {
                        sql_ast::Expr::Source(format!("trunc({arg})::{ty}"))
                    }
                    _ => panic!(),
                }
            }

            "std::to_float32" | "std::to_float64" => {
                let [arg] = unpack_args(args);
                let ty = self.compile_ty_name(ty);
                sql_ast::Expr::Source(format!("{arg}::{ty}"))
            }

            _ => todo!("sql impl for {id}"),
        };
        expr.into()
    }

    fn null(&self, ty: &ir::Ty) -> sql_ast::Expr {
        sql_ast::Expr::Source(format!("NULL::{}", self.compile_ty_name(ty)))
    }

    pub(super) fn compile_ty_name(&self, ty: &ir::Ty) -> &'static str {
        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Primitive(prim) => match prim {
                ir::TyPrimitive::bool => "bool",

                ir::TyPrimitive::int8 => "int2",
                ir::TyPrimitive::uint8 => "int2",
                ir::TyPrimitive::int16 => "int2",
                ir::TyPrimitive::uint16 => "int2",

                ir::TyPrimitive::int32 => "int4",
                ir::TyPrimitive::uint32 => "int4",

                ir::TyPrimitive::int64 => "int8",
                ir::TyPrimitive::uint64 => "int8",

                ir::TyPrimitive::float32 => "float4",
                ir::TyPrimitive::float64 => "float8",
                ir::TyPrimitive::text => "text",
            },
            ir::TyKind::Tuple(fields) if fields.is_empty() => {
                // unit type (holds no data) does not exist in sql so we use a type with
                // the least amount of data
                "bool"
            }
            ir::TyKind::Enum(variants) if utils::is_maybe(variants) => {
                self.compile_ty_name(&variants[1].ty)
            }

            // serialize as json
            ir::TyKind::Tuple(_) | ir::TyKind::Array(_) | ir::TyKind::Enum(_) => "jsonb",

            ir::TyKind::Function(_) => todo!(),
            ir::TyKind::Ident(_) => todo!(),
        }
    }

    fn compile_literal(&self, lit: &ir::Literal, ty: &ir::Ty) -> sql_ast::Expr {
        sql_ast::Expr::Source(match lit {
            ir::Literal::bool(true) => "TRUE".to_string(),
            ir::Literal::bool(false) => "FALSE".to_string(),
            ir::Literal::int8(v) => {
                if *v >= 0 {
                    format!("{v}::{}", self.compile_ty_name(ty))
                } else {
                    format!("({v})::{}", self.compile_ty_name(ty))
                }
            }
            ir::Literal::int16(v) => {
                if *v >= 0 {
                    format!("{v}::{}", self.compile_ty_name(ty))
                } else {
                    format!("({v})::{}", self.compile_ty_name(ty))
                }
            }
            ir::Literal::int32(v) => {
                if *v >= 0 {
                    format!("{v}::{}", self.compile_ty_name(ty))
                } else {
                    format!("({v})::{}", self.compile_ty_name(ty))
                }
            }
            ir::Literal::int64(v) => {
                if *v >= 0 {
                    format!("{v}::{}", self.compile_ty_name(ty))
                } else {
                    format!("({v})::{}", self.compile_ty_name(ty))
                }
            }
            ir::Literal::uint8(v) => {
                if *v < 0x80 {
                    format!("{v}::{}", self.compile_ty_name(ty))
                } else {
                    format!("{}::{}", *v as i8, self.compile_ty_name(ty))
                }
            }
            ir::Literal::uint16(v) => {
                if *v < 0x8000 {
                    format!("{v}::{}", self.compile_ty_name(ty))
                } else {
                    format!("{}::{}", *v as i16, self.compile_ty_name(ty))
                }
            }
            ir::Literal::uint32(v) => {
                if *v < 0x80000000 {
                    format!("{v}::{}", self.compile_ty_name(ty))
                } else {
                    format!("{}::{}", *v as i32, self.compile_ty_name(ty))
                }
            }
            ir::Literal::uint64(v) => {
                if *v < 0x8000000000000000 {
                    format!("{v}::{}", self.compile_ty_name(ty))
                } else {
                    format!("{}::{}", *v as i64, self.compile_ty_name(ty))
                }
            }
            ir::Literal::float32(v) => format!("{v}::{}", self.compile_ty_name(ty)),
            ir::Literal::float64(v) => format!("{v}::{}", self.compile_ty_name(ty)),
            ir::Literal::text(s) => {
                let escaped = sql_ast::escape_string(s, '\'');
                return sql_ast::Expr::Source(format!("'{escaped}'"));
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
        select.projection = self.projection(ty, values);
        select.selection = Some(utils::bool(false));
        sql_ast::SetExpr::Select(Box::new(select))
    }
}

fn get_default_value_for_ty(ty: &ir::Ty) -> &str {
    let item_ty = ty.kind.as_array().unwrap();
    match &item_ty.kind {
        ir::TyKind::Primitive(ir::TyPrimitive::int8) => "0::int2",
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

fn unpack_args<const N: usize>(
    args: impl IntoIterator<Item = sql_ast::Expr>,
) -> [sql_ast::Expr; N] {
    let mut r = Vec::with_capacity(N);
    let mut args = args.into_iter();
    for _ in 0..N {
        r.push(args.next().unwrap());
    }
    r.try_into().unwrap()
}

/// Converts a "lutra table identifier" into an SQL object name.
///
/// For example:
/// - `"invoices" -> invoices`
/// - `"mySchema/invoices" -> "mySchema".invoices`
/// - `"my_schema/hello/world" -> my_schema."hello/world"`
/// - `"public/my_dir/sub_dir/data.parquet" -> public."my_dir/sub_dir/data.parquet"`
///
/// Splits the name on first slash and uses preceding part (if it exists)
/// for the schema name, and following part as the table name.
/// Thus, table name can contain slashes.
///
/// This function will be specialized for different databases,
/// because current behavior targets PostgreSQL.
/// For example, BigQuery uses organizations, projects and datasets in table identifiers.
/// So when translating to BigQuery SQL we will translate:
/// - `"my_organization/my_project/my_dataset/my_table" -> my_organization.my_project.my_dataset.my_table`,
fn translate_table_ident(table_ident: &str) -> sql_ast::ObjectName {
    if let Some((schema, table)) = table_ident.split_once('/') {
        utils::new_object_name([schema, table])
    } else {
        utils::new_object_name([table_ident])
    }
}
