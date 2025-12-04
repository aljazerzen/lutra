use std::collections::HashMap;

use lutra_bin::ir;

use crate::sql::COL_ARRAY_INDEX;
use crate::sql::utils::{Node, RelCols};
use crate::sql::{cr, utils};
use crate::utils::NameGenerator;

pub fn compile(rel: cr::Expr, types: HashMap<&ir::Path, &ir::Ty>) -> sql_ast::Query {
    let mut ctx = Context::new(types);

    let rel_ty = rel.ty.clone();
    let query = ctx.compile_rel(&rel);

    if rel_ty.kind.is_array() {
        // drop index column
        let mut select = ctx.node_into_select(query, &rel_ty);
        let index = select.projection.remove(0);

        // apply the order
        let mut query = utils::query_select(select);
        query.order_by = Some(utils::order_by_one(index.expr));
        query
    } else {
        ctx.node_into_query(query, &rel_ty)
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

    fn register_rel_var(&mut self, input: &cr::BoundExpr, name: String) {
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
    fn compile_rel(&mut self, rel: &cr::Expr) -> Node {
        match &rel.kind {
            cr::ExprKind::From(from) => self.compile_from(from, &rel.ty),

            cr::ExprKind::Join(left_in, right_in, condition) => {
                tracing::debug!("Join");

                let left = self.compile_rel(&left_in.rel);
                let (left_name, left_rels) = self.node_into_rel_var(left, &left_in.rel.ty);

                let right = self.compile_rel(&right_in.rel);
                let (right_name, right_rels) = self.node_into_rel_var(right, &right_in.rel.ty);

                let mut select = utils::select_empty();
                select.from.extend(left_rels);
                select.from.extend(right_rels);
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
                    self.register_rel_var(left_in, left_name);
                    self.register_rel_var(right_in, right_name);

                    select.selection = Some(self.compile_column(condition));

                    self.unregister_rel_var(left_in);
                    self.unregister_rel_var(right_in);
                }

                Node::Select(select)
            }

            cr::ExprKind::BindCorrelated(bound_in, main_in) => {
                tracing::debug!("BindCorrelated");

                // compile inner-to-outer
                let bound = self.compile_rel(&bound_in.rel);
                let (bound_name, bound_rels) = self.node_into_rel_var(bound, &bound_in.rel.ty);

                self.register_rel_var(bound_in, bound_name);
                let main = self.compile_rel(main_in);
                self.unregister_rel_var(bound_in);

                let mut main = self.node_into_select(main, &main_in.ty);

                main.from = std::iter::Iterator::chain(
                    bound_rels.into_iter(),
                    main.from.into_iter().map(utils::lateral),
                )
                .collect();

                Node::Select(main)
            }

            cr::ExprKind::Transform(input_in, transform) => {
                tracing::debug!("Transform::{}", transform.as_ref());

                let mut input = self.compile_rel(&input_in.rel);

                let needs_input_rel = matches!(
                    transform,
                    cr::Transform::Aggregate(_)
                        | cr::Transform::Where(_)
                        | cr::Transform::IndexBy(_)
                        | cr::Transform::Group(..)
                        | cr::Transform::Insert(_)
                );
                if needs_input_rel {
                    let (input_name, input_rel) = self.node_into_rel_var(input, &input_in.rel.ty);
                    input = input_rel
                        .map(Node::Rel)
                        .unwrap_or_else(|| Node::RelVar(input_name.clone()));

                    self.register_rel_var(input_in, input_name);
                }

                let r = self.compile_rel_transform(input, transform, &input_in.rel.ty, &rel.ty);

                if needs_input_rel {
                    self.unregister_rel_var(input_in);
                }

                r
            }

            cr::ExprKind::Bind(val_in, main_in) => {
                tracing::debug!("Bind");

                let name = self.rel_name_gen.next();

                let val = self.compile_rel(&val_in.rel);

                self.register_cte(val_in, name.clone());
                let main = self.compile_rel(main_in);
                self.unregister_rel_var(val_in);

                let val = self.node_into_query(val, &val_in.rel.ty);
                let mut main = self.node_into_query(main, &main_in.ty);

                let with = main.with.get_or_insert_with(utils::with);
                with.cte_tables.insert(0, utils::cte(name, val));

                Node::Query(main)
            }

            cr::ExprKind::Union(parts) => {
                tracing::debug!("Union");

                let mut res = None;
                for part_in in parts {
                    let part = self.compile_rel(part_in);

                    let part = self.node_into_query(part, &part_in.ty);

                    if let Some(r) = res {
                        res = Some(utils::union(r, utils::query_into_set_expr(part)))
                    } else {
                        res = Some(utils::query_into_set_expr(part))
                    }
                }

                let query =
                    utils::query_new(res.unwrap_or_else(|| self.construct_empty_rel(&rel.ty)));

                Node::Query(query)
            }

            cr::ExprKind::Iteration(initial_in, step_in) => {
                let initial = self.compile_rel(initial_in);

                let re_name = self.rel_name_gen.next();

                self.register_cte(step_in, re_name.clone());
                let step = self.compile_rel(&step_in.rel);
                self.unregister_rel_var(step_in);

                let cte_query = utils::union(
                    utils::query_into_set_expr(self.node_into_query(initial, &initial_in.ty)),
                    utils::query_into_set_expr(self.node_into_query(step, &step_in.rel.ty)),
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

                Node::Query(main)
            }
        }
    }

    fn compile_from(&mut self, from: &cr::From, ty: &ir::Ty) -> Node {
        tracing::debug!("From::{}", from.as_ref());
        match from {
            cr::From::Row(row) => Node::Columns {
                exprs: self.compile_columns(row),
                rels: vec![],
            },

            cr::From::RelRef(scope_id) => {
                let rel_ref = self.find_rel_var(*scope_id);
                let rel_name = rel_ref.name.clone();

                if rel_ref.is_cte {
                    let alias = self.rel_name_gen.next();
                    let rel = utils::new_table(utils::new_object_name([rel_name]), Some(alias));
                    Node::Rel(rel)
                } else {
                    Node::RelVar(rel_name)
                }
            }

            cr::From::Table(table_ident) => {
                let mut select = utils::select_empty();

                select
                    .from
                    .push(utils::new_table(translate_table_ident(table_ident), None));

                select.projection = self.pg_repr_import_projection(None, ty);

                Node::Select(select)
            }

            cr::From::Null => Node::from(self.null(ty)),
            cr::From::Literal(literal) => Node::from(self.compile_literal(literal, ty)),
            cr::From::FuncCall(func_name, args_in) => {
                self.compile_func_call(func_name, args_in, ty)
            }
            cr::From::Param(param_index) => Node::Source(format!(
                "${}::{}",
                param_index + 1,
                self.compile_ty_name(ty)
            )),

            cr::From::JsonUnpack(input_in) => {
                let input = self.compile_rel(input_in);
                self.deserialize_json(input, &input_in.ty, ty)
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

                Node::from(sql_ast::Expr::Case {
                    operand: None,
                    cases: sql_cases,
                    else_result: Some(Box::new(else_value)),
                })
            }

            cr::From::SQLSource(source) => {
                let node = Node::Source(source.clone());

                let mut select = utils::select_empty();
                let (rvar_name, rvar) = self.node_into_rel_var(node, ty);
                select.from.extend(rvar);
                select.projection = self.pg_repr_import_projection(Some(&rvar_name), ty);

                Node::Select(select)
            }
        }
    }

    /// Returns columns of the native pg repr, described by a type.
    /// This is used in FROM and INSERT, not the in-query relation repr.
    fn pg_repr_columns<'t>(&'t self, ty: &'t ir::Ty) -> Vec<(String, &'t ir::Ty)> {
        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Primitive(_) => vec![("value".into(), ty)],
            ir::TyKind::Tuple(fields) => fields
                .iter()
                .map(|f| (f.name.clone().unwrap(), &f.ty))
                .collect(),
            ir::TyKind::Array(item) => {
                // no index column
                self.pg_repr_columns(item)
            }
            ir::TyKind::Enum(_) => todo!(),
            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => unreachable!(),
        }
    }

    /// Constructs a projection that imports from native pg repr.
    fn pg_repr_import_projection(
        &self,
        rel_var: Option<&str>,
        ty: &ir::Ty,
    ) -> Vec<sql_ast::SelectItem> {
        let mut values = Vec::new();

        // index
        if self.get_ty_mat(ty).kind.is_array() {
            values.push(sql_ast::Expr::Source("NULL::int4".to_string()));
        }

        // table columns
        values.extend(
            self.pg_repr_columns(ty)
                .into_iter()
                .map(move |(f_name, f_ty)| {
                    let ident = utils::identifier(rel_var, f_name);
                    self.pg_repr_import(ident, f_ty)
                }),
        );
        self.projection(ty, values)
    }

    /// Imports a value from native pg repr into our repr.
    /// For example, pg type `date` is coverted into `int4`.
    fn pg_repr_import(&self, expr_pg: sql_ast::Expr, ty: &ir::Ty) -> sql_ast::Expr {
        // special case: std::Date
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && ty_ident.0 == ["std", "Date"]
        {
            return sql_ast::Expr::Source(format!("({expr_pg}::date - '1970-01-01'::date)"));
        }
        // special case: std::Time
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && ty_ident.0 == ["std", "Time"]
        {
            return sql_ast::Expr::Source(format!(
                "(EXTRACT(EPOCH FROM {expr_pg}) * 1000000)::int8"
            ));
        }

        // general case: noop
        // (but with a type cast, just to be safe)
        sql_ast::Expr::Source(format!("{expr_pg}::{}", self.compile_ty_name(ty)))
    }

    /// Exports a value into native pg repr from our repr.
    /// For example, type std::Date is exported from pg type `int4` into `date`.
    fn pg_repr_export(&self, expr: sql_ast::Expr, ty: &ir::Ty) -> sql_ast::Expr {
        // special case: std::Date
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && ty_ident.0 == ["std", "Date"]
        {
            return sql_ast::Expr::Source(format!("('1970-01-01'::date + {expr})"));
        }
        // special case: std::Time
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && ty_ident.0 == ["std", "Time"]
        {
            return sql_ast::Expr::Source(format!(
                "('00:00'::time + INTERVAL '1 microsecond' * {expr})"
            ));
        }

        // general case: noop
        expr
    }

    fn compile_rel_transform(
        &mut self,
        scoped: Node,
        transform: &cr::Transform,
        input_ty: &ir::Ty,
        ty: &ir::Ty,
    ) -> Node {
        let r = match transform {
            cr::Transform::ProjectRetain(cols) => {
                let (mut columns, rels) = self.node_into_columns_and_rels(scoped, input_ty);

                utils::retain_by_position(&mut columns, cols);

                // apply new column names
                Node::Columns {
                    exprs: columns,
                    rels,
                }
            }

            cr::Transform::ProjectDiscard(cols) => {
                let (mut columns, rels) = self.node_into_columns_and_rels(scoped, input_ty);

                utils::drop_by_position(&mut columns, cols);

                // apply new column names
                Node::Columns {
                    exprs: columns,
                    rels,
                }
            }

            cr::Transform::Aggregate(columns) => {
                let mut select = self.node_into_select(scoped, input_ty);

                let (values, rel_vars) = self.compile_columns_scoped(columns);
                select.projection = self.projection(ty, values);
                select.from.extend(rel_vars.into_iter().map(utils::lateral));
                Node::Select(select)
            }

            cr::Transform::Where(cond_in) => {
                // wrap into a new query
                let rel = self.node_into_rel(scoped, input_ty);
                let mut select = self.rel_into_select(rel, ty, true);

                let cond = self.compile_column(cond_in);
                select.selection = Some(cond);
                Node::Select(select)
            }
            cr::Transform::Limit(limit_in) => {
                let mut query = self.node_into_query(scoped, input_ty);
                if query.limit.is_some() || query.offset.is_some() {
                    query = self.wrap_query(query, input_ty);
                }

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
                Node::Query(query)
            }
            cr::Transform::Offset(offset_in) => {
                let mut query = self.node_into_query(scoped, input_ty);
                if query.limit.is_some() || query.offset.is_some() {
                    query = self.wrap_query(query, input_ty);
                }

                let offset = self.compile_column(offset_in);
                query.offset = Some(offset);
                if query.order_by.is_none() {
                    query.order_by = Some(utils::order_by_one(utils::identifier(
                        None::<&str>,
                        COL_ARRAY_INDEX,
                    )));
                }
                Node::Query(query)
            }
            cr::Transform::IndexBy(key) => {
                // wrap into a new query
                let rel = self.node_into_rel(scoped, input_ty);
                let mut select = self.rel_into_select(rel, ty, true);

                // overwrite array index
                let key = if let Some(key_in) = key {
                    let index = self.compile_column(key_in);
                    format!("(ROW_NUMBER() OVER (ORDER BY {index}))::int4")
                } else {
                    "(ROW_NUMBER() OVER ())::int4".into()
                };
                select.projection[0] = sql_ast::SelectItem {
                    expr: sql_ast::Expr::Source(key),
                    alias: Some(utils::new_ident(COL_ARRAY_INDEX)),
                };
                Node::Select(select)
            }
            cr::Transform::Order => {
                let mut query = self.node_into_query(scoped, input_ty);

                // overwrite ORDER BY
                query.order_by = Some(utils::order_by_one(utils::identifier(
                    None::<&str>,
                    COL_ARRAY_INDEX,
                )));
                Node::Query(query)
            }

            cr::Transform::Group(key, values_in) => {
                // wrap into a new query
                let rel = self.node_into_rel(scoped, input_ty);
                let mut select = self.rel_into_select(rel, ty, true);

                let key = self.compile_columns(key);

                let mut projection = vec![
                    // index
                    sql_ast::Expr::Source("(ROW_NUMBER() OVER ())::int4".into()),
                ];

                // value
                projection.extend(self.compile_columns(values_in));

                select.group_by = sql_ast::GroupByExpr::Expressions(key, vec![]);
                select.projection = self.projection(ty, projection);

                Node::Select(select)
            }

            cr::Transform::Insert(table_ident) => {
                let input_item_ty = self.get_ty_mat(input_ty).kind.as_array().unwrap();
                let input_fields_ty = self.get_ty_mat(input_item_ty).kind.as_tuple().unwrap();

                let mut source = self.node_into_select(scoped, input_ty);

                let new_projection = (source.projection.into_iter())
                    .skip(1) // first column will be index, discard it
                    .zip(input_fields_ty)
                    .map(|(p, f)| self.pg_repr_export(p.expr, &f.ty))
                    .map(|expr| sql_ast::SelectItem { expr, alias: None })
                    .collect();
                source.projection = new_projection;

                let table = translate_table_ident(table_ident);

                let columns = self
                    .pg_repr_columns(input_ty)
                    .iter()
                    .map(|(f_name, _f_ty)| f_name)
                    .map(utils::new_ident)
                    .collect();

                let insert = sql_ast::Insert {
                    source: Box::new(utils::query_select(source)),
                    table,
                    columns,
                };
                let query = utils::query_new(sql_ast::SetExpr::Insert(insert));
                Node::Query(query)
            }
        };
        tracing::trace!("-> {r:?}");
        r
    }

    fn compile_columns_scoped(
        &mut self,
        columns: &[cr::Expr],
    ) -> (Vec<sql_ast::Expr>, Vec<sql_ast::RelNamed>) {
        let mut exprs = Vec::with_capacity(columns.len());
        let mut relations = Vec::new();
        for col in columns {
            let scoped = self.compile_rel(col);
            let (expr, rels) = self.node_into_column_and_rels(scoped, &col.ty);
            exprs.push(expr);
            relations.extend(rels);
        }
        (exprs, relations)
    }

    fn compile_columns(&mut self, columns: &[cr::Expr]) -> Vec<sql_ast::Expr> {
        let mut column_exprs = Vec::with_capacity(columns.len());
        for col in columns {
            let value = self.compile_rel(col);
            column_exprs.extend(self.node_into_columns(value, &col.ty));
        }
        column_exprs
    }

    /// Compile an expression as relation and then fit the result into a column.
    /// In the worst case, this creates a subquery.
    fn compile_column(&mut self, expr: &cr::Expr) -> sql_ast::Expr {
        let rel = self.compile_rel(expr);
        self.node_into_column(rel, &expr.ty)
    }

    fn compile_func_call(&mut self, id: &str, args_in: &[cr::Expr], ty: &ir::Ty) -> Node {
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

            "std::cmp" => {
                let [a, b] = unpack_args(args);
                sql_ast::Expr::Source(format!(
                    "(SELECT CASE WHEN a < b THEN 0 WHEN a > b THEN 2 ELSE 1 END::int2 FROM (VALUES ({a}, {b})) t(a,b))"
                ))
            }
            "std::eq" => utils::new_bin_op("=", args),
            "std::lt" => utils::new_bin_op("<", args),
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
            "std::count" => {
                let [arg] = unpack_args(args);

                sql_ast::Expr::Source(if let sql_ast::Expr::CompoundIdentifier(parts) = arg {
                    let rvar = &parts[0];
                    format!("COUNT({rvar}.*)")
                } else {
                    // We need to reference arg, so we get aggregation of correct cardinality.
                    // But we cannot just use COUNT({arg}) because that will not count NULL values
                    // (which are produced by enum {None, Some: T} type).
                    // So we use IS NULL, which should be performant, but also always emit non-null values.
                    format!("COUNT({arg} IS NULL)")
                })
            }
            "std::any" => sql_ast::Expr::Source(format!(
                "COALESCE({}, FALSE)",
                utils::new_func_call("BOOL_OR", args)
            )),
            "std::all" => sql_ast::Expr::Source(format!(
                "COALESCE({}, TRUE)",
                utils::new_func_call("BOOL_AND", args)
            )),

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
            "std::text::from_ascii" => {
                let [ascii] = unpack_args(args);
                sql_ast::Expr::Source(format!("CHR({ascii})"))
            }
            "std::text::concat" => utils::new_bin_op("||", args),
            "std::text::join" => {
                let [parts, sep] = unpack_args(args);
                sql_ast::Expr::Source(format!("COALESCE(STRING_AGG({parts}, {sep}), '')"))
            }

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
        Node::from(expr)
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
                return sql_ast::Expr::Source(format!("'{escaped}'::text"));
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
