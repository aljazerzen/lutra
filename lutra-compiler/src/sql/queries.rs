use std::collections::HashMap;
use std::rc::Rc;

use super::utils::{ExprOrSource, ExprScoped};
use super::COL_ARRAY_INDEX;
use super::{cr, utils};
use lutra_bin::ir;
use sqlparser::ast as sql_ast;

pub fn compile(rel: cr::RelExpr, types: HashMap<&ir::Path, &ir::Ty>) -> sql_ast::Query {
    let mut ctx = Context { types };

    let rel_ty = rel.ty.clone();
    let query = ctx.compile_re(rel);

    if rel_ty.kind.is_array() {
        // wrap into a top-level projection to remove array indexes
        let mut query = ctx.query_wrap(query, &rel_ty, false);
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
    pub(super) types: HashMap<&'a ir::Path, &'a ir::Ty>,
}

impl<'a> Context<'a> {
    pub(super) fn get_ty_mat(&self, ty: &'a ir::Ty) -> &'a ir::Ty {
        match &ty.kind {
            ir::TyKind::Ident(path) => self.types.get(path).unwrap(),
            _ => ty,
        }
    }

    fn compile_re(&mut self, rel: cr::RelExpr) -> sql_ast::Query {
        match rel.kind {
            cr::RelExprKind::Constructed(rows) => self.compile_rel_constructed(rows, &rel.ty),

            cr::RelExprKind::FromTable(table_name) => {
                let mut select = utils::select_empty();

                let name = sql_ast::ObjectName(vec![sql_ast::Ident::new(table_name)]);
                select.from = vec![sql_ast::TableWithJoins {
                    relation: utils::new_table(name, None),
                    joins: vec![],
                }];

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
            cr::RelExprKind::FromBinding(alias) => {
                let mut select = utils::select_empty();

                let name = sql_ast::ObjectName(vec![sql_ast::Ident::new(alias)]);

                select.from = vec![sql_ast::TableWithJoins {
                    relation: utils::new_table(name, None),
                    joins: vec![],
                }];
                select.projection = self.projection_noop(None, &rel.ty, true);
                utils::query_select(select)
            }
            cr::RelExprKind::SelectRelVar(rel_var_name) => {
                let mut select = utils::select_empty();
                select.projection = self.projection_noop(rel_var_name.as_deref(), &rel.ty, true);
                utils::query_select(select)
            }
            cr::RelExprKind::Limit(inner, limit) => {
                let mut inner = self.compile_re(*inner);
                if inner.limit.is_some() {
                    inner = self.query_wrap(inner, &rel.ty, true);
                }

                inner.limit = Some(self.compile_expr(limit).unwrap_inner().into_expr());
                if inner.order_by.is_none() {
                    inner.order_by = Some(utils::order_by_one(utils::ident(
                        None::<&str>,
                        COL_ARRAY_INDEX,
                    )));
                }
                inner
            }
            cr::RelExprKind::Offset(inner, offset) => {
                let mut inner = self.compile_re(*inner);

                if inner.limit.is_some() | inner.offset.is_some() {
                    inner = self.query_wrap(inner, &rel.ty, true);
                }

                inner.offset = Some(sql_ast::Offset {
                    value: self.compile_expr(offset).unwrap_inner().into_expr(),
                    rows: sql_ast::OffsetRows::None,
                });
                if inner.order_by.is_none() {
                    inner.order_by = Some(utils::order_by_one(utils::ident(
                        None::<&str>,
                        COL_ARRAY_INDEX,
                    )));
                }

                inner
            }
            cr::RelExprKind::ProjectRetain(inner, cols) => {
                let inner_ty = inner.ty.clone();
                let mut inner = self.compile_re(*inner);

                let select = self.query_as_mut_select(&mut inner, &inner_ty);
                utils::retain_by_position(&mut select.projection, cols);
                inner
            }
            cr::RelExprKind::ProjectDrop(inner, cols) => {
                let inner_ty = inner.ty.clone();
                let mut inner = self.compile_re(*inner);

                let select = self.query_as_mut_select(&mut inner, &inner_ty);
                utils::drop_by_position(&mut select.projection, cols);
                inner
            }
            cr::RelExprKind::ProjectReplace(inner, columns) => {
                let inner = self.compile_re(*inner);

                let mut select = utils::select_empty();
                select.from = utils::from(utils::subquery(inner, None));

                let (column_exprs, rvars) = self.compile_columns(columns);
                select.from.extend(self.compile_rvars(rvars));
                select.projection = self.projection(&rel.ty, column_exprs);

                utils::query_select(select)
            }
            cr::RelExprKind::Aggregate(inner, columns) => {
                let inner = self.compile_re(*inner);

                let mut select = utils::select_empty();
                select.from = utils::from(utils::subquery(inner, None));

                let columns: Vec<_> = columns
                    .into_iter()
                    .map(|x| self.compile_expr(x))
                    .map(|e| e.unwrap_inner().into_expr())
                    .collect();
                select.projection = self.projection(&rel.ty, columns);

                utils::query_select(select)
            }
            cr::RelExprKind::Where(inner, cond) => {
                let inner = self.compile_re(*inner);
                let mut query = self.query_wrap(inner, &rel.ty, true);
                let select = self.query_as_mut_select(&mut query, &rel.ty);
                select.selection = Some(self.compile_expr(cond).unwrap_inner().into_expr());
                query
            }
            cr::RelExprKind::OrderBy(inner, key) => {
                let inner = self.compile_re(*inner);

                // wrap into a new query
                let mut select = utils::select_empty();
                select.from = utils::from(utils::subquery(inner, None));
                select.projection = self.projection_noop(None, &rel.ty, true);

                // overwrite array index
                select.projection[0] = sql_ast::SelectItem::ExprWithAlias {
                    expr: self.compile_expr(key).unwrap_inner().into_expr(),
                    alias: sql_ast::Ident::new(COL_ARRAY_INDEX),
                };

                utils::query_select(select)
            }
            cr::RelExprKind::With(name, val, main) => {
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

    fn compile_rvars(&mut self, rvars: Vec<cr::RelVar>) -> Vec<sql_ast::TableWithJoins> {
        rvars
            .into_iter()
            .map(|rvar| self.compile_rvar(rvar))
            .collect()
    }

    fn compile_rel_constructed(&mut self, rows: Vec<Vec<cr::Expr>>, ty: &ir::Ty) -> sql_ast::Query {
        let mut res = None;
        let mut rows = rows.into_iter();

        // first row with aliases
        if let Some(row) = rows.next() {
            let mut select = utils::select_empty();
            let (columns, rvars) = self.compile_columns(row);
            select.from.extend(self.compile_rvars(rvars));
            select.projection = self.projection(ty, columns);
            res = Some(sql_ast::SetExpr::Select(Box::new(select)));
        }

        // all following rows
        for row in rows {
            let mut select = utils::select_empty();

            let (columns, rvars) = self.compile_columns(row);
            select.from.extend(self.compile_rvars(rvars));
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

    fn compile_columns(&mut self, columns: Vec<cr::Expr>) -> (Vec<sql_ast::Expr>, Vec<cr::RelVar>) {
        let mut column_exprs = Vec::with_capacity(columns.len());
        let mut rvars: Vec<std::rc::Rc<cr::RelVar>> = Vec::new();
        for col in columns {
            let scoped = self.compile_expr(col);
            column_exprs.push(scoped.inner.into_expr());
            if let Some(rvar) = scoped.rvar {
                if !rvars.iter().any(|r| Rc::ptr_eq(r, &rvar)) {
                    rvars.push(rvar);
                }
            }
        }
        let rvars = rvars
            .into_iter()
            .map(|r| Rc::try_unwrap(r).unwrap())
            .collect();
        (column_exprs, rvars)
    }

    fn compile_rvar(&mut self, rel_var: cr::RelVar) -> sql_ast::TableWithJoins {
        let subquery = self.compile_re(*rel_var.rel);
        let relation = sql_ast::TableFactor::Derived {
            lateral: false,
            subquery: Box::new(subquery),
            alias: Some(sql_ast::TableAlias {
                name: sql_ast::Ident::new(rel_var.alias),
                columns: Vec::new(),
            }),
        };
        sql_ast::TableWithJoins {
            relation,
            joins: Vec::new(),
        }
    }

    fn compile_expr(&mut self, expr: cr::Expr) -> ExprScoped {
        match expr.kind {
            cr::ExprKind::Null => ExprOrSource::Expr(self.null(&expr.ty)),
            cr::ExprKind::Literal(literal) => self.compile_literal(literal, &expr.ty),
            cr::ExprKind::FuncCall(func_name, args) => {
                let args: Vec<_> = args
                    .into_iter()
                    .map(|x| self.compile_expr(x).unwrap_inner())
                    .collect();
                self.compile_func_call(&func_name, expr.ty, args)
            }
            cr::ExprKind::Subquery(sub) => {
                // optimization: unwrap simple sub-queries
                if let cr::RelExprKind::ProjectRetain(inner, cols) = &sub.kind {
                    if let cr::RelExprKind::SelectRelVar(rvar_name) = &inner.kind {
                        if cols.len() == 1 {
                            let col = self.rel_cols(&inner.ty, true).nth(cols[0]).unwrap();
                            return ExprOrSource::Expr(utils::ident(rvar_name.as_deref(), col))
                                .into();
                        }
                    }
                }
                if let cr::RelExprKind::SelectRelVar(rvar_name) = &sub.kind {
                    if let ir::TyKind::Primitive(_) = sub.ty.kind {
                        let col = self.rel_cols(&sub.ty, true).next().unwrap();
                        return ExprOrSource::Expr(utils::ident(rvar_name.as_deref(), col)).into();
                    }
                }

                ExprOrSource::Expr(sql_ast::Expr::Subquery(Box::new(self.compile_re(*sub))))
            }
            cr::ExprKind::JsonPack(sub) => self.compile_json_pack(*sub, &expr.ty),
            cr::ExprKind::Scoped(rvar, inner) => {
                return ExprScoped {
                    inner: self.compile_expr(*inner).unwrap_inner(), // TODO: double nested Scoped?
                    rvar: Some(rvar),
                };
            }
            cr::ExprKind::Param(param_index) => ExprOrSource::Source(format!(
                "${}::{}",
                param_index + 1,
                self.compile_ty_name(&expr.ty)
            )),
        }
        .into()
    }

    fn compile_json_pack(&mut self, expr: cr::RelExpr, ty: &ir::Ty) -> ExprOrSource {
        match &self.get_ty_mat(&expr.ty).kind {
            ir::TyKind::Array(ty_item) => match &self.get_ty_mat(ty_item).kind {
                ir::TyKind::Primitive(_) => ExprOrSource::Source(format!(
                    "(SELECT json_agg(value ORDER BY index) FROM ({}))",
                    self.compile_re(expr)
                )),
                ir::TyKind::Tuple(_) => {
                    let rel = self.compile_re(expr);

                    let fields: Vec<_> = self.rel_cols(ty, false).collect();
                    let fields = fields.join(", ");

                    let objects =
                        format!("SELECT index, jsonb_build_array({fields}) as value FROM ({rel})",);

                    ExprOrSource::Source(format!(
                        "(SELECT json_agg(value ORDER BY index) FROM ({objects}))"
                    ))
                }
                ir::TyKind::Array(_) => todo!(),
                _ => todo!(),
            },
            ir::TyKind::Tuple(_) => {
                let rel = self.compile_re(expr);

                let fields: Vec<_> = self.rel_cols(ty, false).collect();
                let fields = fields.join(", ");

                ExprOrSource::Source(format!(
                    "(SELECT jsonb_build_array({fields}) as value FROM ({rel}))"
                ))
            }
            _ => unreachable!("{:?}", expr.ty),
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
