use super::utils::ExprOrSource;
use super::COL_ARRAY_INDEX;
use super::{cr, utils};
use lutra_bin::ir;
use sqlparser::ast as sql_ast;

pub fn compile(rel: cr::RelExpr) -> sql_ast::Query {
    let rel_ty = rel.ty.clone();
    let query = compile_re(rel);

    if rel_ty.kind.is_array() {
        // wrap into a top-level projection to remove array indexes
        let mut query = utils::query_wrap(query, &rel_ty, false);
        query.order_by = Some(utils::order_by_one(utils::ident(
            None::<&str>,
            COL_ARRAY_INDEX,
        )));
        query
    } else {
        query
    }
}

fn compile_re(rel: cr::RelExpr) -> sql_ast::Query {
    match rel.kind {
        cr::RelExprKind::Constructed(rows) => compile_rel_constructed(rows, &rel.ty),

        cr::RelExprKind::FromTable(table_name) => {
            let mut select = utils::select_empty();

            let name = sql_ast::ObjectName(vec![sql_ast::Ident::new(table_name)]);
            select.from = vec![sql_ast::TableWithJoins {
                relation: utils::new_table(name, None),
                joins: vec![],
            }];

            let ty_tuple = rel.ty.kind.as_array().unwrap();
            let ty_fields = ty_tuple.kind.as_tuple().unwrap();

            let values = Some(utils::value(sql_ast::Value::Null)) // index
                .into_iter()
                .chain(
                    ty_fields
                        .iter()
                        .map(|field| utils::ident(None::<&str>, field.name.as_ref().unwrap())),
                );

            select.projection = utils::projection(&rel.ty, values);

            utils::query_select(select)
        }
        cr::RelExprKind::FromBinding(alias) => {
            let mut select = utils::select_empty();

            let name = sql_ast::ObjectName(vec![sql_ast::Ident::new(alias)]);

            select.from = vec![sql_ast::TableWithJoins {
                relation: utils::new_table(name, None),
                joins: vec![],
            }];
            select.projection = utils::projection_noop(None, &rel.ty, true);
            utils::query_select(select)
        }
        cr::RelExprKind::SelectRelVar => {
            let mut select = utils::select_empty();
            select.projection = utils::projection_noop(None, &rel.ty, true);
            utils::query_select(select)
        }
        cr::RelExprKind::Limit(inner, limit) => {
            let mut inner = compile_re(*inner);
            if inner.limit.is_some() {
                inner = utils::query_wrap(inner, &rel.ty, true);
            }

            inner.limit = Some(compile_expr(limit).into_expr());
            if inner.order_by.is_none() {
                inner.order_by = Some(utils::order_by_one(utils::ident(
                    None::<&str>,
                    COL_ARRAY_INDEX,
                )));
            }
            inner
        }
        cr::RelExprKind::Offset(inner, offset) => {
            let mut inner = compile_re(*inner);

            if inner.limit.is_some() | inner.offset.is_some() {
                inner = utils::query_wrap(inner, &rel.ty, true);
            }

            inner.offset = Some(sql_ast::Offset {
                value: compile_expr(offset).into_expr(),
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
            let mut inner = compile_re(*inner);

            let select = utils::query_as_mut_select(&mut inner, &inner_ty);
            utils::retain_by_position(&mut select.projection, cols);
            inner
        }
        cr::RelExprKind::ProjectDrop(inner, cols) => {
            let inner_ty = inner.ty.clone();
            let mut inner = compile_re(*inner);

            let select = utils::query_as_mut_select(&mut inner, &inner_ty);
            utils::drop_by_position(&mut select.projection, cols);
            inner
        }
        cr::RelExprKind::ProjectReplace(inner, columns) => {
            let inner = compile_re(*inner);

            let mut select = utils::select_empty();
            select.from = utils::from(utils::subquery(inner, None));

            let columns = columns.into_iter().map(compile_expr).map(|e| e.into_expr());
            select.projection = utils::projection(&rel.ty, columns);

            utils::query_select(select)
        }
        cr::RelExprKind::Aggregate(inner, columns) => {
            let inner = compile_re(*inner);

            let mut select = utils::select_empty();
            select.from = utils::from(utils::subquery(inner, None));

            let columns = columns.into_iter().map(compile_expr).map(|e| e.into_expr());
            select.projection = utils::projection(&rel.ty, columns);

            utils::query_select(select)
        }
        cr::RelExprKind::Where(inner, cond) => {
            let inner = compile_re(*inner);
            let mut query = utils::query_wrap(inner, &rel.ty, true);
            let select = utils::query_as_mut_select(&mut query, &rel.ty);
            select.selection = Some(compile_expr(cond).into_expr());
            query
        }
        cr::RelExprKind::OrderBy(inner, key) => {
            let inner = compile_re(*inner);

            // wrap into a new query
            let mut select = utils::select_empty();
            select.from = utils::from(utils::subquery(inner, None));
            select.projection = utils::projection_noop(None, &rel.ty, true);

            // overwrite array index
            select.projection[0] = sql_ast::SelectItem::ExprWithAlias {
                expr: compile_expr(key).into_expr(),
                alias: sql_ast::Ident::new(COL_ARRAY_INDEX),
            };

            utils::query_select(select)
        }
        cr::RelExprKind::With(name, val, main) => {
            let val = compile_re(*val);

            let mut main = compile_re(*main);

            if main.with.is_none() {
                main.with = Some(utils::with());
            }
            let ctes = &mut main.with.as_mut().unwrap().cte_tables;
            ctes.insert(0, utils::cte(name, val));

            main
        }
    }
}

fn compile_rel_constructed(rows: Vec<Vec<cr::Expr>>, ty: &ir::Ty) -> sql_ast::Query {
    let mut res = None;
    let mut rows = rows.into_iter();

    // first row with aliases
    if let Some(row) = rows.next() {
        let mut values = Vec::with_capacity(1 + row.len());

        values.extend(
            row.into_iter()
                .map(compile_expr)
                .map(ExprOrSource::into_expr),
        );

        let mut select = utils::select_empty();
        select.projection = utils::projection(ty, values);
        res = Some(sql_ast::SetExpr::Select(Box::new(select)));
    }

    // all following rows
    for row in rows {
        let mut select = utils::select_empty();
        select.projection = Vec::with_capacity(1 + row.len());
        select.projection.extend(
            row.into_iter()
                .map(compile_expr)
                .map(ExprOrSource::into_expr)
                .map(sql_ast::SelectItem::UnnamedExpr),
        );

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
        match &item_ty.kind {
            ir::TyKind::Primitive(_) => {
                values.push(null(item_ty));
            }
            ir::TyKind::Tuple(ty_fields) => {
                values.extend(ty_fields.iter().map(|ty_field| null(&ty_field.ty)));
            }
            _ => todo!(),
        }

        let mut select = utils::select_empty();
        select.projection = utils::projection(ty, values);
        select.selection = Some(utils::bool(false));
        sql_ast::SetExpr::Select(Box::new(select))
    }))
}

fn compile_expr(expr: cr::Expr) -> ExprOrSource {
    match expr.kind {
        cr::ExprKind::Literal(literal) => ExprOrSource::Expr(compile_literal(literal)),
        cr::ExprKind::FuncCall(func_name, args) => {
            let args = args.into_iter().map(compile_expr);
            compile_func_call(&func_name, expr.ty, args)
        }
        cr::ExprKind::Subquery(sub) => {
            // optimization: unwrap simple subqueries
            if let cr::RelExprKind::ProjectRetain(inner, cols) = &sub.kind {
                if let cr::RelExprKind::SelectRelVar = &inner.kind {
                    if cols.len() == 1 {
                        let col = utils::rel_cols(&inner.ty, true).nth(cols[0]).unwrap();
                        return ExprOrSource::Expr(utils::ident(None::<&str>, col));
                    }
                }
            }

            ExprOrSource::Expr(sql_ast::Expr::Subquery(Box::new(compile_re(*sub))))
        }
    }
}

fn compile_literal(lit: ir::Literal) -> sql_ast::Expr {
    sql_ast::Expr::Value(match lit {
        ir::Literal::Bool(b) => sql_ast::Value::Boolean(b),
        ir::Literal::Int(i) => sql_ast::Value::Number(format!("{i}::int8"), false),
        ir::Literal::Float(f) => sql_ast::Value::Number(format!("{f:?}::float8"), false),
        ir::Literal::Text(s) => sql_ast::Value::SingleQuotedString(s),
    })
}

fn compile_func_call(
    id: &str,
    ty: ir::Ty,
    args: impl IntoIterator<Item = ExprOrSource>,
) -> ExprOrSource {
    match id {
        "std::mul" => utils::new_bin_op("*", args),
        "std::div" => utils::new_bin_op("/", args),
        "std::mod" => match ty.kind.as_primitive().unwrap() {
            ir::PrimitiveSet::float32 | ir::PrimitiveSet::float64 => {
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

        "std::text_ops::length" => utils::new_func_call("LENGTH", args),

        "std::min" => utils::new_func_call("MIN", args),
        "std::max" => utils::new_func_call("MAX", args),
        "std::sum" => {
            let arg = args.into_iter().next().unwrap();
            let ty = compile_ty_name(&ty);
            ExprOrSource::Source(format!("COALESCE(SUM({arg}), 0)::{ty}"))
        }
        "std::average" => {
            let arg = args.into_iter().next().unwrap();
            let ty = compile_ty_name(&ty);
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

fn null(ty: &ir::Ty) -> sql_ast::Expr {
    sql_ast::Expr::Cast {
        kind: sql_ast::CastKind::DoubleColon,
        expr: Box::new(utils::value(sql_ast::Value::Null)),
        data_type: compile_ty_name(ty),
        format: None,
    }
}

fn compile_ty_name(ty: &ir::Ty) -> sql_ast::DataType {
    match ty.kind {
        ir::TyKind::Primitive(prim) => {
            let name = match prim {
                ir::PrimitiveSet::bool => "bool",
                ir::PrimitiveSet::int8 => "int1",
                ir::PrimitiveSet::int16 => "int2",
                ir::PrimitiveSet::int32 => "int4",
                ir::PrimitiveSet::int64 => "int8",
                ir::PrimitiveSet::uint8 => todo!(),
                ir::PrimitiveSet::uint16 => todo!(),
                ir::PrimitiveSet::uint32 => todo!(),
                ir::PrimitiveSet::uint64 => todo!(),
                ir::PrimitiveSet::float32 => "float4",
                ir::PrimitiveSet::float64 => "float8",
                ir::PrimitiveSet::text => "text",
            };
            sql_ast::DataType::Custom(sql_ast::ObjectName(vec![sql_ast::Ident::new(name)]), vec![])
        }
        ir::TyKind::Tuple(_) => todo!(),
        ir::TyKind::Array(_) => todo!(),
        ir::TyKind::Enum(_) => todo!(),
        ir::TyKind::Function(_) => todo!(),
        ir::TyKind::Ident(_) => todo!(),
    }
}
