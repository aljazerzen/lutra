use super::utils::ExprOrSource;
use super::{cr, utils};
use super::{COL_ARRAY_INDEX, COL_VALUE};
use itertools::Itertools;
use lutra_bin::ir;
use sqlparser::ast as sql_ast;

pub fn compile(rel: cr::RelExpr) -> sql_ast::Query {
    let rel_ty = rel.ty.clone();
    let query = compile_re(rel);

    if rel_ty.kind.is_array() {
        // wrap into a top-level projection to remove array indexes
        let mut query = utils::query_wrap(query, &rel_ty, true);
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

        cr::RelExprKind::From(table_name) => {
            let mut select = utils::select_empty();

            let name = sql_ast::ObjectName(vec![sql_ast::Ident::new(table_name)]);
            let alias = Some(sql_ast::TableAlias {
                name: sql_ast::Ident::new("a"),
                columns: vec![],
            });

            select.from = vec![sql_ast::TableWithJoins {
                relation: utils::new_table(name, alias),
                joins: vec![],
            }];

            let ty_tuple = rel.ty.kind.as_array().unwrap();
            let ty_fields = ty_tuple.kind.as_tuple().unwrap();

            select.projection = Vec::with_capacity(1 + ty_fields.len());
            select.projection.push(sql_ast::SelectItem::ExprWithAlias {
                expr: utils::value(sql_ast::Value::Null),
                alias: sql_ast::Ident::new(COL_ARRAY_INDEX),
            });
            select
                .projection
                .extend(ty_fields.iter().enumerate().map(|(field_n, field)| {
                    sql_ast::SelectItem::ExprWithAlias {
                        expr: sql_ast::Expr::CompoundIdentifier(vec![
                            sql_ast::Ident::new("a"),
                            sql_ast::Ident::new(field.name.clone().unwrap()),
                        ]),
                        alias: sql_ast::Ident::new(format!("f_{field_n}")),
                    }
                }));
            utils::query_select(select)
        }
        cr::RelExprKind::Limit(inner, limit) => {
            let mut inner = compile_re(*inner);
            if inner.limit.is_some() {
                inner = utils::query_wrap(inner, &rel.ty, false);
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
                inner = utils::query_wrap(inner, &rel.ty, false);
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
        cr::RelExprKind::ProjectUnIndex(inner) => {
            let inner = compile_re(*inner);
            let mut inner = utils::query_wrap(inner, &rel.ty, true);
            inner.order_by = Some(utils::order_by_one(utils::ident(
                None::<&str>,
                COL_ARRAY_INDEX,
            )));
            inner
        }
    }
}

fn compile_rel_constructed(rows: Vec<Vec<cr::Expr>>, ty: &ir::Ty) -> sql_ast::Query {
    let mut res = None;
    let mut rows = rows.into_iter().enumerate();

    let represents_array = ty.kind.is_array();
    let item_ty = match &ty.kind {
        ir::TyKind::Primitive(_) | ir::TyKind::Tuple(_) => ty,
        ir::TyKind::Array(item_ty) => item_ty,
        ir::TyKind::Enum(_) | ir::TyKind::Function(_) | ir::TyKind::Ident(_) => todo!(),
    };

    // first row with aliases
    if let Some((order, row)) = rows.next() {
        let mut select = utils::select_empty();
        select.projection = Vec::with_capacity(1 + row.len());

        if represents_array {
            select.projection.push(sql_ast::SelectItem::ExprWithAlias {
                expr: utils::number(order.to_string()),
                alias: sql_ast::Ident::new(COL_ARRAY_INDEX),
            });
        }

        match &item_ty.kind {
            ir::TyKind::Primitive(_) => {
                let expr = row.into_iter().exactly_one().unwrap();
                let expr = compile_expr(expr).into_expr();

                select.projection.push(sql_ast::SelectItem::ExprWithAlias {
                    expr,
                    alias: sql_ast::Ident::new(COL_VALUE),
                });
            }
            ir::TyKind::Tuple(_) => {
                select.projection.extend(
                    row.into_iter()
                        .map(compile_expr)
                        .map(ExprOrSource::into_expr)
                        .enumerate()
                        .map(|(field_n, expr)| sql_ast::SelectItem::ExprWithAlias {
                            expr,
                            alias: sql_ast::Ident::new(format!("f_{field_n}")),
                        }),
                );
            }
            _ => todo!(),
        }

        res = Some(sql_ast::SetExpr::Select(Box::new(select)));
    }

    // all following rows
    for (order, row) in rows {
        let mut select = utils::select_empty();
        select.projection = Vec::with_capacity(1 + row.len());
        select
            .projection
            .push(sql_ast::SelectItem::UnnamedExpr(utils::number(
                order.to_string(),
            )));
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
        let mut select = utils::select_empty();
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
        _ => todo!("sql impl for {id}"),
    }
}
