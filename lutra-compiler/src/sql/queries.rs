use super::cr;
use super::utils;
use super::COL_ARRAY_INDEX;
use super::COL_VALUE;
use lutra_bin::ir;
use sqlparser::ast as sql_ast;

pub fn compile(rel: cr::RelExpr) -> sql_ast::Query {
    match rel.kind {
        cr::RelExprKind::Literal(expr) => {
            let mut select = utils::select_empty();

            let value = sql_ast::SelectItem::ExprWithAlias {
                expr: compile_expr(expr),
                alias: sql_ast::Ident::new(COL_VALUE),
            };
            select.projection = vec![value];

            utils::query_select(select)
        }
        cr::RelExprKind::Tuple(exprs) => {
            let mut select = utils::select_empty();
            select.projection = exprs
                .into_iter()
                .enumerate()
                .map(|(index, expr)| sql_ast::SelectItem::ExprWithAlias {
                    expr: compile_expr(expr),
                    alias: sql_ast::Ident::new(format!("f_{index}")),
                })
                .collect();

            utils::query_select(select)
        }
        cr::RelExprKind::Array(rows) => {
            let mut res = None;
            let mut rows = rows.into_iter().enumerate();

            // first row with aliases
            if let Some((order, row)) = rows.next() {
                let mut select = utils::select_empty();
                select.projection = Vec::with_capacity(1 + row.len());
                select.projection.push(sql_ast::SelectItem::ExprWithAlias {
                    expr: utils::number(order.to_string()),
                    alias: sql_ast::Ident::new(COL_ARRAY_INDEX),
                });
                select
                    .projection
                    .extend(row.into_iter().map(compile_expr).enumerate().map(
                        |(field_n, expr)| sql_ast::SelectItem::ExprWithAlias {
                            expr,
                            alias: sql_ast::Ident::new(format!("f_{field_n}")),
                        },
                    ));

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
            let mut inner = compile(*inner);
            // TODO: wrap if limit cannot be applied to the query
            inner.limit = Some(compile_expr(limit));
            inner
        }
        cr::RelExprKind::Offset(inner, offset) => {
            let mut inner = compile(*inner);

            // TODO: wrap if limit cannot be applied to the query

            inner.offset = Some(sql_ast::Offset {
                value: compile_expr(offset),
                rows: sql_ast::OffsetRows::None,
            });

            inner
        }
    }
}

fn compile_expr(expr: cr::Expr) -> sql_ast::Expr {
    match expr {
        cr::Expr::Literal(literal) => compile_literal(literal),
        cr::Expr::BinOp(left, op_id, right) => {
            sql_ast::Expr::Nested(Box::new(sql_ast::Expr::BinaryOp {
                left: Box::new(compile_expr(*left)),
                op: compile_bin_op(&op_id),
                right: Box::new(compile_expr(*right)),
            }))
        }
    }
}

fn compile_literal(lit: ir::Literal) -> sql_ast::Expr {
    sql_ast::Expr::Value(match lit {
        ir::Literal::Bool(b) => sql_ast::Value::Boolean(b),
        ir::Literal::Int(i) => sql_ast::Value::Number(format!("{i}"), false),
        ir::Literal::Float(f) => sql_ast::Value::Number(format!("{f:?}"), false),
        ir::Literal::Text(s) => sql_ast::Value::SingleQuotedString(s),
    })
}

fn compile_bin_op(id: &str) -> sql_ast::BinaryOperator {
    match id {
        "std::mul" => sql_ast::BinaryOperator::Multiply,
        "std::div" => sql_ast::BinaryOperator::Divide,
        "std::mod" => sql_ast::BinaryOperator::Modulo,
        "std::add" => sql_ast::BinaryOperator::Plus,
        "std::sub" => sql_ast::BinaryOperator::Minus,
        "std::eq" => sql_ast::BinaryOperator::Eq,
        "std::ne" => sql_ast::BinaryOperator::NotEq,
        "std::gt" => sql_ast::BinaryOperator::Gt,
        "std::lt" => sql_ast::BinaryOperator::Lt,
        "std::gte" => sql_ast::BinaryOperator::GtEq,
        "std::lte" => sql_ast::BinaryOperator::LtEq,
        "std::and" => sql_ast::BinaryOperator::And,
        "std::or" => sql_ast::BinaryOperator::Or,
        _ => todo!(),
    }
}
