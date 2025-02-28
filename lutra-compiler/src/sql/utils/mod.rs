#![allow(dead_code)]

mod projection;

#[allow(unused_imports)]
pub use projection::projection_for_ty;

use sqlparser::ast as sql_ast;
use sqlparser::ast::helpers::attached_token::AttachedToken;

pub fn new_table(
    name: sql_ast::ObjectName,
    alias: Option<sql_ast::TableAlias>,
) -> sql_ast::TableFactor {
    sql_ast::TableFactor::Table {
        name,
        alias,
        args: Default::default(),
        with_hints: Default::default(),
        version: Default::default(),
        with_ordinality: Default::default(),
        partitions: Default::default(),
        json_path: Default::default(),
        sample: Default::default(),
    }
}

pub fn select_empty() -> sql_ast::Select {
    sql_ast::Select {
        select_token: AttachedToken::empty(),
        distinct: Default::default(),
        top: Default::default(),
        top_before_distinct: Default::default(),
        projection: Default::default(),
        into: Default::default(),
        from: Default::default(),
        lateral_views: Default::default(),
        prewhere: Default::default(),
        selection: Default::default(),
        group_by: sql_ast::GroupByExpr::Expressions(vec![], vec![]),
        cluster_by: Default::default(),
        distribute_by: Default::default(),
        sort_by: Default::default(),
        having: Default::default(),
        named_window: Default::default(),
        qualify: Default::default(),
        window_before_qualify: Default::default(),
        value_table_mode: Default::default(),
        connect_by: Default::default(),
    }
}

pub fn union(left: sql_ast::SetExpr, right: sql_ast::SetExpr) -> sql_ast::SetExpr {
    sql_ast::SetExpr::SetOperation {
        op: sql_ast::SetOperator::Union,
        set_quantifier: sql_ast::SetQuantifier::All,
        left: Box::new(left),
        right: Box::new(right),
    }
}

pub fn query_new(set_expr: sql_ast::SetExpr) -> sql_ast::Query {
    sql_ast::Query {
        with: Default::default(),
        body: Box::new(set_expr),
        order_by: Default::default(),
        limit: Default::default(),
        limit_by: Default::default(),
        offset: Default::default(),
        fetch: Default::default(),
        locks: Default::default(),
        for_clause: Default::default(),
        settings: Default::default(),
        format_clause: Default::default(),
    }
}

pub fn query_select(select: sql_ast::Select) -> sql_ast::Query {
    query_new(sql_ast::SetExpr::Select(Box::new(select)))
}

pub fn new_expr(source: String) -> sql_ast::Expr {
    sql_ast::Expr::Identifier(sql_ast::Ident::new(source))
}

pub fn value(value: sql_ast::Value) -> sql_ast::Expr {
    sql_ast::Expr::Value(value)
}

pub fn bool(value: bool) -> sql_ast::Expr {
    sql_ast::Expr::Value(sql_ast::Value::Boolean(value))
}

pub fn number(value: impl Into<String>) -> sql_ast::Expr {
    sql_ast::Expr::Value(sql_ast::Value::Number(value.into(), false))
}
