//! Utils for constructing SQL AST nodes

use std::sync::OnceLock;

use regex::Regex;
use sqlparser::ast as sql_ast;
use sqlparser::ast::helpers::attached_token::AttachedToken;

#[track_caller]
pub fn get_rel_alias(rel: &sqlparser::ast::TableFactor) -> &str {
    match rel {
        sql_ast::TableFactor::Table { alias, .. } => &alias.as_ref().unwrap().name.value,
        sql_ast::TableFactor::Derived { alias, .. } => &alias.as_ref().unwrap().name.value,
        sql_ast::TableFactor::TableFunction { alias, .. } => &alias.as_ref().unwrap().name.value,
        sql_ast::TableFactor::Function { alias, .. } => &alias.as_ref().unwrap().name.value,
        sql_ast::TableFactor::UNNEST { alias, .. } => &alias.as_ref().unwrap().name.value,
        sql_ast::TableFactor::JsonTable { alias, .. } => &alias.as_ref().unwrap().name.value,
        sql_ast::TableFactor::OpenJsonTable { alias, .. } => &alias.as_ref().unwrap().name.value,
        sql_ast::TableFactor::NestedJoin { alias, .. } => &alias.as_ref().unwrap().name.value,
        sql_ast::TableFactor::Pivot { alias, .. } => &alias.as_ref().unwrap().name.value,
        sql_ast::TableFactor::Unpivot { alias, .. } => &alias.as_ref().unwrap().name.value,
        sql_ast::TableFactor::MatchRecognize { alias, .. } => &alias.as_ref().unwrap().name.value,
    }
}

pub fn as_sub_rel(rel: &sqlparser::ast::TableFactor) -> Option<&sql_ast::Query> {
    match rel {
        sql_ast::TableFactor::Derived { subquery, .. } => Some(subquery),
        _ => None,
    }
}

pub fn as_mut_sub_rel(rel: &mut sqlparser::ast::TableFactor) -> Option<&mut sql_ast::Query> {
    match rel {
        sql_ast::TableFactor::Derived { subquery, .. } => Some(subquery),
        _ => None,
    }
}

pub fn new_table(name: Vec<String>, alias: Option<String>) -> sql_ast::TableFactor {
    sql_ast::TableFactor::Table {
        name: sql_ast::ObjectName(name.into_iter().map(new_ident).collect()),
        alias: alias.map(|a| sql_ast::TableAlias {
            name: new_ident(a),
            columns: vec![],
        }),
        args: Default::default(),
        with_hints: Default::default(),
        version: Default::default(),
        with_ordinality: Default::default(),
        partitions: Default::default(),
        json_path: Default::default(),
        sample: Default::default(),
    }
}

pub fn sub_rel(query: sql_ast::Query, alias: String) -> sql_ast::TableFactor {
    sql_ast::TableFactor::Derived {
        lateral: false,
        subquery: Box::new(query),
        alias: Some(sql_ast::TableAlias {
            name: new_ident(alias),
            columns: vec![],
        }),
    }
}

pub fn lateral(relation: sql_ast::TableFactor) -> sql_ast::TableFactor {
    match relation {
        sql_ast::TableFactor::Derived {
            lateral: _,
            subquery,
            alias,
        } => sql_ast::TableFactor::Derived {
            lateral: true,
            subquery,
            alias,
        },
        sql_ast::TableFactor::Function {
            lateral: _,
            name,
            args,
            alias,
        } => sql_ast::TableFactor::Function {
            lateral: true,
            name,
            args,
            alias,
        },
        _ => panic!("lateral of {relation:?}"),
    }
}

pub fn rel_func(
    name: sql_ast::Ident,
    args: Vec<sql_ast::Expr>,
    alias: Option<String>,
) -> sql_ast::TableFactor {
    sql_ast::TableFactor::Function {
        lateral: false,
        name: sql_ast::ObjectName(vec![name]),
        args: args
            .into_iter()
            .map(sql_ast::FunctionArgExpr::Expr)
            .map(sql_ast::FunctionArg::Unnamed)
            .collect(),
        alias: alias.map(|a| sql_ast::TableAlias {
            name: new_ident(a),
            columns: vec![],
        }),
    }
}

pub fn from(relation: sql_ast::TableFactor) -> sql_ast::TableWithJoins {
    sql_ast::TableWithJoins {
        relation,
        joins: Vec::new(),
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

pub fn query_into_set_expr(query: sql_ast::Query) -> sql_ast::SetExpr {
    if query.with.is_some()
        || query.order_by.is_some()
        || query.limit.is_some()
        || !query.limit_by.is_empty()
        || query.offset.is_some()
        || query.fetch.is_some()
        || !query.locks.is_empty()
        || query.for_clause.is_some()
    {
        return sql_ast::SetExpr::Query(Box::new(query));
    }
    *query.body
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

#[track_caller]
pub fn unwrap_select_item(item: sql_ast::SelectItem) -> sql_ast::Expr {
    match item {
        sql_ast::SelectItem::UnnamedExpr(e) => e,
        sql_ast::SelectItem::ExprWithAlias { expr, .. } => expr,
        sql_ast::SelectItem::QualifiedWildcard(_, _) => panic!(),
        sql_ast::SelectItem::Wildcard(_) => panic!(),
    }
}

pub fn new_expr(source: String) -> sql_ast::Expr {
    sql_ast::Expr::Identifier(new_ident(source))
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

pub fn identifier(first: Option<impl Into<String>>, second: impl Into<String>) -> sql_ast::Expr {
    if let Some(table) = first {
        sql_ast::Expr::CompoundIdentifier(vec![new_ident(table), new_ident(second)])
    } else {
        sql_ast::Expr::Identifier(new_ident(second.into()))
    }
}

pub fn func_call(
    func_name: impl Into<String>,
    args: impl IntoIterator<Item = sql_ast::Expr>,
) -> sql_ast::Expr {
    sql_ast::Expr::Function(sql_ast::Function {
        name: sql_ast::ObjectName(vec![new_ident(func_name)]),
        uses_odbc_syntax: Default::default(),
        parameters: sql_ast::FunctionArguments::None,
        args: sql_ast::FunctionArguments::List(sql_ast::FunctionArgumentList {
            duplicate_treatment: Default::default(),
            args: args
                .into_iter()
                .map(sql_ast::FunctionArgExpr::Expr)
                .map(sql_ast::FunctionArg::Unnamed)
                .collect(),
            clauses: Default::default(),
        }),
        filter: Default::default(),
        null_treatment: Default::default(),
        over: Default::default(),
        within_group: Default::default(),
    })
}

pub fn order_by_one(expr: sql_ast::Expr) -> sql_ast::OrderBy {
    sql_ast::OrderBy {
        exprs: vec![sql_ast::OrderByExpr {
            expr,
            asc: Default::default(),
            nulls_first: Default::default(),
            with_fill: Default::default(),
        }],
        interpolate: Default::default(),
    }
}

pub fn with() -> sql_ast::With {
    sql_ast::With {
        with_token: AttachedToken::empty(),
        recursive: false,
        cte_tables: Vec::new(),
    }
}

pub fn cte(name: String, val: sql_ast::Query) -> sql_ast::Cte {
    sql_ast::Cte {
        alias: sql_ast::TableAlias {
            name: new_ident(name),
            columns: Default::default(),
        },
        query: Box::new(val),
        from: Default::default(),
        materialized: Default::default(),
        closing_paren_token: AttachedToken::empty(),
    }
}

pub fn new_ident<S: Into<String>>(name: S) -> sql_ast::Ident {
    let name: String = name.into();

    if valid_ident_regex().is_match(&name) {
        sql_ast::Ident::new(name)
    } else {
        sql_ast::Ident::with_quote('"', name)
    }
}

pub(crate) fn valid_ident_regex() -> &'static Regex {
    static VALID_IDENT: OnceLock<Regex> = OnceLock::new();
    VALID_IDENT.get_or_init(|| {
        // One of:
        // - `*`
        // - An ident starting with `a-z_\$` and containing other characters `a-z0-9_\$`
        //
        // We could replace this with pomsky (regex<>pomsky : sql<>prql)
        // ^ ('*' | [ascii_lower '_$'] [ascii_lower ascii_digit '_$']* ) $
        Regex::new(r"^((\*)|(^[a-z_\$][a-z0-9_\$]*))$").unwrap()
    })
}
