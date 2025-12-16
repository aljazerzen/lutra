//! Utils for constructing SQL AST nodes

use std::sync::OnceLock;

use regex::Regex;

pub fn get_rel_alias(rel: &sql_ast::RelNamed) -> Option<&str> {
    rel.alias.as_ref().map(|r| r.name.value.as_str())
}

pub fn as_sub_rel(rel: &sql_ast::RelNamed) -> Option<&sql_ast::Query> {
    match &rel.expr {
        sql_ast::RelExpr::Subquery { subquery, .. } => Some(subquery),
        _ => None,
    }
}

pub fn as_mut_sub_rel(rel: &mut sql_ast::RelNamed) -> Option<&mut sql_ast::Query> {
    match &mut rel.expr {
        sql_ast::RelExpr::Subquery { subquery, .. } => Some(subquery),
        _ => None,
    }
}

pub fn new_table(name: sql_ast::ObjectName, alias: Option<String>) -> sql_ast::RelNamed {
    sql_ast::RelNamed {
        expr: sql_ast::RelExpr::Table { name },
        lateral: false,
        alias: alias.map(new_table_alias),
    }
}

pub fn sub_rel(query: sql_ast::Query, alias: String) -> sql_ast::RelNamed {
    sql_ast::RelNamed {
        lateral: false,
        expr: sql_ast::RelExpr::Subquery {
            subquery: Box::new(query),
        },
        alias: Some(new_table_alias(alias)),
    }
}

pub fn lateral(mut relation: sql_ast::RelNamed) -> sql_ast::RelNamed {
    relation.lateral = true;
    relation
}

pub fn rel_func(
    name: sql_ast::Ident,
    args: Vec<sql_ast::Expr>,
    alias: Option<String>,
) -> sql_ast::RelNamed {
    sql_ast::RelNamed {
        lateral: false,
        expr: sql_ast::RelExpr::Function {
            name: sql_ast::ObjectName(vec![name]),
            args: args.into_iter().collect(),
        },
        alias: alias.map(new_table_alias),
    }
}

pub fn select_empty() -> sql_ast::Select {
    sql_ast::Select {
        distinct: Default::default(),
        projection: Default::default(),
        into: Default::default(),
        from: Default::default(),
        selection: Default::default(),
        group_by: sql_ast::GroupByExpr::Expressions(vec![], vec![]),
        sort_by: Default::default(),
        having: Default::default(),
    }
}

pub fn select_from(rel: sql_ast::RelNamed) -> sql_ast::Select {
    sql_ast::Select {
        distinct: Default::default(),
        projection: Default::default(),
        into: Default::default(),
        from: vec![rel],
        selection: Default::default(),
        group_by: sql_ast::GroupByExpr::Expressions(vec![], vec![]),
        sort_by: Default::default(),
        having: Default::default(),
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
        || query.offset.is_some()
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
        offset: Default::default(),
    }
}

pub fn query_select(select: sql_ast::Select) -> sql_ast::Query {
    query_new(sql_ast::SetExpr::Select(Box::new(select)))
}

#[track_caller]
pub fn unwrap_select_item(item: sql_ast::SelectItem) -> sql_ast::Expr {
    item.expr
}

pub fn bool(value: bool) -> sql_ast::Expr {
    sql_ast::Expr::Source(if value { "TRUE" } else { "FALSE" }.to_string())
}

pub fn number(value: impl Into<String>) -> sql_ast::Expr {
    sql_ast::Expr::Source(value.into())
}

pub fn identifier(first: Option<impl Into<String>>, second: impl Into<String>) -> sql_ast::Expr {
    if let Some(table) = first {
        sql_ast::Expr::CompoundIdentifier(vec![new_ident(table), new_ident(second)])
    } else {
        sql_ast::Expr::Identifier(new_ident(second.into()))
    }
}

pub fn order_by_one(expr: sql_ast::Expr) -> sql_ast::OrderBy {
    sql_ast::OrderBy {
        exprs: vec![sql_ast::OrderByExpr {
            expr,
            options: sql_ast::OrderByOptions {
                asc: None,
                nulls_first: None,
            },
        }],
    }
}

pub fn with() -> sql_ast::With {
    sql_ast::With {
        recursive: false,
        cte_tables: Vec::new(),
    }
}

pub fn cte(name: String, val: sql_ast::Query) -> sql_ast::Cte {
    sql_ast::Cte {
        alias: new_table_alias(name),
        query: Box::new(val),
        materialized: Default::default(),
    }
}

pub fn new_index(order_by: Option<sql_ast::Expr>) -> sql_ast::Expr {
    sql_ast::Expr::Source(if let Some(order_by) = order_by {
        format!("(ROW_NUMBER() OVER (ORDER BY {order_by}) - 1)::int4")
    } else {
        "(ROW_NUMBER() OVER () - 1)::int4".into()
    })
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

pub fn new_object_name<S, I>(parts: I) -> sql_ast::ObjectName
where
    S: Into<String>,
    I: IntoIterator<Item = S>,
{
    sql_ast::ObjectName(parts.into_iter().map(new_ident).collect())
}

pub fn new_table_alias(name: impl Into<String>) -> sql_ast::TableAlias {
    sql_ast::TableAlias {
        name: new_ident(name),
        columns: vec![],
    }
}

pub fn new_bin_op(op: &str, args: impl IntoIterator<Item = sql_ast::Expr>) -> sql_ast::Expr {
    let mut args = args.into_iter();
    sql_ast::Expr::Source(format!(
        "({} {op} {})",
        args.next().unwrap(),
        args.next().unwrap(),
    ))
}

pub fn new_un_op(op: &str, args: impl IntoIterator<Item = sql_ast::Expr>) -> sql_ast::Expr {
    let mut args = args.into_iter();
    sql_ast::Expr::Source(format!("({op} {})", args.next().unwrap()))
}

pub fn new_func_call(
    func_name: &str,
    args: impl IntoIterator<Item = sql_ast::Expr>,
) -> sql_ast::Expr {
    let mut r = func_name.to_string();
    r += "(";
    for (index, arg) in args.into_iter().enumerate() {
        if index > 0 {
            r += ", ";
        }
        r += &arg.to_string();
    }
    r += ")";
    sql_ast::Expr::Source(r)
}
