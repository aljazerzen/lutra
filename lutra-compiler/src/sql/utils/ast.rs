//! Utils for constructing SQL AST nodes

use lutra_sql as sa;

// TODO: these things should probably be moved into sql-ast crate

pub fn get_rel_alias(rel: &sa::RelNamed) -> Option<&str> {
    rel.alias.as_ref().map(|r| r.name.value.as_str())
}

pub fn as_sub_rel(rel: &sa::RelNamed) -> Option<&sa::Query> {
    match &rel.expr {
        sa::RelExpr::Subquery(subquery) => Some(subquery),
        _ => None,
    }
}

pub fn as_mut_sub_rel(rel: &mut sa::RelNamed) -> Option<&mut sa::Query> {
    match &mut rel.expr {
        sa::RelExpr::Subquery(subquery) => Some(subquery),
        _ => None,
    }
}

pub fn expr_to_subquery(expr: sa::Expr) -> sa::RelExpr {
    let mut select = select_empty();
    select.projection.push(sa::SelectItem { expr, alias: None });

    sa::RelExpr::Subquery(Box::new(query_select(select)))
}

pub fn lateral(mut relation: sa::RelNamed) -> sa::RelNamed {
    relation.lateral = true;
    relation
}

pub fn rel_func(name: sa::Ident, args: Vec<sa::Expr>, alias: Option<String>) -> sa::RelNamed {
    sa::RelNamed {
        lateral: false,
        expr: sa::RelExpr::Function {
            name: sa::ObjectName(vec![name]),
            args: args.into_iter().collect(),
            ordinality: false,
        },
        alias: alias.map(new_table_alias),
    }
}

pub fn select_empty() -> sa::Select {
    sa::Select {
        from: Default::default(),
        selection: Default::default(),
        group_by: Default::default(),
        distinct: Default::default(),
        projection: Default::default(),
        having: Default::default(),
    }
}

pub fn select_from(rel: sa::RelNamed) -> sa::Select {
    sa::Select {
        from: vec![rel],
        selection: Default::default(),
        group_by: Default::default(),
        distinct: Default::default(),
        projection: Default::default(),
        having: Default::default(),
    }
}

pub fn union(left: sa::SetExpr, right: sa::SetExpr) -> sa::SetExpr {
    sa::SetExpr::SetOperation {
        op: sa::SetOperator::Union,
        set_quantifier: sa::SetQuantifier::All,
        left: Box::new(left),
        right: Box::new(right),
    }
}

pub fn query_into_set_expr(query: sa::Query) -> sa::SetExpr {
    if query.with.is_some()
        || query.order_by.is_some()
        || query.limit.is_some()
        || query.offset.is_some()
    {
        return sa::SetExpr::Query(Box::new(query));
    }
    *query.body
}

pub fn query_new(set_expr: sa::SetExpr) -> sa::Query {
    sa::Query {
        with: Default::default(),
        body: Box::new(set_expr),
        order_by: Default::default(),
        limit: Default::default(),
        offset: Default::default(),
    }
}

pub fn query_select(select: sa::Select) -> sa::Query {
    query_new(sa::SetExpr::Select(Box::new(select)))
}

#[track_caller]
pub fn unwrap_select_item(item: sa::SelectItem) -> sa::Expr {
    item.expr
}

pub fn bool(value: bool) -> sa::Expr {
    sa::Expr::Source(if value { "TRUE" } else { "FALSE" }.to_string())
}

pub fn number(value: impl Into<String>) -> sa::Expr {
    sa::Expr::Source(value.into())
}

pub fn identifier(first: Option<impl Into<String>>, second: impl Into<String>) -> sa::Expr {
    if let Some(table) = first {
        sa::Expr::CompoundIdentifier(vec![new_ident(table), new_ident(second)])
    } else {
        sa::Expr::Identifier(new_ident(second.into()))
    }
}

pub fn order_by_one(expr: sa::Expr) -> Option<sa::OrderBy> {
    let keys = if let sa::Expr::IndexBy(keys) = expr {
        if keys.is_empty() {
            return None; // we are ordering by an arbitrary index
        };
        keys
    } else {
        vec![expr]
    };
    if let Some(sa::Expr::Source(s)) = keys.first()
        && s == "0"
    {
        return None; // we are ordering by a constant index (only one row)
    }

    Some(sa::OrderBy {
        exprs: keys
            .into_iter()
            .map(|key| sa::OrderByExpr {
                expr: key,
                options: sa::OrderByOptions {
                    asc: None,
                    nulls_first: None,
                },
            })
            .collect(),
    })
}

pub fn with() -> sa::With {
    sa::With {
        recursive: false,
        cte_tables: Vec::new(),
    }
}

pub fn cte(name: String, val: sa::Query) -> sa::Cte {
    sa::Cte {
        alias: new_table_alias(name),
        query: Box::new(val),
        materialized: Default::default(),
    }
}

pub fn new_ident<S: Into<String>>(name: S) -> sa::Ident {
    sa::Ident::with_quote_if_needed('"', name)
}

pub fn new_object_name<S, I>(parts: I) -> sa::ObjectName
where
    S: Into<String>,
    I: IntoIterator<Item = S>,
{
    sa::ObjectName(parts.into_iter().map(new_ident).collect())
}

pub fn new_table_alias(name: impl Into<String>) -> sa::TableAlias {
    sa::TableAlias::simple(new_ident(name))
}

pub fn new_bin_op(op: &str, args: impl IntoIterator<Item = sa::Expr>) -> sa::Expr {
    let mut args = args.into_iter();
    sa::Expr::Source(format!(
        "({} {op} {})",
        args.next().unwrap(),
        args.next().unwrap(),
    ))
}

pub fn new_un_op(op: &str, args: impl IntoIterator<Item = sa::Expr>) -> sa::Expr {
    let mut args = args.into_iter();
    sa::Expr::Source(format!("({op} {})", args.next().unwrap()))
}

pub fn new_func_call(func_name: &str, args: impl IntoIterator<Item = sa::Expr>) -> sa::Expr {
    let mut r = func_name.to_string();
    r += "(";
    for (index, arg) in args.into_iter().enumerate() {
        if index > 0 {
            r += ", ";
        }
        r += &arg.to_string();
    }
    r += ")";
    sa::Expr::Source(r)
}

pub fn set_or_bin_op(target: &mut Option<sa::Expr>, op: &str, value: sa::Expr) {
    *target = Some(if let Some(existing) = target.take() {
        new_bin_op(op, [existing, value])
    } else {
        value
    });
}
