use std::fmt::Write;

use sqlparser::ast as sql_ast;

use crate::sql::utils;

#[derive(Debug, Clone)]
pub enum ExprOrSource {
    Expr(Box<sql_ast::Expr>),
    Source(String),

    // Represents `rel_var.*`
    RelVar(String),
}

pub fn new_bin_op(op: &str, args: impl IntoIterator<Item = ExprOrSource>) -> ExprOrSource {
    let mut args = args.into_iter();
    ExprOrSource::Source(format!(
        "({} {op} {})",
        args.next().unwrap(),
        args.next().unwrap(),
    ))
}

pub fn new_un_op(op: &str, args: impl IntoIterator<Item = ExprOrSource>) -> ExprOrSource {
    let mut args = args.into_iter();
    ExprOrSource::Source(format!("({op} {})", args.next().unwrap()))
}

pub fn new_func_call(
    func_name: &str,
    args: impl IntoIterator<Item = ExprOrSource>,
) -> ExprOrSource {
    let mut r = func_name.to_string();
    r += "(";
    for (index, arg) in args.into_iter().enumerate() {
        if index > 0 {
            r += ", ";
        }
        r += &arg.to_string();
    }
    r += ")";
    ExprOrSource::Source(r)
}

impl ExprOrSource {
    pub fn new_expr(expr: sql_ast::Expr) -> Self {
        Self::Expr(Box::new(expr))
    }

    pub fn as_rel_var(&self) -> Option<&str> {
        match self {
            ExprOrSource::Expr(_) => None,
            ExprOrSource::Source(_) => None,
            ExprOrSource::RelVar(var_name) => Some(var_name),
        }
    }

    pub fn into_expr(self) -> sql_ast::Expr {
        match self {
            ExprOrSource::Expr(expr) => *expr,

            // hack to get SQL source into sql_ast::Expr, without too much overhead
            ExprOrSource::Source(source) => sql_ast::Expr::Identifier(sql_ast::Ident::new(source)),

            ExprOrSource::RelVar(rvar_name) => sql_ast::Expr::CompoundIdentifier(vec![
                utils::new_ident(rvar_name),
                utils::new_ident("value"),
            ]),
        }
    }
}

impl std::fmt::Display for ExprOrSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprOrSource::Expr(e) => e.fmt(f),
            ExprOrSource::Source(s) => f.write_str(s),
            ExprOrSource::RelVar(s) => {
                f.write_str(s)?;
                f.write_str(".value")
            }
        }
    }
}

pub struct ExprOrSourceDisplay<'a> {
    pub exprs: &'a [ExprOrSource],
}

impl<'a> std::fmt::Display for ExprOrSourceDisplay<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("[\n")?;
        for e in self.exprs {
            f.write_str("  ")?;
            e.fmt(f)?;
            f.write_str(",\n")?;
        }
        f.write_char(']')?;
        Ok(())
    }
}
