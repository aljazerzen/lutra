use std::fmt::Write;

use crate::sql::utils;

#[derive(Debug, Clone)]
pub enum ExprOrRelVar {
    Expr(Box<sql_ast::Expr>),

    // Represents `rel_var.*`
    RelVar(String),
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

impl ExprOrRelVar {
    pub fn new(x: String) -> Self {
        ExprOrRelVar::Expr(Box::new(utils::new_expr(x)))
    }

    pub fn new_expr(expr: sql_ast::Expr) -> Self {
        Self::Expr(Box::new(expr))
    }

    pub fn as_rel_var(&self) -> Option<&str> {
        match self {
            ExprOrRelVar::Expr(_) => None,
            ExprOrRelVar::RelVar(var_name) => Some(var_name),
        }
    }

    pub fn into_expr(self) -> Result<sql_ast::Expr, ExprOrRelVar> {
        match self {
            ExprOrRelVar::Expr(expr) => Ok(*expr),
            ExprOrRelVar::RelVar(_) => Err(self),
        }
    }
}

pub struct ExprDisplay<'a> {
    pub exprs: &'a [sql_ast::Expr],
}

impl<'a> std::fmt::Display for ExprDisplay<'a> {
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
