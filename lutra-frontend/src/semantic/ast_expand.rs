use itertools::Itertools;

use crate::ir::pl::{self, new_binop};
use crate::pr;
use crate::semantic::{NS_THAT, NS_THIS};
use crate::{Error, Result};
use lutra_parser::generic;

/// An AST pass that maps AST to PL.
pub fn expand_expr(expr: pr::Expr) -> Result<pl::Expr> {
    let kind = match expr.kind {
        pr::ExprKind::Ident(v) => pl::ExprKind::Ident(pr::Ident::from_name(v)),
        pr::ExprKind::Indirection {
            base,
            field: pr::IndirectionKind::Name(field),
        } => pl::ExprKind::Indirection {
            base: expand_expr_box(base)?,
            field: pl::IndirectionKind::Name(field),
        },
        pr::ExprKind::Indirection {
            base,
            field: pr::IndirectionKind::Position(field),
        } => pl::ExprKind::Indirection {
            base: expand_expr_box(base)?,
            field: pl::IndirectionKind::Position(field),
        },
        pr::ExprKind::Indirection {
            base,
            field: pr::IndirectionKind::Star,
        } => pl::ExprKind::All {
            within: expand_expr_box(base)?,
            except: Box::new(pl::Expr::new(pl::ExprKind::Tuple(vec![]))),
        },

        pr::ExprKind::Literal(v) => pl::ExprKind::Literal(v),
        pr::ExprKind::Pipeline(v) => {
            let mut e = desugar_pipeline(v)?;
            e.alias = expr.alias.or(e.alias);
            return Ok(e);
        }
        pr::ExprKind::Tuple(mut v) => {
            // maybe extract last element for unpacking
            let mut last_unpacking = None;
            if v.last()
                .and_then(|v| v.kind.as_range())
                .map_or(false, |x| x.start.is_none() && x.end.is_some())
            {
                last_unpacking = Some(v.pop().unwrap().kind.into_range().unwrap().end.unwrap());
            }

            let mut fields = expand_exprs(v)?;

            if let Some(last) = last_unpacking {
                let mut last = expand_expr(*last)?;
                last.flatten = true;
                fields.push(last);
            }
            pl::ExprKind::Tuple(fields)
        }
        pr::ExprKind::Array(v) => pl::ExprKind::Array(expand_exprs(v)?),

        pr::ExprKind::Range(v) => expands_range(v)?,

        pr::ExprKind::Unary(unary) => expand_unary(unary)?,
        pr::ExprKind::Binary(binary) => expand_binary(binary)?,

        pr::ExprKind::FuncCall(v) => pl::ExprKind::FuncCall(pl::FuncCall {
            name: expand_expr_box(v.name)?,
            args: expand_exprs(v.args)?,
            named_args: v
                .named_args
                .into_iter()
                .map(|(k, v)| -> Result<_> { Ok((k, expand_expr(v)?)) })
                .try_collect()?,
        }),
        pr::ExprKind::Func(v) => pl::ExprKind::Func(
            pl::Func {
                return_ty: v.return_ty,
                body: expand_expr_box(v.body)?,
                params: expand_func_params(v.params)?,
                named_params: expand_func_params(v.named_params)?,
                generic_type_params: v.generic_type_params,
            }
            .into(),
        ),
        pr::ExprKind::SString(v) => pl::ExprKind::SString(
            v.into_iter()
                .map(|v| v.try_map(expand_expr))
                .try_collect()?,
        ),
        pr::ExprKind::FString(v) => pl::ExprKind::FString(
            v.into_iter()
                .map(|v| v.try_map(expand_expr))
                .try_collect()?,
        ),
        pr::ExprKind::Case(v) => pl::ExprKind::Case(
            v.into_iter()
                .map(|case| -> Result<_> {
                    Ok(pl::SwitchCase {
                        condition: expand_expr_box(case.condition)?,
                        value: expand_expr_box(case.value)?,
                    })
                })
                .try_collect()?,
        ),
        pr::ExprKind::Param(v) => pl::ExprKind::Param(v),
        pr::ExprKind::Internal(v) => pl::ExprKind::Internal(v),
    };

    Ok(pl::Expr {
        kind,
        span: expr.span,
        alias: expr.alias,
        id: None,
        target_id: None,
        ty: None,
        needs_window: false,
        flatten: false,
    })
}

/// De-sugars range `a..b` into `{start=a, end=b}`.
///
/// TODO: Open bounds are mapped into `null`.
fn expands_range(v: generic::Range<Box<pr::Expr>>) -> Result<pl::ExprKind> {
    let mut start = v
        .start
        .map(|e| expand_expr(*e))
        .transpose()?
        .unwrap_or_else(|| pl::Expr::new(pr::Literal::Integer(0))); // TODO
    start.alias = Some("start".into());
    let mut end = v
        .end
        .map(|e| expand_expr(*e))
        .transpose()?
        .unwrap_or_else(|| pl::Expr::new(pr::Literal::Integer(10000))); // TODO
    end.alias = Some("end".into());
    Ok(pl::ExprKind::Tuple(vec![start, end]))
}

fn expand_exprs(exprs: Vec<pr::Expr>) -> Result<Vec<pl::Expr>> {
    exprs.into_iter().map(expand_expr).collect()
}

#[allow(clippy::boxed_local)]
fn expand_expr_box(expr: Box<pr::Expr>) -> Result<Box<pl::Expr>> {
    Ok(Box::new(expand_expr(*expr)?))
}

fn desugar_pipeline(mut pipeline: pr::Pipeline) -> Result<pl::Expr> {
    let value = pipeline.exprs.remove(0);
    let mut value = expand_expr(value)?;

    for expr in pipeline.exprs {
        let expr = expand_expr(expr)?;
        let span = expr.span;

        value = pl::Expr::new(pl::ExprKind::FuncCall(pl::FuncCall::new_simple(
            expr,
            vec![value],
        )));
        value.span = span;
    }

    Ok(value)
}

/// Desugar unary operators into function calls.
fn expand_unary(pr::UnaryExpr { op, expr }: pr::UnaryExpr) -> Result<pl::ExprKind> {
    use pr::UnOp::*;

    let expr = expand_expr(*expr)?;

    let func_name = match op {
        Neg => ["std", "neg"],
        Not => ["std", "not"],
        Add => return Ok(expr.kind),
        EqSelf => {
            let pl::ExprKind::Ident(ident) = expr.kind else {
                return Err(Error::new_simple(
                    "you can only use column names with self-equality operator",
                ));
            };
            if !ident.path.is_empty() {
                return Err(Error::new_simple(
                    "you cannot use namespace prefix with self-equality operator",
                ));
            }

            let left = pl::Expr {
                span: expr.span,
                ..pl::Expr::new(pr::Ident {
                    path: vec![NS_THIS.to_string()],
                    name: ident.name.clone(),
                })
            };
            let right = pl::Expr {
                span: expr.span,
                ..pl::Expr::new(pr::Ident {
                    path: vec![NS_THAT.to_string()],
                    name: ident.name,
                })
            };
            return Ok(new_binop(left, &["std", "eq"], right).kind);
        }
    };
    Ok(pl::ExprKind::FuncCall(pl::FuncCall::new_simple(
        pl::Expr::new(pr::Ident::from_path(func_name.to_vec())),
        vec![expr],
    )))
}

/// Desugar binary operators into function calls.
fn expand_binary(pr::BinaryExpr { op, left, right }: pr::BinaryExpr) -> Result<pl::ExprKind> {
    let left = expand_expr(*left)?;
    let right = expand_expr(*right)?;

    let func_name: Vec<&str> = match op {
        pr::BinOp::Mul => vec!["std", "mul"],
        pr::BinOp::DivInt => vec!["std", "div_i"],
        pr::BinOp::DivFloat => vec!["std", "div_f"],
        pr::BinOp::Mod => vec!["std", "mod"],
        pr::BinOp::Pow => vec!["std", "math", "pow"],
        pr::BinOp::Add => vec!["std", "add"],
        pr::BinOp::Sub => vec!["std", "sub"],
        pr::BinOp::Eq => vec!["std", "eq"],
        pr::BinOp::Ne => vec!["std", "ne"],
        pr::BinOp::Gt => vec!["std", "gt"],
        pr::BinOp::Lt => vec!["std", "lt"],
        pr::BinOp::Gte => vec!["std", "gte"],
        pr::BinOp::Lte => vec!["std", "lte"],
        pr::BinOp::RegexSearch => vec!["std", "regex_search"],
        pr::BinOp::And => vec!["std", "and"],
        pr::BinOp::Or => vec!["std", "or"],
        pr::BinOp::Coalesce => vec!["std", "coalesce"],
    };

    // For the power operator, we need to reverse the order, since `math.pow a
    // b` is equivalent to `b ** a`. (but for example `sub a b` is equivalent to
    // `a - b`).
    //
    // (I think this is the most globally consistent approach, since final
    // arguments should be the "data", which in the case of `pow` would be the
    // base; but it's not perfect, we could change it...)
    let (left, right) = match op {
        pr::BinOp::Pow => (right, left),
        _ => (left, right),
    };
    Ok(new_binop(left, &func_name, right).kind)
}

fn expand_func_param(value: pr::FuncParam) -> Result<pl::FuncParam> {
    Ok(pl::FuncParam {
        name: value.name,
        ty: value.ty,
        default_value: value.default_value.map(expand_expr_box).transpose()?,
    })
}

fn expand_func_params(value: Vec<pr::FuncParam>) -> Result<Vec<pl::FuncParam>> {
    value.into_iter().map(expand_func_param).collect()
}

fn expand_stmt(value: pr::Stmt) -> Result<pl::Stmt> {
    Ok(pl::Stmt {
        id: None,
        kind: expand_stmt_kind(value.kind)?,
        span: value.span,
        annotations: value
            .annotations
            .into_iter()
            .map(expand_annotation)
            .try_collect()?,
    })
}

pub fn expand_module_def(v: pr::ModuleDef) -> Result<pl::ModuleDef> {
    Ok(pl::ModuleDef {
        name: v.name,
        stmts: expand_stmts(v.stmts)?,
    })
}

fn expand_stmts(value: Vec<pr::Stmt>) -> Result<Vec<pl::Stmt>> {
    value.into_iter().map(expand_stmt).collect()
}

fn expand_stmt_kind(value: pr::StmtKind) -> Result<pl::StmtKind> {
    Ok(match value {
        pr::StmtKind::VarDef(v) => pl::StmtKind::VarDef(pl::VarDef {
            name: v.name,
            value: v.value.map(expand_expr_box).transpose()?,
            ty: v.ty,
        }),
        pr::StmtKind::TypeDef(v) => pl::StmtKind::TypeDef(pl::TypeDef {
            name: v.name,
            value: v.value,
        }),
        pr::StmtKind::ModuleDef(v) => pl::StmtKind::ModuleDef(expand_module_def(v)?),
        pr::StmtKind::ImportDef(v) => pl::StmtKind::ImportDef(pl::ImportDef {
            alias: v.alias,
            name: v.name,
        }),
    })
}

fn expand_annotation(value: pr::Annotation) -> Result<pl::Annotation> {
    Ok(pl::Annotation {
        expr: expand_expr_box(value.expr)?,
    })
}
