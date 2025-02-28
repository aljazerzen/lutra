use super::cr;
use lutra_bin::ir;

// struct Context {}

pub fn compile(program: &ir::Program) -> cr::RelExpr {
    let ir::ExprKind::Function(func) = &program.main.kind else {
        todo!("top-level: {:?}", program.main)
    };

    compile_rel(&func.body)
}

pub fn compile_rel(expr: &ir::Expr) -> cr::RelExpr {
    let kind = match &expr.kind {
        ir::ExprKind::Literal(_) => cr::RelExprKind::Literal(compile_expr(expr)),
        ir::ExprKind::Tuple(fields) => {
            let row = fields.iter().map(compile_expr).collect();
            cr::RelExprKind::Tuple(row)
        }
        ir::ExprKind::Array(items) => {
            let rows = items.iter().map(compile_tuple).collect();
            cr::RelExprKind::Array(rows)
        }

        ir::ExprKind::Call(call) => match &call.function.kind {
            ir::ExprKind::Pointer(ir::Pointer::External(ptr)) if ptr.id.starts_with("std::") => {
                compile_rel_std(expr)
            }
            ir::ExprKind::Pointer(ir::Pointer::External(ptr)) => {
                let is_table = call.function.ty.kind.as_function().map_or(false, |f| {
                    if !f.params.is_empty() {
                        return false;
                    }

                    f.body.kind.as_array().map_or(false, |a| a.kind.is_tuple())
                });

                if !is_table {
                    todo!("only supported external refs are table functions (no params, return array of tuples)");
                }

                cr::RelExprKind::From(ptr.id.clone())
            }
            ir::ExprKind::Pointer(_) => todo!(),
            ir::ExprKind::Literal(_) => todo!(),
            ir::ExprKind::Call(_) => todo!(),
            ir::ExprKind::Function(_) => todo!(),
            ir::ExprKind::Tuple(_) => todo!(),
            ir::ExprKind::Array(_) => todo!(),
            ir::ExprKind::TupleLookup(_) => todo!(),
            ir::ExprKind::Binding(_) => todo!(),
        },

        ir::ExprKind::Pointer(_) => todo!(),
        ir::ExprKind::Function(_) => todo!(),

        ir::ExprKind::TupleLookup(_) => todo!(),
        ir::ExprKind::Binding(_) => todo!(),
    };
    cr::RelExpr {
        kind,
        ty: expr.ty.clone(),
    }
}

pub fn compile_rel_std(expr: &ir::Expr) -> cr::RelExprKind {
    let ir::ExprKind::Call(call) = &expr.kind else {
        unreachable!()
    };

    let ir::ExprKind::Pointer(ir::Pointer::External(ptr)) = &call.function.kind else {
        unreachable!()
    };
    match ptr.id.as_str() {
        "std::slice" => {
            let array = compile_rel(&call.args[0]);
            let start = compile_expr(&call.args[1]);
            let end = compile_expr(&call.args[2]);

            let offset = cr::RelExpr {
                kind: cr::RelExprKind::Offset(Box::new(array), start.clone()),
                ty: expr.ty.clone(),
            };

            cr::RelExprKind::Limit(Box::new(offset), new_bin_op(end, "std::sub", start))
        }
        _ => todo!(),
    }
}

fn new_bin_op(left: cr::Expr, op: &str, right: cr::Expr) -> cr::Expr {
    cr::Expr::BinOp(Box::new(left), op.into(), Box::new(right))
}

/// Compiles an expression that can be placed into a list of columns.
/// It must have type of tuple.
pub fn compile_tuple(expr: &ir::Expr) -> Vec<cr::Expr> {
    match &expr.kind {
        ir::ExprKind::Literal(_) => unreachable!(),

        ir::ExprKind::Tuple(fields) => fields.iter().map(compile_expr).collect(),

        ir::ExprKind::Pointer(_) => todo!(),
        ir::ExprKind::Call(_) => todo!(),
        ir::ExprKind::Function(_) => todo!(),
        ir::ExprKind::Array(_) => todo!(),
        ir::ExprKind::TupleLookup(_) => todo!(),
        ir::ExprKind::Binding(_) => todo!(),
    }
}

pub fn compile_expr(expr: &ir::Expr) -> cr::Expr {
    match &expr.kind {
        ir::ExprKind::Pointer(_) => todo!(),
        ir::ExprKind::Literal(lit) => cr::Expr::Literal(lit.clone()),
        ir::ExprKind::Call(call) => match &call.function.kind {
            ir::ExprKind::Pointer(ir::Pointer::External(ptr)) if ptr.id.starts_with("std::") => {
                compile_expr_std(expr)
            }
            _ => todo!(),
        },
        ir::ExprKind::Function(_) => todo!(),
        ir::ExprKind::Tuple(_) => todo!(),
        ir::ExprKind::Array(_) => todo!(),
        ir::ExprKind::TupleLookup(_) => todo!(),
        ir::ExprKind::Binding(_) => todo!(),
    }
}

pub fn compile_expr_std(expr: &ir::Expr) -> cr::Expr {
    let ir::ExprKind::Call(call) = &expr.kind else {
        unreachable!()
    };

    let ir::ExprKind::Pointer(ir::Pointer::External(ptr)) = &call.function.kind else {
        unreachable!()
    };
    match ptr.id.as_str() {
        "std::mul" | "std::div" | "std::mod" | "std::add" | "std::sub" | "std::eq" | "std::ne"
        | "std::gt" | "std::lt" | "std::gte" | "std::lte" | "std::and" | "std::or" => {
            let left = compile_expr(&call.args[0]);
            let right = compile_expr(&call.args[1]);
            new_bin_op(left, &ptr.id, right)
        }
        _ => todo!(),
    }
}
