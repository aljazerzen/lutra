use lutra_bin::ir;

use crate::sql::cr::{self, CrFold};

pub fn optimize(expr: cr::Expr) -> cr::Expr {
    Optimizer {}.fold_expr(expr).unwrap()
}

struct Optimizer {}

impl cr::CrFold for Optimizer {
    fn fold_from(&mut self, from: cr::From, ty: ir::Ty) -> Result<cr::Expr, ()> {
        match from {
            // cr::From::Row(cols) if cols.len() == 1 => Ok(cols.into_iter().next().unwrap()),
            cr::From::JsonUnpack(expr) => {
                if let cr::ExprKind::From(cr::From::JsonPack(inner)) = expr.kind {
                    // JsonUnpack(JsonPack(x)) is simplified into x
                    Ok(*inner)
                } else {
                    // normal flow
                    cr::fold_from(self, cr::From::JsonUnpack(expr), ty)
                }
            }
            cr::From::JsonPack(expr) => {
                if let cr::ExprKind::From(cr::From::JsonUnpack(inner)) = expr.kind {
                    // JsonPack(JsonUnpack(x)) is simplified into x
                    Ok(*inner)
                } else {
                    // normal flow
                    cr::fold_from(self, cr::From::JsonPack(expr), ty)
                }
            }
            _ => cr::fold_from(self, from, ty),
        }
    }
}
