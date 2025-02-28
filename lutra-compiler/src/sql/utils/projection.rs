use lutra_bin::ir;
use sqlparser::ast as sql_ast;

use crate::sql::{COL_ARRAY_INDEX, COL_VALUE};

pub fn projection_for_ty(rel_var_name: &str, ty: &ir::Ty) -> Vec<sql_ast::SelectItem> {
    let mut r = Vec::new();
    projection_for_ty_re(rel_var_name, ty, &mut r);
    r
}

fn projection_for_ty_re(rel_var_name: &str, ty: &ir::Ty, r: &mut Vec<sql_ast::SelectItem>) {
    match &ty.kind {
        ir::TyKind::Primitive(_) => {
            r.push(sql_ast::SelectItem::UnnamedExpr(
                sql_ast::Expr::CompoundIdentifier(vec![
                    sql_ast::Ident::new(rel_var_name),
                    sql_ast::Ident::new(COL_VALUE),
                ]),
            ));
        }
        ir::TyKind::Tuple(fields) => {
            for (index, _) in fields.iter().enumerate() {
                r.push(sql_ast::SelectItem::UnnamedExpr(
                    sql_ast::Expr::CompoundIdentifier(vec![
                        sql_ast::Ident::new(rel_var_name),
                        sql_ast::Ident::new(format!("f_{index}")),
                    ]),
                ));
            }
        }
        ir::TyKind::Array(items) => {
            r.push(sql_ast::SelectItem::ExprWithAlias {
                expr: sql_ast::Expr::Value(sql_ast::Value::Null),
                alias: sql_ast::Ident::new(COL_ARRAY_INDEX),
            });
            projection_for_ty_re(rel_var_name, items, r)
        }
        ir::TyKind::Enum(_) => todo!(),
        ir::TyKind::Function(_) => todo!(),
        ir::TyKind::Ident(_) => todo!(),
    }
}
