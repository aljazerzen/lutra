use lutra_bin::ir;
use sqlparser::ast as sql_ast;

use crate::sql::{COL_ARRAY_INDEX, COL_VALUE};

/// Generates an identity (no-op) projection of a relation of type `ty`, from relation variable `rel_var_name`.
/// When `top_level` is set, the projection does not include array index.
pub fn projection_for_ty(
    rel_var_name: Option<&str>,
    ty: &ir::Ty,
    top_level: bool,
) -> Vec<sql_ast::SelectItem> {
    let mut r = Vec::new();
    projection_for_ty_re(rel_var_name, ty, top_level, &mut r);
    r
}

fn projection_for_ty_re(
    rel_var_name: Option<&str>,
    ty: &ir::Ty,
    top_level: bool,
    r: &mut Vec<sql_ast::SelectItem>,
) {
    match &ty.kind {
        ir::TyKind::Primitive(_) => {
            r.push(sql_ast::SelectItem::UnnamedExpr(super::ident(
                rel_var_name,
                COL_VALUE,
            )));
        }
        ir::TyKind::Tuple(fields) => {
            for (index, _) in fields.iter().enumerate() {
                r.push(sql_ast::SelectItem::UnnamedExpr(super::ident(
                    rel_var_name,
                    format!("f_{index}"),
                )));
            }
        }
        ir::TyKind::Array(items) => {
            if !top_level {
                r.push(sql_ast::SelectItem::UnnamedExpr(super::ident(
                    rel_var_name,
                    COL_ARRAY_INDEX,
                )));
            }
            projection_for_ty_re(rel_var_name, items, top_level, r)
        }
        ir::TyKind::Enum(_) => todo!(),
        ir::TyKind::Function(_) => todo!(),
        ir::TyKind::Ident(_) => todo!(),
    }
}
