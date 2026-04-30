use crate::ArrayReader;
use crate::ir::{self, TyKind};

/// Structural shape of a value.
#[derive(Debug, Clone, Copy)]
pub struct Shape {
    /// Number of array items; `Some` only when the top-level type is `Array`.
    pub items: Option<usize>,
    /// Number of tuple fields; `Some` when the top-level type is `Tuple` or
    /// `Array` whose item type is a `Tuple`.
    pub fields: Option<usize>,
}

/// Extract the [`Shape`] of `data` without decoding individual values.
///
/// Item count is read from the array header in O(1).
/// Field count is read from the type definition without touching `data`.
pub fn get_shape(data: &[u8], ty: &ir::Ty, ty_defs: &[ir::TyDef]) -> Shape {
    let ty_mat = resolve_ty(ty, ty_defs);
    match &ty_mat.kind {
        TyKind::Array(item_ty) => {
            let (_, items) = ArrayReader::<&[u8]>::read_head(data);
            Shape {
                items: Some(items),
                fields: get_shape_tuple(item_ty, ty_defs),
            }
        }
        TyKind::Tuple(_) => Shape {
            items: None,
            fields: get_shape_tuple(ty_mat, ty_defs),
        },
        _ => Shape {
            items: None,
            fields: None,
        },
    }
}

pub fn get_shape_tuple(ty: &ir::Ty, ty_defs: &[ir::TyDef]) -> Option<usize> {
    let ty_mat = resolve_ty(ty, ty_defs);
    match &ty_mat.kind {
        TyKind::Tuple(fields) => Some(
            fields
                .iter()
                .map(|f| get_shape_tuple(&f.ty, ty_defs).unwrap_or(1))
                .sum(),
        ),
        _ => None,
    }
}

fn resolve_ty<'a>(ty: &'a ir::Ty, ty_defs: &'a [ir::TyDef]) -> &'a ir::Ty {
    if let TyKind::Ident(path) = &ty.kind
        && let Some(def) = ty_defs.iter().find(|d| &d.name == path)
    {
        return resolve_ty(&def.ty, ty_defs);
    }
    ty
}
