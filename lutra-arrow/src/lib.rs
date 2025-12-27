mod interface;
mod to_arrow;
mod to_lutra;
mod validate;

pub use interface::get_interface;
pub use to_arrow::lutra_to_arrow;
pub use to_lutra::arrow_to_lutra;
pub use validate::validate_schema;

use lutra_bin::ir;
use std::collections::HashMap;

struct Context<'t> {
    types: HashMap<&'t ir::Path, &'t ir::Ty>,
}

impl<'t> Context<'t> {
    fn new(ty_defs: &'t [ir::TyDef]) -> Self {
        Self {
            types: HashMap::from_iter(ty_defs.iter().map(|d| (&d.name, &d.ty))),
        }
    }

    fn get_ty_mat(&self, ty: &'t ir::Ty) -> &'t ir::Ty {
        match &ty.kind {
            ir::TyKind::Ident(path) => self.types.get(path).unwrap(),
            _ => ty,
        }
    }
}
