mod literal;
mod printer;

pub use crate::generated::ir::*;
#[cfg(feature = "std")]
pub use printer::{print, print_ty};

use crate::{boxed, vec};

impl Program {
    pub fn get_output_ty(&self) -> &Ty {
        let main_ty = self.main.ty.kind.as_function().unwrap();
        &main_ty.body
    }

    pub fn get_input_tys(&self) -> &[Ty] {
        let main_ty = self.main.ty.kind.as_function().unwrap();
        main_ty.params.as_slice()
    }
}

impl PartialEq for Ty {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for Ty {}

impl Ty {
    pub fn new(kind: impl Into<TyKind>) -> Self {
        Ty {
            kind: kind.into(),
            layout: None,
            name: None,
        }
    }
}

impl From<PrimitiveSet> for TyKind {
    fn from(value: PrimitiveSet) -> Self {
        TyKind::Primitive(value)
    }
}
impl From<vec::Vec<TyTupleField>> for TyKind {
    fn from(value: vec::Vec<TyTupleField>) -> Self {
        TyKind::Tuple(value)
    }
}
impl From<TyFunction> for TyKind {
    fn from(value: TyFunction) -> Self {
        TyKind::Function(boxed::Box::new(value))
    }
}
impl From<Path> for TyKind {
    fn from(value: Path) -> Self {
        TyKind::Ident(value)
    }
}
