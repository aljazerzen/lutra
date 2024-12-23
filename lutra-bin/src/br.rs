pub use crate::generated::br::*;
pub use crate::generated::ir::{Literal, Ty, TyEnumVariant, TyFunction, TyKind, TyTupleField};

#[derive(Clone, Copy)]
pub enum SidKind {
    External,
    Var,
    FunctionScope,
}

impl SidKind {
    fn tag(self) -> u32 {
        match self {
            SidKind::External => 0 << 30,
            SidKind::Var => 1 << 30,
            SidKind::FunctionScope => 2 << 30,
        }
    }
}

impl Sid {
    pub fn kind(&self) -> SidKind {
        let sid_kind: u32 = self.0 >> 30;
        match sid_kind {
            0 => SidKind::External,
            1 => SidKind::Var,
            2 => SidKind::FunctionScope,
            _ => {
                panic!()
            }
        }
    }

    pub fn with_tag(mut self, kind: SidKind) -> Self {
        self.0 |= kind.tag();
        self
    }
}
