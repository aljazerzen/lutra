use crate::ir;

pub enum SidKind {
    External,
    Var,
    FunctionScope,
}

impl ir::Sid {
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
}
