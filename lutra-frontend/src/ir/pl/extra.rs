use crate::ir::pl::{Expr, ExprKind, FuncCall, Ident};

impl FuncCall {
    pub fn new_simple(name: Expr, args: Vec<Expr>) -> Self {
        FuncCall {
            name: Box::new(name),
            args,
            named_args: Default::default(),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct FuncMetadata {
    /// Name of the function. Used for user-facing messages only.
    pub name_hint: Option<Ident>,

    pub implicit_closure: Option<Box<ImplicitClosureConfig>>,
    pub coerce_tuple: Option<u8>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ImplicitClosureConfig {
    pub param: u8,
    pub this: Option<u8>,
    pub that: Option<u8>,
}

impl FuncMetadata {
    pub(crate) fn as_debug_name(&self) -> &str {
        let ident = self.name_hint.as_ref();

        ident.map(|n| n.name.as_str()).unwrap_or("<anonymous>")
    }
}

impl Expr {
    pub fn new(kind: impl Into<ExprKind>) -> Self {
        Expr {
            id: None,
            kind: kind.into(),
            span: None,
            target_id: None,
            ty: None,
            needs_window: false,
            alias: None,
            flatten: false,
        }
    }
}
