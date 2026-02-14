use lutra_bin::ir;
use std::collections::HashMap;

/// Context for resolving type identifiers
pub struct Context<'a> {
    types: HashMap<&'a ir::Path, &'a ir::Ty>,
}

impl<'a> Context<'a> {
    pub fn new(ty_defs: &'a [ir::TyDef]) -> Self {
        Context {
            types: ty_defs.iter().map(|def| (&def.name, &def.ty)).collect(),
        }
    }

    /// Resolve type identifiers to their materialized types
    pub fn get_ty_mat(&self, ty: &'a ir::Ty) -> Result<&'a ir::Ty, Error> {
        let mut ty = ty;
        while let ir::TyKind::Ident(path) = &ty.kind {
            ty = self.types.get(path).ok_or(Error::BadType)?;
        }
        Ok(ty)
    }

    /// Checks if enum is option pattern: enum {none, some: T} where T is primitive or array.
    /// These can be represented as nullable values (single column).
    pub fn is_option(&self, variants: &[ir::TyEnumVariant]) -> Result<bool, Error> {
        if variants.len() != 2 || !variants[0].ty.is_unit() {
            return Ok(false);
        }
        let some_ty = self.get_ty_mat(&variants[1].ty)?;
        Ok(some_ty.kind.is_primitive() || some_ty.kind.is_array())
    }
}

/// Common error type for Arrow conversion operations
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("type mismatch: Arrow has {arrow_ty}, but Lutra expects {lutra_ty}")]
    TypeMismatch { arrow_ty: String, lutra_ty: String },

    #[error("expected single row, got {got}")]
    ExpectedSingleRow { got: usize },

    #[error("expected {expected} columns, got {got}")]
    ColumnCountMismatch { expected: usize, got: usize },

    #[error("unexpected null value for non-option type")]
    UnexpectedNull,

    #[error("provided type is invalid")]
    BadType,

    #[error("unsupported type for Arrow conversion: {0}")]
    UnsupportedType(String),

    #[error("Arrow error: {0}")]
    Arrow(#[from] arrow::error::ArrowError),
}
