//! Table rendering for Lutra binary data.
//!
//! Provides ASCII table rendering with support for nested tuples and arrays.
//! See `tabular.AGENTS.md` for design documentation.

#![cfg(feature = "std")]

mod format;
mod iterate;
mod layout;
mod render;

use crate::ir;
use crate::tabular::TabularReader;

/// Wraps [`TabularReader`] and recursively flattens nested tuples into leaf cells.
#[derive(Clone)]
pub struct Table<'d, 't> {
    reader: TabularReader<'d, 't>,
}

/// Configuration for table rendering.
#[derive(Debug, Clone)]
pub struct Config {
    /// Maximum column width (default: 20).
    pub max_col_width: usize,
    /// Maximum array items to show (default: 3).
    pub max_array_items: usize,
    /// Number of rows to sample for layout (None = scan all, default: Some(100)).
    pub sample_rows: Option<usize>,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            max_col_width: 20,
            max_array_items: 3,
            sample_rows: Some(100),
        }
    }
}

impl<'d, 't> Table<'d, 't> {
    pub fn new(data: &'d [u8], ty: &'t ir::Ty, ty_defs: &'t [ir::TyDef]) -> Self {
        Self {
            reader: TabularReader::new(data, ty, ty_defs),
        }
    }

    pub fn render(self) -> String {
        self.render_with_config(&Config::default())
    }

    pub fn render_with_config(self, config: &Config) -> String {
        // Pass 1: compute layout (sampling)
        let layout = self.clone().compute_layout(config);

        if layout.total_rows == 0 {
            return String::new();
        }

        // Pass 2: render
        render::render(self, &layout, config)
    }

    fn ty(&self) -> &'t ir::Ty {
        self.reader.ty()
    }

    /// Resolve type identifiers to their definitions.
    pub(super) fn get_ty_mat<'a>(&self, ty: &'a ir::Ty) -> &'a ir::Ty
    where
        't: 'a,
    {
        self.reader.get_ty_mat(ty)
    }

    /// Returns the row item type (unwraps array if root is array).
    pub(super) fn row_ty(&self) -> &'t ir::Ty {
        let ty = self.get_ty_mat(self.ty());
        match &ty.kind {
            ir::TyKind::Array(item) => item.as_ref(),
            _ => ty,
        }
    }

    /// Returns true if the type can be rendered in a single cell.
    pub(super) fn is_flat(&self, ty: &ir::Ty) -> bool {
        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Primitive(_) => true,
            ir::TyKind::Enum(variants) => variants.iter().all(|v| {
                let payload_ty = self.get_ty_mat(&v.ty);
                match &payload_ty.kind {
                    ir::TyKind::Tuple(fields) if fields.is_empty() => true,
                    _ => self.is_flat(&v.ty),
                }
            }),
            ir::TyKind::Tuple(_) => false,
            ir::TyKind::Array(_) => false,
            ir::TyKind::Function(_) => false,
            ir::TyKind::Ident(_) => unreachable!("should be resolved"),
        }
    }
}
