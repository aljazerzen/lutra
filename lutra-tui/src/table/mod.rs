//! Table rendering for Lutra binary data.
//!
//! Provides ASCII table rendering with support for nested tuples and arrays.
//! See `tabular.AGENTS.md` for design documentation.

mod format;
mod layout;
mod render;

use std::borrow::Cow;

pub use layout::Layout;
pub use render::Renderer;

use lutra_bin::TableCell;
use lutra_bin::TabularReader;
use lutra_bin::TupleReader;
use lutra_bin::ir;

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

impl<'d, 't: 'd> Table<'d, 't> {
    pub fn new(data: &'d [u8], ty: &'t ir::Ty, ty_defs: &'t [ir::TyDef]) -> Self {
        Self {
            reader: TabularReader::new(data, ty, ty_defs),
        }
    }

    /// Render to a [`View`] of styled lines.
    pub fn render_once(self, config: Config) -> crate::terminal::View<'d> {
        let layout = self.clone().compute_layout(config);
        Renderer::new(self, Cow::Owned(layout))
            .render(usize::MAX, None)
            .0
    }

    /// Render a window of rows (for scrolling/pagination) using a pre-computed layout.
    ///
    /// - `height`: Stop after rendering height number of lines
    /// - `cursor`: scroll and selection of highlighted cell
    ///
    /// The header is always included in the output.
    pub fn render(
        self,
        layout: &'d layout::Layout,
        height: usize,
        cursor: Option<&Cursor>,
    ) -> (crate::terminal::View<'d>, usize) {
        Renderer::new(self, Cow::Borrowed(layout)).render(height, cursor)
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

/// A selected cell within a rendered table.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Cursor {
    /// 0-based logical row index.
    pub row: usize,
    /// 0-based column index.
    pub col: usize,
    /// 0-based index of first row that should be visible
    pub scroll: usize,
}

impl Cursor {
    /// Clamp row and col to valid bounds for the given layout.
    pub fn clamp(&mut self, layout: &Layout) {
        self.row = self.row.min(layout.last_row());
        self.col = self.col.min(layout.last_col());
    }

    /// Correct scroll such that the cursor is in the view.
    pub fn scroll_into_view(&mut self, page_size: usize, layout: &Layout) {
        const VIEW_MARGIN: usize = 3;

        // first and last row of cursor visible
        let cursor_first = self.row.saturating_sub(VIEW_MARGIN);
        let cursor_last = (self.row + VIEW_MARGIN + 1)
            .min(layout.row_count())
            .saturating_sub(page_size);

        self.scroll = self.scroll.min(cursor_first);
        self.scroll = self.scroll.max(cursor_last);
    }
}

impl<'d, 't> Iterator for Table<'d, 't> {
    type Item = Vec<TableCell<'d, 't>>;

    fn next(&mut self) -> Option<Self::Item> {
        let row = self.reader.next()?;
        Some(self.flatten_row(row))
    }
}

impl<'d, 't> Table<'d, 't> {
    pub fn advance_by_(&mut self, n: usize) -> Result<(), std::num::NonZero<usize>> {
        self.reader.advance_by_(n)
    }

    fn flatten_row(&self, cells: Vec<TableCell<'d, 't>>) -> Vec<TableCell<'d, 't>> {
        let mut leaves = Vec::new();
        for cell in cells {
            self.flatten_cell(cell, &mut leaves);
        }
        leaves
    }

    fn flatten_cell(&self, cell: TableCell<'d, 't>, out: &mut Vec<TableCell<'d, 't>>) {
        let ty = self.get_ty_mat(cell.ty());
        match &ty.kind {
            ir::TyKind::Tuple(fields) => {
                let reader = TupleReader::new_for_ty(cell.data(), ty);
                for (i, field) in fields.iter().enumerate() {
                    let field_data: &'d [u8] = reader.get_field(i);
                    let field_cell = TableCell::new(field_data, &field.ty, cell.ty_defs());
                    self.flatten_cell(field_cell, out);
                }
            }
            _ => {
                // Leaf: primitive, array, or enum
                out.push(cell);
            }
        }
    }
}
