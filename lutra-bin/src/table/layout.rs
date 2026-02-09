//! Layout computation for table rendering (pass 1).

use bytes::Buf;

use crate::ArrayReader;
use crate::ir;
use crate::tabular::TableCell;

use super::format::{format_ty_name, format_value, truncate};
use super::{Config, Table};

/// Computed layout for table rendering.
#[derive(Debug, Clone)]
pub struct Layout {
    /// Hierarchical column groups (for multi-row headers).
    pub column_groups: Vec<ColumnGroup>,
    /// Flat list of leaf columns.
    pub columns: Vec<Column>,
    /// Width for each leaf column.
    pub col_widths: Vec<usize>,
    /// Width for the row index column.
    pub row_index_width: usize,
    /// Visual height for each logical row (due to array expansion).
    pub row_heights: Vec<usize>,
    /// Total logical rows scanned.
    pub total_rows: usize,
}

/// Leaf column with type info (for formatting/alignment).
#[derive(Debug, Clone)]
pub struct Column {
    /// Column name.
    pub name: String,
    /// Type display string for header.
    pub ty_name: String,
    /// Alignment for data in this column.
    pub align: Align,
}

/// Cell alignment.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Align {
    Left,
    Right,
}

/// Hierarchical column group (for multi-row headers with nested columns).
#[derive(Debug, Clone)]
pub struct ColumnGroup {
    /// Column group name (field name or positional index).
    pub name: String,
    /// Nested column groups (empty for leaf columns).
    pub children: Vec<ColumnGroup>,
}

impl ColumnGroup {
    /// Count of leaf columns under this column group.
    pub fn leaf_count(&self) -> usize {
        if self.children.is_empty() {
            1
        } else {
            self.children.iter().map(|c| c.leaf_count()).sum()
        }
    }

    /// Maximum depth of the column hierarchy.
    pub fn depth(&self) -> usize {
        if self.children.is_empty() {
            1
        } else {
            1 + self.children.iter().map(|c| c.depth()).max().unwrap_or(0)
        }
    }
}

impl<'d, 't> Table<'d, 't> {
    /// Compute layout by sampling rows from the tabular iterator.
    pub fn compute_layout(mut self, config: &Config) -> Layout {
        let column_groups = self.build_column_groups(self.row_ty());
        let columns = self.flatten_columns(&column_groups, self.row_ty());

        // Initialize widths from headers
        let mut col_widths: Vec<usize> = columns
            .iter()
            .map(|c| {
                let name_width = c.name.chars().count();
                let ty_width = c.ty_name.chars().count();
                name_width.max(ty_width)
            })
            .collect();

        let mut row_heights = Vec::new();
        let mut rows_scanned = 0;

        while let Some(row) = self.next() {
            let mut row_height = 1usize;
            for (i, cell) in row.iter().enumerate() {
                let (width, height) = self.measure_cell(cell, config);
                if i < col_widths.len() {
                    col_widths[i] = col_widths[i].max(width);
                }
                row_height = row_height.max(height);
            }
            row_heights.push(row_height);

            rows_scanned += 1;
            if let Some(l) = config.sample_rows
                && rows_scanned >= l
            {
                break;
            }
        }

        // Compute row index width based on total rows
        let row_index_width = if rows_scanned == 0 {
            1
        } else {
            (rows_scanned - 1).to_string().len()
        };

        Layout {
            column_groups,
            columns,
            col_widths,
            row_index_width,
            row_heights,
            total_rows: rows_scanned,
        }
    }

    fn build_column_groups(&self, ty: &'t ir::Ty) -> Vec<ColumnGroup> {
        let ty = self.get_ty_mat(ty);
        match &ty.kind {
            ir::TyKind::Tuple(fields) => fields
                .iter()
                .enumerate()
                .map(|(i, f)| {
                    let name = f.name.clone().unwrap_or_else(|| i.to_string());
                    let children = self.build_column_groups(&f.ty);
                    ColumnGroup { name, children }
                })
                .collect(),
            _ => vec![],
        }
    }

    fn flatten_columns(&self, groups: &[ColumnGroup], ty: &'t ir::Ty) -> Vec<Column> {
        let ty = self.get_ty_mat(ty);
        match &ty.kind {
            ir::TyKind::Tuple(fields) => {
                let mut leaves = Vec::new();
                for (group, field) in groups.iter().zip(fields.iter()) {
                    if group.children.is_empty() {
                        leaves.push(Column {
                            name: group.name.clone(),
                            ty_name: format_ty_name(&field.ty, self),
                            align: self.infer_align(&field.ty),
                        });
                    } else {
                        leaves.extend(self.flatten_columns(&group.children, &field.ty));
                    }
                }
                leaves
            }
            _ => {
                // Single column for non-tuple root (primitive, array, enum)
                vec![Column {
                    name: "value".into(),
                    ty_name: format_ty_name(ty, self),
                    align: self.infer_align(ty),
                }]
            }
        }
    }

    /// Infer alignment from type.
    fn infer_align(&self, ty: &ir::Ty) -> Align {
        let ty = self.get_ty_mat(ty);
        match &ty.kind {
            ir::TyKind::Primitive(p) => match p {
                ir::TyPrimitive::bool | ir::TyPrimitive::text => Align::Left,
                _ => Align::Right, // All numeric types
            },
            ir::TyKind::Enum(_) => Align::Left,
            ir::TyKind::Array(_) => Align::Left,
            ir::TyKind::Tuple(_) => Align::Left,
            ir::TyKind::Function(_) => Align::Left,
            ir::TyKind::Ident(_) => unreachable!("should be resolved"),
        }
    }

    /// Measure a cell's width and height.
    fn measure_cell(&self, cell: &TableCell, config: &Config) -> (usize, usize) {
        let ty = self.get_ty_mat(cell.ty());

        match &ty.kind {
            ir::TyKind::Array(item_ty) if self.is_flat(item_ty) => {
                // Expandable array: measure items
                let reader = ArrayReader::new_for_ty(cell.data(), ty);
                let count = reader.remaining();

                let height = if count == 0 {
                    1
                } else if count <= config.max_array_items {
                    count
                } else {
                    config.max_array_items // (max-1) items + "… X more" row
                };

                // Measure width of items (sample first few)
                let mut max_width = 0usize;
                let items_to_measure = count.min(config.max_array_items);
                for item_data in reader.take(items_to_measure) {
                    if let Ok((text, _)) = format_value(item_data.chunk(), item_ty, self) {
                        let text = truncate(&text, config.max_col_width);
                        max_width = max_width.max(text.chars().count());
                    }
                }

                // Also consider the "… N more" text width
                if count > config.max_array_items {
                    let ellipsis_text = format!("… {} more", count - (config.max_array_items - 1));
                    max_width = max_width.max(ellipsis_text.chars().count());
                }

                (max_width.min(config.max_col_width), height)
            }
            ir::TyKind::Array(_) => {
                // Non-flat array: "[…]"
                (3, 1)
            }
            _ => {
                // Primitive or enum: format and measure
                if let Ok((text, _)) = format_value(cell.data(), cell.ty(), self) {
                    let text = truncate(&text, config.max_col_width);
                    (text.chars().count(), 1)
                } else {
                    (3, 1) // fallback for errors
                }
            }
        }
    }
}
