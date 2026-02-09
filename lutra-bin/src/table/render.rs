//! ASCII table rendering (pass 2).

use bytes::Buf;

use crate::ArrayReader;
use crate::ir;
use crate::tabular::TableCell;

use super::format::{format_value, truncate};
use super::layout::{Align, Column, ColumnGroup, Layout};
use super::{Config, Table};

/// Render a FlatTabular as ASCII using precomputed layout.
pub fn render(mut tabular: Table, layout: &Layout, config: &Config) -> String {
    if layout.total_rows == 0 {
        return String::new();
    }

    let mut out = String::new();

    // Render hierarchical headers (with row index column space)
    render_headers(&mut out, layout);

    // Divider (including row index column)
    let total_width: usize = layout.row_index_width
        + 1  // space after row index
        + layout.col_widths.iter().sum::<usize>()
        + layout.col_widths.len().saturating_sub(1);
    out += &"─".repeat(total_width);
    out += "\n";

    // Data rows
    let mut row_idx = 0;
    while let Some(row) = tabular.next() {
        render_data_row(&mut out, row_idx, &row, layout, config, &tabular);
        row_idx += 1;
    }

    out
}

fn render_headers(out: &mut String, layout: &Layout) {
    let max_depth = max_column_depth(&layout.column_groups).max(1);

    // If no nested columns, max_depth is 1, so we only render leaf names and types
    // If nested, we render parent rows first
    if layout.column_groups.is_empty() {
        // Single value case - render leaf names directly
        render_leaf_names(out, layout);
        render_type_names(out, layout);
        return;
    }

    // Render header name rows (one row per depth level)
    // Each row shows either:
    // - Parent column names (centered over their children)
    // - Leaf column names (for leaves at top level, shown in first row; for
    //   leaves with parents, shown at their parent's depth + 1)
    for level in 0..max_depth {
        render_header_name_row(out, layout, level);
    }

    // Type names row
    render_type_names(out, layout);
}

fn render_header_name_row(out: &mut String, layout: &Layout, level: usize) {
    // Empty space for row index column
    out.push_str(&" ".repeat(layout.row_index_width + 1));

    let mut col_idx = 0;
    render_header_name_row_impl(
        out,
        &layout.column_groups,
        &layout.columns,
        &layout.col_widths,
        level,
        0,
        &mut col_idx,
    );
    *out = out.trim_end().to_string();
    out.push('\n');
}

fn render_header_name_row_impl(
    out: &mut String,
    column_groups: &[ColumnGroup],
    columns: &[Column],
    widths: &[usize],
    level: usize,
    current_depth: usize,
    col_idx: &mut usize,
) {
    for group in column_groups {
        let leaf_count = group.leaf_count();
        let span_width = calculate_span_width(widths, *col_idx, leaf_count);

        if group.children.is_empty() {
            // Leaf column
            // Calculate at which level this leaf's name should appear
            // Top-level leaves (current_depth == 0) appear at level 0
            // Nested leaves appear at level = their parent's depth
            let leaf_name_level = if current_depth == 0 {
                0 // Top-level leaf: show at first row
            } else {
                // This leaf is inside a parent at current_depth
                // Its name should appear at the row after its parent
                current_depth
            };

            if level == leaf_name_level {
                let col = &columns[*col_idx];
                let padded = format_aligned(&group.name, span_width, col.align);
                out.push_str(&padded);
            } else {
                out.push_str(&" ".repeat(span_width));
            }

            if *col_idx + leaf_count < widths.len() {
                out.push(' ');
            }
            *col_idx += leaf_count;
        } else {
            // Parent column group with children
            // Parent name appears at current_depth (relative to where it is in the tree)
            // But we need to consider top-alignment: parent at top level shows at row 0
            let parent_name_level = current_depth;

            if level == parent_name_level {
                // Show parent name centered
                let name = &group.name;
                let name_width = name.chars().count();
                if name_width >= span_width {
                    out.push_str(&name.chars().take(span_width).collect::<String>());
                } else {
                    let padding = span_width - name_width;
                    let left_pad = padding / 2;
                    let right_pad = padding - left_pad;
                    out.push_str(&" ".repeat(left_pad));
                    out.push_str(name);
                    out.push_str(&" ".repeat(right_pad));
                }
                if *col_idx + leaf_count < widths.len() {
                    out.push(' ');
                }
                *col_idx += leaf_count;
            } else if level > parent_name_level {
                // Below parent's name level: recurse into children
                render_header_name_row_impl(
                    out,
                    &group.children,
                    columns,
                    widths,
                    level,
                    current_depth + 1,
                    col_idx,
                );
            } else {
                // Above parent's level: show empty
                out.push_str(&" ".repeat(span_width));
                if *col_idx + leaf_count < widths.len() {
                    out.push(' ');
                }
                *col_idx += leaf_count;
            }
        }
    }
}

fn calculate_span_width(widths: &[usize], start: usize, count: usize) -> usize {
    let end = (start + count).min(widths.len());
    let sum: usize = widths[start..end].iter().sum();
    // Add separators between columns
    let separators = count.saturating_sub(1);
    sum + separators
}

fn render_leaf_names(out: &mut String, layout: &Layout) {
    // Empty space for row index column
    out.push_str(&" ".repeat(layout.row_index_width + 1));

    for (i, (col, width)) in layout.columns.iter().zip(&layout.col_widths).enumerate() {
        let padded = format_aligned(&col.name, *width, col.align);
        out.push_str(&padded);
        if i < layout.columns.len() - 1 {
            out.push(' ');
        }
    }
    *out = out.trim_end().to_string();
    out.push('\n');
}

fn render_type_names(out: &mut String, layout: &Layout) {
    // Empty space for row index column
    out.push_str(&" ".repeat(layout.row_index_width + 1));

    for (i, (col, width)) in layout.columns.iter().zip(&layout.col_widths).enumerate() {
        let padded = format_aligned(&col.ty_name, *width, col.align);
        out.push_str(&padded);
        if i < layout.columns.len() - 1 {
            out.push(' ');
        }
    }
    *out = out.trim_end().to_string();
    out.push('\n');
}

fn render_data_row(
    out: &mut String,
    row_idx: usize,
    cells: &[TableCell],
    layout: &Layout,
    config: &Config,
    tabular: &Table,
) {
    let row_height = layout.row_heights.get(row_idx).copied().unwrap_or(1);

    // Expand each cell into visual rows
    let expanded: Vec<Vec<(String, Align)>> = cells
        .iter()
        .zip(layout.columns.iter())
        .map(|(cell, col)| expand_cell(cell, col.align, config, tabular))
        .collect();

    for vr in 0..row_height {
        // Row index (only on first visual row)
        if vr == 0 {
            out.push_str(&format!(
                "{:>width$} ",
                row_idx,
                width = layout.row_index_width
            ));
        } else {
            out.push_str(&" ".repeat(layout.row_index_width + 1));
        }

        for (i, ((values, width), col)) in expanded
            .iter()
            .zip(&layout.col_widths)
            .zip(&layout.columns)
            .enumerate()
        {
            let (content, align) = values
                .get(vr)
                .cloned()
                .unwrap_or_else(|| (String::new(), col.align));
            let padded = format_aligned(&content, *width, align);
            out.push_str(&padded);
            if i < expanded.len() - 1 {
                out.push(' ');
            }
        }
        *out = out.trim_end().to_string();
        out.push('\n');
    }
}

fn expand_cell(
    cell: &TableCell,
    default_align: Align,
    config: &Config,
    tabular: &Table,
) -> Vec<(String, Align)> {
    let ty = tabular.get_ty_mat(cell.ty());

    match &ty.kind {
        ir::TyKind::Array(item_ty) if tabular.is_flat(item_ty) => {
            let reader = ArrayReader::new_for_ty(cell.data(), ty);
            let items: Vec<_> = reader.collect();
            let total = items.len();

            if total == 0 {
                vec![]
            } else if total <= config.max_array_items {
                items
                    .iter()
                    .map(|data| {
                        if let Ok((text, align)) = format_value(data.chunk(), item_ty, tabular) {
                            (truncate(&text, config.max_col_width), align)
                        } else {
                            ("?".into(), default_align)
                        }
                    })
                    .collect()
            } else {
                let show = config.max_array_items - 1;
                let mut result: Vec<_> = items[..show]
                    .iter()
                    .map(|data| {
                        if let Ok((text, align)) = format_value(data.chunk(), item_ty, tabular) {
                            (truncate(&text, config.max_col_width), align)
                        } else {
                            ("?".into(), default_align)
                        }
                    })
                    .collect();
                result.push((format!("… {} more", total - show), Align::Left));
                result
            }
        }
        ir::TyKind::Array(_) => vec![("[…]".into(), Align::Left)],
        _ => {
            if let Ok((text, align)) = format_value(cell.data(), cell.ty(), tabular) {
                vec![(truncate(&text, config.max_col_width), align)]
            } else {
                vec![("?".into(), default_align)]
            }
        }
    }
}

fn format_aligned(content: &str, width: usize, align: Align) -> String {
    let content_width = content.chars().count();
    if content_width >= width {
        content.chars().take(width).collect()
    } else {
        match align {
            Align::Left => format!("{:<width$}", content, width = width),
            Align::Right => format!("{:>width$}", content, width = width),
        }
    }
}

fn max_column_depth(column_groups: &[ColumnGroup]) -> usize {
    column_groups.iter().map(|c| c.depth()).max().unwrap_or(0)
}
