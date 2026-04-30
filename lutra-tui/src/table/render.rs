//! ASCII table rendering

use std::borrow::Cow;

use lutra_bin::bytes::Buf;
use lutra_bin::ir;
use lutra_bin::{ArrayReader, TableCell};

use crate::table::{Config, Cursor, Table};
use crate::terminal::{Line, Span, Style, View};

use super::format::{format_value, truncate};
use super::layout::{Align, ColumnGroup, Layout};

/// Table renderer with precomputed layout and configuration.
pub struct Renderer<'d> {
    table: Table<'d, 'd>,
    layout: Cow<'d, Layout>,
}

impl<'d> Renderer<'d> {
    /// Create a new renderer.
    pub fn new(table: Table<'d, 'd>, layout: Cow<'d, Layout>) -> Self {
        Self { table, layout }
    }

    fn config(&self) -> &Config {
        &self.layout.config
    }

    /// Render the table to a view.
    ///
    /// Returns the view and number of rows rendered.
    pub fn render(mut self, height: usize, cursor: Option<&Cursor>) -> (View<'d>, usize) {
        let mut view = View::new();

        self.render_headers(&mut view);
        self.render_divider(&mut view);

        let scroll = cursor.map(|c| c.scroll).unwrap_or_default();
        self.table.advance_by_(scroll).ok();

        let mut rows_rendered = 0;
        while view.line_count() < height {
            let Some(row) = self.table.next() else {
                break;
            };

            let row_idx = scroll + rows_rendered;
            let cursor = cursor.filter(|c| c.row == row_idx);
            self.render_data_row(&mut view, row_idx, &row, cursor);
            rows_rendered += 1;
        }

        (view, rows_rendered)
    }

    fn render_divider(&self, view: &mut View<'d>) {
        let index_width = if self.layout.show_index {
            self.layout.col_index_width + 1 // space after row index
        } else {
            0
        };
        let total_width: usize = index_width
            + self.layout.columns.iter().map(|c| c.width).sum::<usize>()
            + self.layout.columns.len().saturating_sub(1);
        view.push_line(Line::styled("─".repeat(total_width), Style::muted()));
    }

    fn render_headers(&self, view: &mut View<'d>) {
        if self.layout.column_groups.is_empty() {
            view.push_line(self.render_leaf_names());
        } else {
            for row in 0..self.layout.names_height {
                let mut line = Line::empty();

                if self.layout.show_index {
                    line.push_string(" ".repeat(self.layout.col_index_width));
                }

                self.render_header_name_row(&mut line, 0, &self.layout.column_groups, row, 0);
                view.push_line(line);
            }
        }
        view.push_line(self.render_type_names());
    }

    fn render_header_name_row(
        &self,
        out: &mut Line<'d>,
        start_idx: usize,
        groups: &[ColumnGroup],
        row: usize,
        depth: usize,
    ) {
        let mut col_idx = start_idx;
        for (i, group) in groups.iter().enumerate() {
            if i > 0 || (depth == 0 && self.layout.show_index) {
                out.push_span(" ");
            }

            let leaf_count = group.leaf_count();
            let span_width = self.get_header_name_width(col_idx, leaf_count);

            if group.children.is_empty() {
                if row == depth {
                    let col = &self.layout.columns[col_idx];
                    out.extend(align_or_truncate(
                        Span::new(group.name.clone()),
                        span_width,
                        col.align,
                    ));
                } else {
                    out.push_string(" ".repeat(span_width));
                }
            } else {
                if row == depth {
                    out.extend(align_or_truncate(
                        Span::new(group.name.clone()),
                        span_width,
                        Align::Center,
                    ));
                } else if row > depth {
                    self.render_header_name_row(out, col_idx, &group.children, row, depth + 1);
                } else {
                    out.push_string(" ".repeat(span_width));
                }
            }
            col_idx += leaf_count;
        }
    }

    fn render_leaf_names(&self) -> Line<'d> {
        let mut out = Line::empty();
        if self.layout.show_index {
            out.push_string(" ".repeat(self.layout.col_index_width));
        }

        for (i, col) in self.layout.columns.iter().enumerate() {
            if i > 0 || self.layout.show_index {
                out.push_span(" ");
            }
            out.extend(align_or_truncate(
                Span::new(col.name.clone()),
                col.width,
                col.align,
            ));
        }
        out
    }

    fn render_type_names(&self) -> Line<'d> {
        let mut out = Line::empty();
        if self.layout.show_index {
            out.push_string(" ".repeat(self.layout.col_index_width));
        }

        for (i, col) in self.layout.columns.iter().enumerate() {
            if i > 0 || self.layout.show_index {
                out.push_span(" ");
            }
            out.extend(align_or_truncate(
                Span::styled(col.ty_name.clone(), Style::muted()),
                col.width,
                col.align,
            ));
        }
        out
    }

    fn render_data_row(
        &mut self,
        view: &mut View<'d>,
        row_idx: usize,
        cells: &[TableCell<'d, '_>],
        cursor: Option<&Cursor>,
    ) -> usize {
        let row_height = self.layout.row_heights.get(row_idx).copied().unwrap_or(1);

        // Expand each cell into visual rows
        let mut cells: Vec<Vec<Span<'d>>> = cells
            .iter()
            .map(|cell| {
                let mut cell_lines = self.expand_cell(cell);
                cell_lines.reverse();
                cell_lines
            })
            .collect();

        let row_style = cursor
            .is_some_and(|c| c.row == row_idx)
            .then_some(Style::cursor_pale());

        for vr in 0..row_height {
            let mut out = Line::empty();

            // index
            if self.layout.show_index {
                if vr == 0 {
                    let index = format!("{:>width$}", row_idx, width = self.layout.col_index_width);
                    let style = row_style.unwrap_or(Style::muted());
                    out.push_span(Span::styled(index, style));
                } else {
                    out.push_string(" ".repeat(self.layout.col_index_width));
                }
            }

            for (col_idx, (cell, col)) in cells.iter_mut().zip(&self.layout.columns).enumerate() {
                let selected = cursor.is_some_and(|c| c.row == row_idx && c.col == col_idx);
                let style = selected
                    .then_some(Style::cursor())
                    .or(row_style)
                    .unwrap_or_default();

                // divider
                if col_idx > 0 || self.layout.show_index {
                    out.push_span(Span::styled(" ", style));
                }

                // content
                let mut content = cell.pop().unwrap_or_else(|| Span::new(""));
                content = content.set(style);
                out.extend(align_or_truncate(content, col.width, col.align));

                // cursor
                if selected {
                    view.set_cursor_here(out.width() as u16);
                }
            }
            view.push_line(out);
        }

        row_height
    }

    fn expand_cell(&self, cell: &TableCell) -> Vec<Span<'d>> {
        let ty = self.table.get_ty_mat(cell.ty());

        match &ty.kind {
            ir::TyKind::Array(item_ty) if self.table.is_flat(item_ty) => {
                let reader = ArrayReader::new_for_ty(cell.data(), ty);
                let items: Vec<_> = reader.collect();
                let total = items.len();

                if total == 0 {
                    vec![]
                } else if total <= self.config().max_array_items {
                    items
                        .iter()
                        .map(|data| {
                            if let Ok(text) = format_value(data.chunk(), item_ty, &self.table) {
                                Span::from(truncate(&text, self.config().max_col_width))
                            } else {
                                Span::from("?")
                            }
                        })
                        .collect()
                } else {
                    let show = self.config().max_array_items - 1;
                    let mut result: Vec<_> = items[..show]
                        .iter()
                        .map(|data| {
                            if let Ok(text) = format_value(data.chunk(), item_ty, &self.table) {
                                Span::from(truncate(&text, self.config().max_col_width))
                            } else {
                                Span::from("?")
                            }
                        })
                        .collect();
                    result.push(Span::from(format!("… {} more", total - show)));
                    result
                }
            }
            ir::TyKind::Array(_) => vec!["[…]".into()],
            _ => {
                if let Ok(text) = format_value(cell.data(), cell.ty(), &self.table) {
                    vec![Span::from(truncate(&text, self.config().max_col_width))]
                } else {
                    vec![Span::from("?")]
                }
            }
        }
    }

    fn get_header_name_width(&self, start: usize, count: usize) -> usize {
        let end = (start + count).min(self.layout.columns.len());
        let sum: usize = self.layout.columns[start..end]
            .iter()
            .map(|c| c.width)
            .sum();
        let separators = count.saturating_sub(1);
        sum + separators
    }
}

fn align_or_truncate(mut span: Span, width: usize, align: Align) -> Line {
    let (_, rem_width) = span.split_at_width(width);
    if rem_width == 0 {
        return Line::from(span);
    }

    let padding = Span::styled(" ".repeat(rem_width), span.style);
    match align {
        Align::Left => Line::from(vec![span, padding]),
        Align::Right => Line::from(vec![padding, span]),
        Align::Center => {
            let mut pad_left = padding;
            let pad_right = pad_left.split_at(pad_left.content.len() / 2);
            Line::from(vec![pad_left, span, pad_right])
        }
    }
}
