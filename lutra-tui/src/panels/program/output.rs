use std::cell::Cell;
use std::rc::Rc;
use std::time::Duration;

use lutra_bin::rr;
use lutra_bin::{Table, TableConfig, TableLayout};
use ratatui::prelude::*;
use ratatui::widgets::{Block, Borders, Padding, Paragraph};

use crate::terminal::{Action, Component, EventResult};
use crate::utils::clip_top;

/// Output stage component - displays execution results
pub struct OutputPane {
    // Shared state
    program_ty: Rc<rr::ProgramType>,

    // Output-specific state (preserved across transitions)
    output: Vec<u8>,
    duration: Duration,

    // Table rendering state
    layout: TableLayout,
    config: TableConfig,
    scroll: usize,
    viewport_rows: Cell<usize>,
}

impl OutputPane {
    pub fn new(program_ty: Rc<rr::ProgramType>, output: Vec<u8>, duration: Duration) -> Self {
        // Create table config
        let config = TableConfig::default();

        // Compute layout (one-time, potentially expensive operation)
        let table = Table::new(&output, &program_ty.output, &program_ty.defs);
        let layout = table.compute_layout(&config);

        Self {
            program_ty,
            output,
            duration,
            layout,
            config,
            scroll: 0,
            viewport_rows: Cell::new(10), // Default until first render
        }
    }
}

impl Component for OutputPane {
    fn render(&self, frame: &mut Frame, area: Rect) {
        let block = Block::default()
            .title(" Output ")
            .borders(Borders::TOP | Borders::LEFT)
            .padding(Padding::left(1))
            .border_style(Style::default().fg(Color::DarkGray));

        let inner = block.inner(area);
        frame.render_widget(block, area);
        let area = inner;

        // Header with duration
        let header = format!("Completed in {:.2?}", self.duration);
        let header = Span::styled(header, Style::default().fg(Color::DarkGray));
        frame.render_widget(header, area);

        let area = clip_top(area, 2);

        // Output value (rendered as table)
        let table = Table::new(&self.output, &self.program_ty.output, &self.program_ty.defs);
        let height = area.height as usize;
        let (table_text, rows_rendered) =
            table.render_window(&self.layout, &self.config, self.scroll, height);
        self.viewport_rows.set(rows_rendered);

        // Convert string to styled lines (headers, divider, and row numbers are DarkGray)
        let styled_lines = colorize_table(&table_text, &self.layout);
        frame.render_widget(Paragraph::new(styled_lines), area);
    }

    fn handle(&mut self, action: Action) -> EventResult {
        match action {
            Action::MoveUp => {
                self.scroll = self.scroll.saturating_sub(1);
                EventResult::redraw()
            }
            Action::MoveDown => {
                // Prevent scrolling beyond available rows
                if self.scroll + 1 < self.layout.total_rows {
                    self.scroll += 1;
                }
                EventResult::redraw()
            }
            Action::MovePageUp => {
                // Scroll up by viewport height, but at least 1 row
                let page_size = self.viewport_rows.get().max(1);
                self.scroll = self.scroll.saturating_sub(page_size);
                EventResult::redraw()
            }
            Action::MovePageDown => {
                // Scroll down by viewport height
                let page_size = self.viewport_rows.get().max(1);
                let max_scroll = self.layout.total_rows.saturating_sub(1);
                self.scroll = (self.scroll + page_size).min(max_scroll);
                EventResult::redraw()
            }
            Action::Select => {
                // Return to input state
                EventResult::action(Action::ReturnToInput)
            }
            _ => EventResult::default(),
        }
    }
}

/// Convert plain string table to styled Lines
fn colorize_table<'a>(table_text: &'a str, layout: &lutra_bin::TableLayout) -> Vec<Line<'a>> {
    // This coloring is a bit hacky, but it works.

    let style_default = Style::default();
    let style_muted = Style::default().fg(Color::DarkGray);

    let mute_rows = [
        layout.names_height,     // type row
        layout.names_height + 1, // divider row
    ];

    // Row index column width (leading spaces in headers)
    let mute_prefix = layout.col_index_width + 1;

    table_text
        .lines()
        .enumerate()
        .map(|(i, line)| {
            // muted lines
            if mute_rows.contains(&i) {
                return Line::from(vec![Span::styled(line.to_string(), style_muted)]);
            }

            // muted prefix
            let (leading, rest) = line.split_at(mute_prefix);
            Line::from(vec![
                Span::styled(leading, style_muted),
                Span::styled(rest, style_default),
            ])
        })
        .collect()
}
