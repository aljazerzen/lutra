use std::cell::Cell;
use std::rc::Rc;
use std::time::Duration;

use crossterm::event::KeyCode;
use lutra_bin::{Table, TableConfig, TableLayout};
use lutra_bin::{ir, rr};
use ratatui::layout::Alignment;
use ratatui::prelude::*;
use ratatui::widgets::{Block, Borders, Padding, Paragraph};

use crate::terminal::{Action, ActionResult, Component};
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

    // Auto-run state
    auto_run: AutoRunState,
}

impl OutputPane {
    pub fn new(
        program_ty: Rc<rr::ProgramType>,
        output: Vec<u8>,
        duration: Duration,
        auto_run: AutoRunState,
    ) -> Self {
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
            auto_run,
        }
    }

    /// Get a mutable reference to the auto-run state.
    pub fn auto_run_mut(&mut self) -> &mut AutoRunState {
        &mut self.auto_run
    }

    /// Take ownership of the auto-run state, replacing it with a new default state.
    pub fn take_auto_run(&mut self) -> AutoRunState {
        std::mem::replace(&mut self.auto_run, AutoRunState::new())
    }
}

impl Component for OutputPane {
    fn render(&self, frame: &mut Frame, area: Rect) {
        // Get auto-run status text and color
        let status = self.auto_run.widget();

        let block = Block::default()
            .title(" Output ")
            .title(Line::from(status).alignment(Alignment::Right))
            .borders(Borders::TOP | Borders::LEFT)
            .padding(Padding::left(1))
            .border_style(Style::default().fg(Color::DarkGray));

        let inner = block.inner(area);
        frame.render_widget(block, area);
        let area = inner;

        // Header with duration
        let header = format!("Completed in {:.2?}", self.duration);
        let header = Span::styled(header, Style::default().fg(Color::DarkGray));
        frame.render_widget(header.into_right_aligned_line(), area);

        let area = clip_top(area, 1);

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

    fn handle(&mut self, action: Action) -> ActionResult {
        let Some(key) = action.as_key() else {
            return ActionResult::default();
        };

        match key.code {
            KeyCode::Up => {
                self.scroll = self.scroll.saturating_sub(1);
                ActionResult::redraw()
            }
            KeyCode::Down => {
                // Prevent scrolling beyond available rows
                if self.scroll + 1 < self.layout.total_rows {
                    self.scroll += 1;
                }
                ActionResult::redraw()
            }
            KeyCode::PageUp => {
                // Scroll up by viewport height, but at least 1 row
                let page_size = self.viewport_rows.get().max(1);
                self.scroll = self.scroll.saturating_sub(page_size);
                ActionResult::redraw()
            }
            KeyCode::PageDown => {
                // Scroll down by viewport height
                let page_size = self.viewport_rows.get().max(1);
                let max_scroll = self.layout.total_rows.saturating_sub(1);
                self.scroll = (self.scroll + page_size).min(max_scroll);
                ActionResult::redraw()
            }
            KeyCode::Enter => {
                // Return to input state
                ActionResult::action(Action::ReturnToInput)
            }
            _ => ActionResult::default(),
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
            if !line.is_char_boundary(mute_prefix) {
                return Line::from(line);
            }
            let (leading, rest) = line.split_at(mute_prefix);
            Line::from(vec![
                Span::styled(leading, style_muted),
                Span::styled(rest, style_default),
            ])
        })
        .collect()
}

/// Auto-run state for a program pane.
#[derive(Debug, Clone)]
pub struct AutoRunState {
    /// Whether auto-run is enabled by user.
    enabled: bool,

    /// Last known input type (for detecting changes).
    last_input_ty: Option<ir::Ty>,

    /// Whether auto-run is currently suspended due to type change.
    suspended: bool,
}

impl AutoRunState {
    pub fn new() -> Self {
        Self {
            enabled: false,
            last_input_ty: None,
            suspended: false,
        }
    }

    /// Toggle auto-run enabled/disabled. Returns new enabled state.
    pub fn toggle(&mut self) -> bool {
        self.enabled = !self.enabled;
        if self.enabled {
            // Clear suspension when manually enabling
            self.suspended = false;
        }
        self.enabled
    }

    /// Check if input type changed. If changed and enabled, suspends auto-run.
    /// Returns true if type changed.
    pub fn on_new_input_ty(&mut self, new_input_ty: &ir::Ty) -> bool {
        let changed = match &self.last_input_ty {
            Some(old_ty) => old_ty != new_input_ty,
            None => false,
        };

        if changed && self.enabled {
            self.suspended = true;
        }

        self.last_input_ty = Some(new_input_ty.clone());
        self.enabled && !self.suspended
    }

    /// Called after a successful manual run. Resumes auto-run if suspended.
    pub fn on_successful_run(&mut self) {
        if self.suspended && self.enabled {
            self.suspended = false;
        }
    }

    /// Returns status text and color for UI display.
    pub fn widget(&self) -> Span<'static> {
        let (text, color) = if !self.enabled {
            (" ⏹ Auto-run disabled", Color::DarkGray)
        } else if self.suspended {
            (" ⏸ Auto-run (type changed)", Color::Yellow)
        } else {
            (" ▶︎ Auto-run enabled", Color::White)
        };
        Span::styled(text, Style::default().fg(color))
    }
}
