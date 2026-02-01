use std::rc::Rc;

use lutra_compiler::error::DiagnosticMessage;
use ratatui::prelude::*;
use ratatui::widgets::Paragraph;

use crate::project::{CompileResult, ProjectState};
use crate::terminal::{Action, Component, EventResult};

/// Center panel displaying compilation errors and warnings.
pub struct DiagnosticsPanel {
    diagnostics: Rc<Vec<DiagnosticMessage>>,
    pub focused: bool,
    /// Currently selected diagnostic index.
    selected: usize,
    /// Scroll offset within the selected diagnostic.
    scroll: usize,
}

impl DiagnosticsPanel {
    /// Creates a new empty diagnostics panel.
    pub fn new() -> Self {
        Self {
            diagnostics: Rc::new(vec![]),
            focused: false,
            selected: 0,
            scroll: 0,
        }
    }

    /// Reset selection when diagnostics change.
    pub fn update(&mut self, project: &ProjectState) {
        self.diagnostics = if let CompileResult::Failed { diagnostics } = &project.compilation {
            diagnostics.clone()
        } else {
            Rc::new(vec![])
        };

        let count = self.diagnostics.len();

        if self.selected >= count && count > 0 {
            self.selected = count - 1;
        }
        self.scroll = 0;
    }
}

impl Component for DiagnosticsPanel {
    /// Handle a key event. Returns an action if the panel requests app-level behavior.
    fn handle(&mut self, action: Action) -> EventResult {
        match action {
            Action::MoveUp => {
                if self.selected > 0 {
                    self.selected -= 1;
                    self.scroll = 0;
                    return EventResult::redraw();
                }
            }
            Action::MoveDown => {
                if self.selected + 1 < self.diagnostics.len() {
                    self.selected += 1;
                    self.scroll = 0;
                    return EventResult::redraw();
                }
            }
            _ => {}
        }
        EventResult::default()
    }

    /// Render the panel to the given area.
    fn render(&self, frame: &mut Frame, area: Rect) {
        let block = crate::style::panel_primary(" Errors ", self.focused);
        let inner = block.inner(area);
        frame.render_widget(block, area);

        if self.diagnostics.is_empty() {
            let empty = Paragraph::new("No errors");
            frame.render_widget(empty, inner);
            return;
        }

        // Render diagnostics list
        let content: String = self
            .diagnostics
            .iter()
            .enumerate()
            .map(|(i, d)| {
                let prefix = if i == self.selected { "▸ " } else { "  " };
                format!("{prefix}{}", d.display())
            })
            .collect::<Vec<_>>()
            .join("\n\n");

        let paragraph = Paragraph::new(content);
        frame.render_widget(paragraph, inner);
    }
}
