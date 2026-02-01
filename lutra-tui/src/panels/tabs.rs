use lutra_bin::ir;
use lutra_compiler::Project;
use ratatui::prelude::*;
use ratatui::widgets::{Block, Padding, Paragraph};

use crate::RunnerConfig;
use crate::panels::ProgramPane;
use crate::style;
use crate::terminal::{Action, Component, EventResult};

/// Tabbed interface for managing multiple program panes.
pub struct Tabs {
    /// All open program panes.
    panels: Vec<ProgramPane>,

    /// Currently active tab index.
    active_tab: usize,

    /// Runner client for creating new panels.
    runner_client: lutra_runner::channel::ClientSender,

    /// Focus state.
    focused: bool,
}

impl Tabs {
    /// Create a new tabs component.
    pub fn new(runner_client: lutra_runner::channel::ClientSender) -> Self {
        Self {
            panels: Vec::new(),
            active_tab: 0,
            runner_client,
            focused: false,
        }
    }

    pub fn set_focused(&mut self, focused: bool) {
        self.focused = focused;
        if let Some(p) = self.panels.get_mut(self.active_tab) {
            p.set_focused(focused);
        }
    }

    /// Open a program (reuses existing tab or creates new).
    ///
    /// Returns true if a new tab was created.
    pub fn open_program(
        &mut self,
        path: ir::Path,
        project: &Project,
        runner_cfg: &RunnerConfig,
    ) -> bool {
        // Check if panel for this path already exists
        if let Some(index) = self.panels.iter().position(|p| p.path() == &path) {
            // Reuse: switch to existing tab
            self.active_tab = index;
            return false; // No new tab created
        }

        // Create new panel
        let panel = ProgramPane::new(path, self.runner_client.clone(), project, runner_cfg);
        self.panels.push(panel);
        self.active_tab = self.panels.len() - 1;
        true // New tab created
    }

    /// Close the active tab.
    pub fn close_active_tab(&mut self) {
        if self.panels.is_empty() {
            return;
        }

        let mut panel = self.panels.remove(self.active_tab);
        panel.clear(); // Release prepared program

        // Adjust active_tab if needed
        if self.active_tab >= self.panels.len() && self.active_tab > 0 {
            self.active_tab -= 1;
        }
    }

    /// Switch to tab by index (0-based, clamped to valid range).
    pub fn switch_to_tab(&mut self, index: usize) {
        if index < self.panels.len() {
            self.active_tab = index;
        }
        // Ignore if out of bounds
    }

    /// Switch to next tab (wraps around).
    pub fn next_tab(&mut self) {
        if self.panels.is_empty() {
            return;
        }
        self.active_tab = (self.active_tab + 1) % self.panels.len();
    }

    /// Switch to previous tab (wraps around).
    pub fn prev_tab(&mut self) {
        if self.panels.is_empty() {
            return;
        }
        self.active_tab = if self.active_tab == 0 {
            self.panels.len() - 1
        } else {
            self.active_tab - 1
        };
    }

    /// Get mutable reference to active panel (if any).
    pub fn active_panel_mut(&mut self) -> Option<&mut ProgramPane> {
        self.panels.get_mut(self.active_tab)
    }

    /// Get immutable reference to active panel (if any).
    #[allow(dead_code)]
    pub fn active_panel(&self) -> Option<&ProgramPane> {
        self.panels.get(self.active_tab)
    }

    /// Handle runner message (dispatch to correct panel by request_id).
    pub fn handle_runner_message(&mut self, msg: lutra_runner::channel::messages::ServerMessage) {
        // Iterate through all panels and let each check if message is for them
        let lutra_runner::channel::messages::ServerMessage::Response(mut res) = msg;
        for panel in &mut self.panels {
            match panel.handle_runner_response(res) {
                Ok(_) => break,    // found
                Err(r) => res = r, // not found, continue
            }
        }
    }

    /// Clear all tabs.
    pub fn clear_all(&mut self) {
        for mut panel in self.panels.drain(..) {
            panel.clear();
        }
        self.active_tab = 0;
    }

    /// Get number of open tabs.
    #[allow(dead_code)]
    pub fn tab_count(&self) -> usize {
        self.panels.len()
    }
}

impl Component for Tabs {
    fn handle(&mut self, action: Action) -> EventResult {
        // Delegate to active panel
        if let Some(panel) = self.active_panel_mut() {
            panel.handle(action)
        } else {
            EventResult::default()
        }
    }

    fn render(&self, frame: &mut Frame, area: Rect) {
        if self.panels.is_empty() {
            // No tabs open - show hint
            self.render_empty_hint(frame, area);
            return;
        }

        // Split area: tabs (1 line) + content
        let layout = Layout::vertical([
            Constraint::Length(1), // Tab bar
            Constraint::Min(0),    // Active panel content
        ])
        .split(area);

        let tabs_area = layout[0];
        let content_area = layout[1];

        // Render tab bar
        self.render_tabs(frame, tabs_area);

        // Render active panel
        if let Some(panel) = self.panels.get(self.active_tab) {
            panel.render(frame, content_area);
        }
    }
}

// Rendering code
impl Tabs {
    /// Render the tab bar.
    fn render_tabs(&self, frame: &mut Frame, area: Rect) {
        let mut spans = Vec::new();

        for (index, panel) in self.panels.iter().enumerate() {
            let path = panel.path();
            let path_str = self.format_tab_label(path, index, area.width);

            let style = if index == self.active_tab {
                Style::default()
                    .bg(style::COLOR_BG_PRIMARY)
                    .fg(if self.focused {
                        style::COLOR_FG_ACCENT
                    } else {
                        Color::White
                    })
                    .underlined()
            } else {
                Style::default().fg(Color::DarkGray)
            };

            // Add number prefix for shortcuts (1-9)
            spans.push(Span::raw(" "));
            if index < 9 {
                spans.push(Span::raw(format!("{}:", index + 1)));
            }
            spans.push(Span::styled(path_str, style));
        }

        let line = Line::from(spans);
        frame.render_widget(line, area);
    }

    /// Format a tab label, truncating if needed.
    fn format_tab_label(&self, path: &ir::Path, _index: usize, available_width: u16) -> String {
        let full = path.0.join("::");

        // Calculate max width per tab (rough estimate)
        let tab_count = self.panels.len().max(1);
        let max_width = (available_width as usize / tab_count).saturating_sub(4); // Account for number prefix and spacing

        if full.len() <= max_width {
            full
        } else {
            self.truncate_path(path, max_width)
        }
    }

    /// Truncate a path to fit within max_width.
    fn truncate_path(&self, path: &ir::Path, max_width: usize) -> String {
        let full = path.0.join("::");
        if full.len() <= max_width || max_width < 5 {
            return full;
        }

        let parts: Vec<_> = path.0.iter().map(|s| s.as_str()).collect();

        if parts.len() == 1 {
            // Single part: truncate end
            format!("{}...", &full[..max_width.saturating_sub(3)])
        } else {
            // Multiple parts: keep last (function name), truncate rest
            let last = parts.last().unwrap();
            let remaining_width = max_width.saturating_sub(last.len() + 5); // "...::fn"

            if remaining_width > 0 {
                format!("{}...::{}", &full[..remaining_width], last)
            } else {
                // Extreme case: just show truncated last part
                let start = last.len().saturating_sub(max_width - 3);
                format!("...{}", &last[start..])
            }
        }
    }

    /// Render empty hint when no tabs are open.
    fn render_empty_hint(&self, frame: &mut Frame, area: Rect) {
        let block = Block::new()
            .padding(Padding::symmetric(2, 1))
            .bg(style::COLOR_BG_SECONDARY);

        let paragraph = Paragraph::new(
            "Project compiles!\n\n\
            Run: select and press Enter\n\
            Exit: Esc or Ctrl-c",
        )
        .block(block);

        let area = area.centered(Constraint::Length(27), Constraint::Length(6));

        frame.render_widget(paragraph, area);
    }
}
