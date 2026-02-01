use ratatui::layout::{Constraint, Direction, Layout as RatatuiLayout, Rect};

use crate::project::ProjectState;

/// Which panel has keyboard focus.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PanelFocus {
    Definitions,
    Diagnostics,
    Run,
}

/// Which panel is shown in the center area.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CenterPanel {
    /// Show diagnostics (when there are errors/warnings).
    Diagnostics,
    /// Show run panel (when no diagnostics).
    Run,
}

/// Layout configuration for the interactive UI.
pub struct Layout {
    /// Which panel has keyboard focus.
    pub focus: PanelFocus,

    /// What to show in center area (determined by diagnostic count).
    pub center: CenterPanel,

    /// Width of definitions panel as percentage (0-100).
    pub definitions_width_percent: u16,
}

/// Computed layout areas for rendering.
pub struct LayoutAreas {
    /// Area for the definitions panel (left sidebar).
    pub definitions: Rect,
    /// Area for the center panel (diagnostics or run).
    pub center: Rect,
    /// Area for the status bar.
    pub status: Rect,
}

impl Default for Layout {
    fn default() -> Self {
        Self {
            focus: PanelFocus::Definitions,
            center: CenterPanel::Run,
            definitions_width_percent: 30,
        }
    }
}

impl Layout {
    /// Creates a new layout with default settings.
    pub fn new() -> Self {
        Self::default()
    }

    /// Update center panel based on diagnostic count.
    pub fn update(&mut self, project: &ProjectState) {
        let has_diagnostics = project.diagnostic_count() > 0;

        self.center = if has_diagnostics {
            CenterPanel::Diagnostics
        } else {
            CenterPanel::Run
        };

        // Adjust focus if current focus is on hidden panel
        if has_diagnostics && matches!(self.focus, PanelFocus::Run) {
            self.focus = PanelFocus::Diagnostics;
        }
        if !has_diagnostics && matches!(self.focus, PanelFocus::Diagnostics) {
            self.focus = PanelFocus::Run;
        }
    }

    /// Cycle focus to the next panel.
    pub fn cycle_focus(&mut self) {
        self.focus = match self.focus {
            PanelFocus::Definitions => match self.center {
                CenterPanel::Diagnostics => PanelFocus::Diagnostics,
                CenterPanel::Run => PanelFocus::Run,
            },
            PanelFocus::Diagnostics => PanelFocus::Definitions,
            PanelFocus::Run => PanelFocus::Definitions,
        };
    }

    /// Compute layout areas for the given terminal size.
    pub fn compute_areas(&self, area: Rect) -> LayoutAreas {
        // Split vertically: main area + status bar
        let vertical = RatatuiLayout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Min(0),    // Main area
                Constraint::Length(1), // Status bar
            ])
            .split(area);

        let main_area = vertical[0];
        let status = vertical[1];

        // Split main area horizontally: definitions panel + center panel
        let horizontal = RatatuiLayout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Percentage(self.definitions_width_percent),
                Constraint::Percentage(100 - self.definitions_width_percent),
            ])
            .split(main_area);

        LayoutAreas {
            definitions: horizontal[0],
            center: horizontal[1],
            status,
        }
    }
}
