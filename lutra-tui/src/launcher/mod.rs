mod project;
mod runner;

pub use runner::RunnerType;

use std::path::PathBuf;

use crossterm::event;
use ratatui::prelude::*;
use ratatui::widgets::Borders;

use crate::terminal::{Action, ActionResult, Component};
use project::ProjectSelector;
use runner::RunnerPane;

/// Runs the launcher TUI and returns the selected project and runner configuration.
pub fn run(
    initial_project: Option<PathBuf>,
    initial_runner: Option<RunnerParams>,
) -> anyhow::Result<Option<LauncherResult>> {
    let mut app = LauncherApp::new(initial_project, initial_runner)?;

    // Enter terminal
    let mut term = ratatui::init();

    // Listen for terminal events
    let (action_tx, action_rx) = std::sync::mpsc::channel();
    let _event_thread = crate::terminal::spawn_event_reader(action_tx);

    // Run
    let r = crate::terminal::run_action_loop(&mut app, &mut term, action_rx);

    // Restore terminal
    ratatui::restore();

    r?;

    Ok(app.get_result())
}

/// Result of the launcher containing the selected project and runner configuration.
#[derive(Debug, Clone)]
pub struct LauncherResult {
    pub project_path: PathBuf,
    pub runner_params: RunnerParams,
}

/// Parameters for configuring a runner.
#[derive(Debug, Clone)]
pub enum RunnerParams {
    Interpreter,
    PostgreSQL(String),
    DuckDB(String),
}

/// State machine for the launcher.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Focus {
    Project,
    Runner,
    Done,
}

/// The main launcher application.
pub struct LauncherApp {
    focus: Focus,

    project: ProjectSelector,
    runner: RunnerPane,

    launch: bool,
}

impl LauncherApp {
    pub fn new(
        initial_project: Option<PathBuf>,
        initial_runner: Option<RunnerParams>,
    ) -> anyhow::Result<Self> {
        Ok(Self {
            focus: Focus::Project,
            project: ProjectSelector::new(initial_project)?,
            runner: RunnerPane::new_with_initial(initial_runner),
            launch: false,
        })
    }

    /// Check if we have both project and runner configured.
    fn is_ready(&self) -> bool {
        self.project.has_selection() && self.runner.has_selection()
    }

    /// Build the final result.
    fn get_result(&self) -> Option<LauncherResult> {
        if !self.launch {
            return None;
        }

        let project_path = self.project.get_selection()?;
        let runner_params = self.runner.to_runner_params()?;

        Some(LauncherResult {
            project_path,
            runner_params,
        })
    }

    fn cycle_focus(&mut self) {
        self.focus = match self.focus {
            Focus::Project => Focus::Runner,
            Focus::Runner => Focus::Done,
            Focus::Done => Focus::Project,
        };

        self.project.set_focused(self.focus == Focus::Project);
        self.runner.set_focused(self.focus == Focus::Runner);
    }

    fn render_launch_button(&self, frame: &mut Frame, area: Rect) {
        let is_focused = matches!(self.focus, Focus::Done);

        // button
        let text = "[ Launch ]";

        let mut style = Style::default().fg(Color::DarkGray);
        if is_focused {
            style = style.bg(crate::style::COLOR_BG_ACCENT);
        }
        if self.is_ready() {
            style = style.fg(Color::White).bold();
        }
        let mut line = Line::from(Span::styled(text, style));

        // missing text
        let mut missing = Vec::with_capacity(2);
        if !self.project.has_selection() {
            missing.push("project");
        }
        if !self.runner.has_selection() {
            missing.push("runner");
        }
        if !missing.is_empty() {
            line.push_span(Span::styled(
                format!(" (missing {})", missing.join(" and ")),
                Style::default().fg(Color::DarkGray),
            ));
        };

        frame.render_widget(line, area);
    }
}

impl Component for LauncherApp {
    fn render(&self, frame: &mut Frame, area: Rect) {
        // Fill background
        frame.render_widget(
            ratatui::widgets::Block::default().bg(crate::style::COLOR_BG_SECONDARY),
            area,
        );

        // Layout
        const MAX_WIDTH: u16 = 100;
        const MAX_HEIGHT: u16 = 50;

        let main_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Min(0),
                Constraint::Max(MAX_HEIGHT),
                Constraint::Min(0),
                Constraint::Length(1),
            ])
            .split(area);
        let help_area = main_layout[3];

        let centered = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Min(0),
                Constraint::Max(MAX_WIDTH),
                Constraint::Min(0),
            ])
            .split(main_layout[1]);
        let content_area = centered[1];

        // Main block
        let block =
            crate::style::panel_primary(" Launcher ", false).borders(Borders::TOP | Borders::LEFT);
        let inner = block.inner(content_area);
        frame.render_widget(block, content_area);

        fn section_constr(is_focused: bool) -> Constraint {
            if is_focused {
                Constraint::Min(10)
            } else {
                Constraint::Length(2)
            }
        }
        let area_panes = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(1), // Spacer
                section_constr(self.focus == Focus::Project),
                Constraint::Length(1), // Spacer
                section_constr(self.focus == Focus::Runner),
                Constraint::Length(1), // Spacer
                Constraint::Length(3), // Launch button
            ])
            .split(inner);

        // Render sub-panes
        self.project.render(frame, area_panes[1]);
        self.runner.render(frame, area_panes[3]);
        self.render_launch_button(frame, area_panes[5]);

        // Render help bar
        let help_text = match self.focus {
            Focus::Project => "Enter: Select | ↑↓: Navigate | Tab: Switch panel | Esc: Exit",
            Focus::Runner => {
                if self.runner.is_configuring() {
                    "Enter: Confirm | Type to enter config | Tab: Switch panel | Esc: Back"
                } else {
                    "Enter: Select | ↑↓: Navigate | Tab: Switch panel | Esc: Exit"
                }
            }
            Focus::Done => "Enter: Select | Tab: Switch panel | Esc: Exit",
        };
        frame.render_widget(
            ratatui::widgets::Paragraph::new(help_text).style(
                Style::default()
                    .bg(crate::style::COLOR_BG_ACCENT)
                    .fg(Color::White),
            ),
            help_area,
        );
    }

    fn handle(&mut self, action: Action) -> ActionResult {
        match action {
            Action::CycleFocus => {
                self.cycle_focus();
                ActionResult::redraw()
            }
            Action::Terminal(event::Event::Key(key)) => {
                // Tab to switch focus
                if key.code == event::KeyCode::Tab {
                    return ActionResult::action(Action::CycleFocus);
                }

                // Esc handling
                if key.code == event::KeyCode::Esc {
                    if self.runner.is_configuring() {
                        // When configuring, go back to runner selection
                        self.runner.cancel_configuration();
                        return ActionResult::redraw();
                    } else {
                        // Otherwise, exit the launcher
                        return ActionResult::shutdown();
                    }
                }

                // Delegate to focused component
                match self.focus {
                    Focus::Project => self
                        .project
                        .handle(Action::Terminal(event::Event::Key(key))),

                    Focus::Runner => self.runner.handle(Action::Terminal(event::Event::Key(key))),
                    Focus::Done => {
                        // Handle button interactions
                        if let event::KeyCode::Enter = key.code
                            && self.is_ready()
                        {
                            self.launch = true;
                            return ActionResult::shutdown();
                        }
                        ActionResult::default()
                    }
                }
            }
            Action::Terminal(event::Event::Resize(_, _)) => ActionResult::redraw(),
            _ => ActionResult::default(),
        }
    }
}
