use std::path::Path;

use ratatui::prelude::*;

use crate::RunnerConfig;
use crate::project::CompileResult;

/// Status bar at the bottom of the screen.
pub struct StatusBar {
    /// Project path to display.
    project_path: String,
    /// Project status
    status: Status,
    /// Runner name.
    runner_name: String,
}

pub enum Status {
    Compiling,
    Error(usize),
    Ok,
}

impl StatusBar {
    /// Creates a new status bar.
    pub fn new(project_path: &Path, runner: &RunnerConfig) -> Self {
        let runner_name = match runner {
            RunnerConfig::Interpreter { .. } => "interpreter",
            RunnerConfig::Postgres { .. } => "postgres",
        };

        Self {
            project_path: project_path.display().to_string(),
            status: Status::Compiling,
            runner_name: runner_name.to_string(),
        }
    }

    pub fn set_compiling(&mut self) {
        self.status = Status::Compiling;
    }

    pub fn update(&mut self, compilation: &CompileResult) {
        self.status = match compilation {
            CompileResult::Success { .. } => Status::Ok,
            CompileResult::Failed { diagnostics } => Status::Error(diagnostics.len()),
            CompileResult::Pending => Status::Compiling,
        }
    }

    /// Render the status bar.
    pub fn render(&self, frame: &mut Frame, area: Rect) {
        let check_color = if matches!(self.status, Status::Error(_)) {
            Color::LightRed
        } else {
            Color::White
        };

        let check_msg = match self.status {
            Status::Compiling => "⟳ Compiling...".to_string(),
            Status::Error(cnt) => {
                format!("⚠ {cnt} error{}", if cnt == 1 { "" } else { "s" })
            }
            Status::Ok => "✓ OK".to_string(),
        };

        let line = Line::from(vec![
            Span::raw(" "),
            Span::styled(&self.project_path, Style::default().fg(Color::White)),
            Span::raw("  │  "),
            Span::styled(check_msg, Style::default().fg(check_color)),
            Span::raw("  │  "),
            Span::raw(format!("⚙ {}", self.runner_name)),
            Span::raw("  │  "),
            Span::styled(
                "F5:Run  1-9:Tabs  Ctrl+←→:Nav  x:Close  Tab:Switch  q:Quit",
                Style::default().fg(Color::Gray),
            ),
        ])
        .style(Style::default().bg(crate::style::COLOR_BG_ACCENT));

        frame.render_widget(line, area);
    }
}
