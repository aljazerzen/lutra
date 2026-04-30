use std::path;

use crate::RunnerConfig;
use crate::keybindings::KeyContext;
use crate::project::{CompileResult, ProjectState};

/// Status bar at the bottom of the screen.
pub struct StatusBar {
    /// Project path to display.
    project_path: String,
    /// Project status
    status: Status,
    /// Runner name.
    runner_name: String,
    /// Debug
    pub debug: Option<String>,
}

pub enum Status {
    Compiling,
    Error(usize),
    Ok,
}

impl StatusBar {
    /// Creates a new status bar.
    pub fn new(project: Option<path::PathBuf>, runner: &RunnerConfig) -> Self {
        let runner_format = format!("{:?}", runner.format);

        let project_path = project
            .map(|project| {
                let path = project.canonicalize();
                path.as_deref().unwrap_or(&project).display().to_string()
            })
            .unwrap_or_default();

        Self {
            project_path,
            status: Status::Compiling,
            runner_name: runner_format,
            debug: None,
        }
    }

    pub fn set_compiling(&mut self) {
        self.status = Status::Compiling;
    }

    pub fn update(&mut self, project: &ProjectState) {
        if let Some(source) = &project.source {
            self.project_path = source.get_root().display().to_string();
        }
        self.status = match &project.compilation {
            CompileResult::Success { .. } => Status::Ok,
            CompileResult::Failed { diagnostics } => Status::Error(diagnostics.len()),
            CompileResult::Pending => Status::Compiling,
        }
    }

    pub fn view(&self, ctx: KeyContext) -> String {
        let check_msg = match self.status {
            Status::Compiling => "compiling".to_string(),
            Status::Error(cnt) => {
                format!("{cnt} error{}", if cnt == 1 { "" } else { "s" })
            }
            Status::Ok => "ok".to_string(),
        };

        let mut r = format!(
            "{} ⸱ {} ⸱ {} ⸱ {ctx:?}",
            self.project_path, check_msg, self.runner_name
        );
        if let Some(debug) = &self.debug {
            r += " ⸱ ";
            r += debug;
        }
        r
    }
}
