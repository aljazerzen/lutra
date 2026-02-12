mod error;
mod input;
mod output;
mod source;

pub use error::ErrorPane;
pub use input::InputPane;
pub use output::OutputPane;

use lutra_bin::{ir, rr};
use lutra_compiler::Project;
use lutra_runner::binary::messages::Response;
use ratatui::prelude::*;
use ratatui::widgets::{Block, Padding};
use std::rc::Rc;
use std::time::Instant;

use crate::RunnerConfig;
use crate::panels::program::source::SourcePane;
use crate::style;
use crate::terminal::{Action, ActionResult, Component};

/// Center panel for executing a selected definition.
pub struct ProgramPane {
    // -- state --
    path: ir::Path,

    focused: bool,
    stage: RunStage,

    /// Currently prepared program ID
    program: Option<Rc<rr::Program>>,
    program_ty: Option<Rc<rr::ProgramType>>,
    prepared_program_id: Option<u32>,

    /// Currently running execution
    executing: Option<RunningExecution>,

    /// Auto-run configuration
    auto_run: AutoRunState,

    // -- handles --
    /// Runner client for sending execution commands
    runner_client: lutra_runner::channel::ClientSender,

    // -- components --
    source_pane: Option<SourcePane>,
    input_pane: Option<InputPane>,
    output_pane: Option<OutputPane>,
    error_pane: Option<ErrorPane>,
}

struct RunningExecution {
    request_id: u32,
    start_time: Instant,
}

/// Execution state for a program.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RunStage {
    /// Waiting for user to provide input.
    Input,
    /// Currently executing.
    Running,
    /// Execution completed.
    Output,
    /// Execution failed.
    Error,
}

impl ProgramPane {
    /// Creates a new program pane.
    pub fn new(
        path: ir::Path,
        runner_client: lutra_runner::channel::ClientSender,
        project: &Project,
        runner: &RunnerConfig,
    ) -> Self {
        let source_pane = SourcePane::new(&path, project);

        let mut this = Self {
            path,
            program: None,
            program_ty: None,

            focused: false,
            stage: RunStage::Input,

            runner_client,
            prepared_program_id: None,
            executing: None,

            auto_run: AutoRunState::new(),

            input_pane: None,
            output_pane: None,
            error_pane: None,
            source_pane,
        };

        if let Err(e) = this.compile_and_prepare(project, runner) {
            this.stage = RunStage::Error;
            this.error_pane = Some(ErrorPane::new(e));
        }
        this
    }

    /// Get the program path for this panel.
    pub fn path(&self) -> &ir::Path {
        &self.path
    }

    fn compile_and_prepare(
        &mut self,
        project: &Project,
        runner: &RunnerConfig,
    ) -> Result<(), String> {
        let program = self.path.0.join("::");

        let format = match runner {
            RunnerConfig::Interpreter { .. } => lutra_compiler::ProgramFormat::BytecodeLt,
            RunnerConfig::Postgres { .. } => lutra_compiler::ProgramFormat::SqlPg,
            RunnerConfig::DuckDB { .. } => lutra_compiler::ProgramFormat::SqlDuckdb,
        };

        let (program, ty) =
            lutra_compiler::compile(project, &program, None, format).map_err(|e| e.to_string())?;

        // Wrap in Rc for sharing across components
        let program = Rc::new(program);
        let program_ty = Rc::new(ty);

        // Prepare program on runner (returns program_id)
        let program_id = self
            .runner_client
            .prepare(&program)
            .map_err(|e| format!("Failed to prepare program: {}", e))?;

        // Create panes
        self.input_pane = Some(InputPane::new(&program_ty.input, &program_ty.defs));
        self.output_pane = None;
        self.error_pane = None;

        // Set programs
        self.program = Some(program);
        self.program_ty = Some(program_ty);
        self.prepared_program_id = Some(program_id);

        self.stage = RunStage::Input;

        Ok(())
    }

    pub fn set_focused(&mut self, focused: bool) {
        self.focused = focused;
        if let Some(input) = &mut self.input_pane {
            input.set_focused(focused);
        }
    }

    /// Check if a form input field currently has cursor focus.
    pub fn is_form_field_focused(&self) -> bool {
        self.focused && matches!(self.stage, RunStage::Input)
    }

    /// Toggle auto-run enabled/disabled. Returns new enabled state.
    pub fn toggle_auto_run(&mut self) -> bool {
        self.auto_run.toggle()
    }

    /// Clears the current program.
    pub fn clear(&mut self) {
        // Release prepared program (ignore errors)
        if let Some(program_id) = self.prepared_program_id.take() {
            let _ = self.runner_client.release(program_id);
        }

        self.input_pane = None;
        self.output_pane = None;
        self.error_pane = None;
        self.executing = None;
    }

    /// Recompile the program with new project.
    /// Returns true if auto-run should execute.
    pub fn recompile(&mut self, project: &Project, runner: &RunnerConfig) -> Result<bool, String> {
        // Clear previous preparation
        if let Some(program_id) = self.prepared_program_id.take() {
            let _ = self.runner_client.release(program_id);
        }

        // Compile new program
        self.compile_and_prepare(project, runner)?;

        // Check if input type changed
        if let Some(program_ty) = &self.program_ty {
            let type_changed = self.auto_run.check_type_change(&program_ty.input);

            if type_changed && self.auto_run.enabled {
                // Type changed, auto-run suspended
                return Ok(false);
            }
        }

        // Return whether we should auto-run
        Ok(self.auto_run.should_auto_run())
    }

    /// Prepare and execute the program.
    pub fn prepare_and_execute(&mut self) -> Result<(), String> {
        let Some(input_pane) = &self.input_pane else {
            return Err("No input pane available".to_string());
        };

        let Some(program_ty) = &self.program_ty else {
            return Err("Program not compiled".to_string());
        };

        let Some(program_id) = self.prepared_program_id else {
            return Err("No program prepared".to_string());
        };

        // Get input and encode
        let input = input_pane
            .get_value()
            .encode(&program_ty.input, &program_ty.defs)
            .map_err(|e| format!("Encoding input: {e}"))?;

        // Send execute command (returns request_id)
        let request_id = self
            .runner_client
            .execute(program_id, &input)
            .map_err(|e| format!("Failed to send execute: {}", e))?;

        // Store executing state
        self.executing = Some(RunningExecution {
            request_id,
            start_time: Instant::now(),
        });
        self.stage = RunStage::Running;

        Ok(())
    }

    /// Handle a message from the runner server.
    pub fn handle_runner_response(&mut self, response: Response) -> Result<(), Response> {
        // Check if this response is for our current execution
        let Some(running) = &self.executing else {
            return Err(response);
        };
        if running.request_id != response.request_id {
            return Err(response);
        }

        // Calculate duration
        let duration = running.start_time.elapsed();

        // Clear executing state
        self.executing = None;

        // Process result
        match response.result {
            lutra_runner::channel::messages::Result::Ok(output) => {
                // SAFETY: program_ty must be Some if we're executing
                let program_ty = self
                    .program_ty
                    .clone()
                    .expect("program_ty should be set during execution");

                // Resume auto-run if it was suspended
                self.auto_run.on_successful_run();

                self.output_pane = Some(OutputPane::new(program_ty, output, duration));
                self.stage = RunStage::Output;
            }
            lutra_runner::channel::messages::Result::Err(err) => {
                // Format error message with code if present
                let error_msg = match &err.code {
                    Some(code) => format!("{}: {}", code, err.message),
                    None => err.message.clone(),
                };
                self.error_pane = Some(ErrorPane::new(error_msg));
                self.stage = RunStage::Error;
            }
        }
        Ok(())
    }

    /// Set error state directly.
    pub fn set_error(&mut self, error: String) {
        self.error_pane = Some(ErrorPane::new(error));
        self.stage = RunStage::Error;
    }
}

impl Component for ProgramPane {
    fn handle(&mut self, action: Action) -> ActionResult {
        // Special handling for ReturnToInput action
        if matches!(action, Action::ReturnToInput) {
            self.stage = RunStage::Input;
            return ActionResult::redraw();
        }

        // Delegate to current stage component
        match self.stage {
            RunStage::Input => self
                .input_pane
                .as_mut()
                .map(|s| s.handle(action))
                .unwrap_or_default(),
            RunStage::Output => self
                .output_pane
                .as_mut()
                .map(|s| s.handle(action))
                .unwrap_or_default(),
            RunStage::Error => self
                .error_pane
                .as_mut()
                .map(|s| s.handle(action))
                .unwrap_or_default(),
            _ => ActionResult::default(),
        }
    }

    /// Render the panel to the given area.
    fn render(&self, frame: &mut Frame, area: Rect) {
        let block = Block::default()
            .bg(style::COLOR_BG_PRIMARY)
            .padding(Padding::new(1, 0, 1, 0));
        let inner = block.inner(area);
        frame.render_widget(block, area);
        let area = inner;

        // Split: auto-run status (1 line) + rest
        let layout = Layout::vertical([
            Constraint::Min(0),    // Source + stage
            Constraint::Length(1), // Auto-run status
        ])
        .split(area);

        // Render auto-run status at the bottom
        let (text, color) = self.auto_run.status_text();
        let status_line = Line::from(Span::styled(text, Style::default().fg(color)));
        frame.render_widget(status_line, layout[1]);

        let area = layout[0];

        // Split body area: source (if exists) + current stage
        let area = if let Some(source_pane) = &self.source_pane {
            // Use exact height needed for source pane, rest for stage
            let source_height = source_pane.height();
            let chunks = Layout::vertical([
                Constraint::Length(source_height), // Source pane (exact height)
                Constraint::Length(1),             // Spacer
                Constraint::Min(0),                // Stage pane (remaining space)
            ])
            .split(area);

            source_pane.render(frame, chunks[0]);

            chunks[2]
        } else {
            // No source, use full area for stage
            area
        };

        // Render current stage (Input/Running/Output/Error) in stage_area
        match self.stage {
            RunStage::Input => {
                if let Some(stage) = &self.input_pane {
                    stage.render(frame, area);
                }
            }
            RunStage::Running => {
                frame.render_widget(Text::raw("Running..."), area);
            }
            RunStage::Output => {
                if let Some(stage) = &self.output_pane {
                    stage.render(frame, area);
                }
            }
            RunStage::Error => {
                if let Some(stage) = &self.error_pane {
                    stage.render(frame, area);
                }
            }
        }
    }
}

/// Auto-run state for a program pane.
#[derive(Debug, Clone)]
struct AutoRunState {
    /// Whether auto-run is enabled by user.
    enabled: bool,

    /// Last known input type (for detecting changes).
    last_input_ty: Option<ir::Ty>,

    /// Whether auto-run is currently suspended due to type change.
    suspended: bool,
}

impl AutoRunState {
    fn new() -> Self {
        Self {
            enabled: false,
            last_input_ty: None,
            suspended: false,
        }
    }

    /// Toggle auto-run enabled/disabled. Returns new enabled state.
    fn toggle(&mut self) -> bool {
        self.enabled = !self.enabled;
        if self.enabled {
            // Clear suspension when manually enabling
            self.suspended = false;
        }
        self.enabled
    }

    /// Check if input type changed. If changed and enabled, suspends auto-run.
    /// Returns true if type changed.
    fn check_type_change(&mut self, new_input_ty: &ir::Ty) -> bool {
        let changed = match &self.last_input_ty {
            Some(old_ty) => old_ty != new_input_ty,
            None => false,
        };

        if changed && self.enabled {
            self.suspended = true;
        }

        self.last_input_ty = Some(new_input_ty.clone());
        changed
    }

    /// Returns true if auto-run should execute.
    fn should_auto_run(&self) -> bool {
        self.enabled && !self.suspended
    }

    /// Called after a successful manual run. Resumes auto-run if suspended.
    fn on_successful_run(&mut self) {
        if self.suspended && self.enabled {
            self.suspended = false;
        }
    }

    /// Returns status text and color for UI display.
    fn status_text(&self) -> (&str, Color) {
        if !self.enabled {
            ("⏹ Auto-run disabled", Color::DarkGray)
        } else if self.suspended {
            ("⏸ Auto-run (type changed)", Color::Yellow)
        } else {
            ("▶︎ Auto-run enabled", Color::White)
        }
    }
}
