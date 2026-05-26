use std::path;

use crossterm::event;

use crate::cell::BoundInput;
use crate::commands::CommandRegistry;
use crate::input;
use crate::keybindings::{KeyBindings, KeyContext};
use crate::panels::StatusBar;
use crate::project::{CompileResult, ProjectState};
use crate::repl::ReplPane;
use crate::runner;
use crate::terminal::Rect;
use crate::terminal::{Action, ActionResult, Line, Style, View};

/// The interactive shell application state.
pub struct Shell {
    /// Project state (sources, compilation results).
    pub project: ProjectState,

    /// Runner session state.
    runner: runner::RunnerSession,

    /// Keybindings configuration.
    keybindings: KeyBindings,

    /// Commands.
    pub commands: CommandRegistry,

    /// REPL pane.
    pub repl: ReplPane,

    /// Status bar.
    status: StatusBar,

    startup_done: bool,

    /// Scroll offset into wrapped diagnostics lines.
    diagnostics_scroll: usize,
}

impl Shell {
    pub fn new(
        project_path: Option<path::PathBuf>,
        repr: lutra_compiler::ProgramRepr,
        runner_client: lutra_runner::channel::ClientSender,
    ) -> Self {
        let mut s = Self {
            project: ProjectState::new(),
            runner: runner::RunnerSession::new(runner_client, repr),
            keybindings: KeyBindings::new(),
            commands: CommandRegistry::default(),
            repl: ReplPane::new(),
            status: StatusBar::new(project_path, repr),
            startup_done: false,
            diagnostics_scroll: 0,
        };
        s.commands.register_buildin();
        s.repl.completions.update_commands(s.commands.get());
        s
    }

    fn key_context(&self) -> KeyContext {
        if self.has_diagnostics() {
            KeyContext::Diagnostics
        } else if !self.repl.completions.is_empty() && self.repl.cursor.on_program() {
            KeyContext::Completions
        } else {
            KeyContext::Cell(self.repl.cursor.stage)
        }
    }

    fn recompile(&mut self) {
        self.status.set_compiling();
        self.project.recompile();
        self.status.update(&self.project);
        self.repl.completions.update_project(&self.project);
        self.diagnostics_scroll = 0;

        self.repl.draft.program_ty = None;
        self.repl.draft.argument = None;
        self.runner.release();
        if self.repl.cursor.on_argument() {
            self.repl.cursor.stage = crate::cell::CellStage::Program;
        }
    }

    fn has_diagnostics(&self) -> bool {
        matches!(
            &self.project.compilation,
            CompileResult::Failed { diagnostics } if !diagnostics.is_empty()
        )
    }

    fn render_diagnostics(&self, cols: u16) -> View<'_> {
        let CompileResult::Failed { diagnostics } = &self.project.compilation else {
            return View::new();
        };

        let mut view = View::new();
        view.push_line(Line::new(format!(
            "{} compile error{}",
            diagnostics.len(),
            if diagnostics.len() == 1 { "" } else { "s" }
        )));
        view.push_line(Line::empty());

        for (i, diagnostic) in diagnostics.iter().enumerate() {
            if i > 0 {
                view.push_line(Line::empty());
            }
            for text_line in diagnostic.display().lines() {
                view.push_line(text_line);
            }
        }

        view.wrap(cols, 0);
        view
    }

    pub(crate) fn render(&self, area: Rect) -> View<'_> {
        let (body_area, _status) = area.split_bottom(2);

        let mut view = if self.has_diagnostics() {
            let diagnostics = self.render_diagnostics(area.cols).lines;
            let overflow = diagnostics.len().saturating_sub(body_area.rows as usize);

            View::from(
                diagnostics
                    .into_iter()
                    .skip(self.diagnostics_scroll.min(overflow))
                    .take(body_area.rows as usize)
                    .collect::<Vec<_>>(),
            )
        } else {
            let completions = self.repl.completions.view();
            let cell = self.repl.visible_cell();
            cell.render(body_area, Some(&self.repl.cursor), completions)
        };

        view.truncate(body_area.rows as usize);
        view.push_line(Line::styled(
            "─".repeat(area.cols.min(20) as usize),
            Style::muted(),
        ));
        view.push_line(Line::styled(
            self.status.view(self.key_context()),
            Style::muted(),
        ));
        view
    }

    fn scroll_diagnostics_up(&mut self, amount: usize) {
        self.diagnostics_scroll = self.diagnostics_scroll.saturating_sub(amount);
    }

    fn scroll_diagnostics_down(&mut self, amount: usize) {
        self.diagnostics_scroll = self.diagnostics_scroll.saturating_add(amount);
    }

    pub(crate) fn cmd_pull(&mut self) -> ActionResult {
        let CompileResult::Success { .. } = &self.project.compilation else {
            return self
                .repl
                .commit_error("cannot /pull while project has compile errors");
        };

        match self.runner.pull() {
            Ok(()) => ActionResult::redraw(),
            Err(err) => self.repl.commit_error(err),
        }
    }

    pub(crate) fn cmd_pipe(&mut self) -> ActionResult {
        let Some(output) = self.repl.last_successful_output() else {
            return self
                .repl
                .commit_error("no successful history output to bind");
        };
        let bound = output.as_bound_input();
        self.bind_input(bound, "x".into())
    }

    fn inspect_output(&mut self) -> ActionResult {
        let Some(output) = self.repl.visible_cell().as_output() else {
            return ActionResult::default();
        };
        let input = output.as_bound_input();

        let ty = output.cell.program_ty.as_ref().unwrap();
        let layout = output.cell.output_layout();
        let prompt = self.repl.cursor.as_program(&ty.output, &ty.defs, layout);
        self.bind_input(input, prompt)
    }

    fn bind_input(&mut self, input: BoundInput, prompt: String) -> ActionResult {
        let res = if self.repl.cursor.history_cursor.is_none() {
            self.repl.commit_cell(false)
        } else {
            ActionResult::default()
        };
        self.repl.draft.bound_input = Some(input);
        let prompt_len = prompt.len();
        self.repl.draft.prompt = prompt;
        self.repl.cursor.goto_draft(Some(prompt_len));
        self.repl.completions.update_query("");
        res.and(ActionResult::redraw())
    }

    fn enter_shell_command_mode(&mut self) {
        self.repl.draft.bound_input = None;
        self.repl.draft.program_ty = None;
        self.repl.draft.argument = None;
        self.repl.draft.output = None;
        self.repl.cursor.stage = crate::cell::CellStage::Program;
    }

    fn submit_prompt(&mut self) -> ActionResult {
        let r = self.repl.checkout_from_history();
        self.runner.release();

        let prompt = self.repl.draft.prompt.trim().to_string();
        if prompt.is_empty() {
            return r;
        }

        // try: command
        if let Some(res) = self.commands.parse_and_find(&prompt) {
            self.enter_shell_command_mode();

            let cmd = match res {
                Err(message) => return r.and(self.repl.commit_error(message)),
                Ok(c) => c,
            };

            return r.and(if cmd.ty().input.is_unit() {
                // run immediately
                cmd.run(self, vec![])
            } else {
                // prompt argument
                let mut argument = input::InputPane::new(&cmd.ty().input, &cmd.ty().defs, "Ok");
                if let Some(value) = cmd.default_input() {
                    argument.set_value(value.clone())
                }
                self.repl.draft.program_ty = Some(cmd.ty().clone());
                self.repl.draft.argument = Some(argument);
                self.repl.cursor.stage = crate::cell::CellStage::Argument;
                ActionResult::redraw()
            });
        }

        let CompileResult::Success { project } = &self.project.compilation else {
            return r;
        };

        // try: inspect
        if self.repl.try_describe(project) {
            return r.and(self.repl.commit_cell(false));
        }

        let bound_input = self.repl.draft.bound_input.clone();

        // base case: lutra program
        let program_source = self.repl.draft.get_program_source();
        let params = lutra_compiler::CompileParams::new(program_source, self.runner.repr());
        let res = lutra_compiler::compile(project, &params);
        if let Err(err) = res {
            return r.and(self.repl.commit_compile_error(err.to_string()));
        }
        let (program, ty) = res.unwrap();

        if ty.input.is_unit() || bound_input.is_some() {
            // execute immediately
            self.repl.draft.argument = None;
            self.repl.draft.program_ty = Some(ty);
            let argument = bound_input.map(|x| x.data).unwrap_or_default();
            if let Err(err) = self.runner.run(&program, &argument) {
                return r.and(self.repl.commit_error(err));
            }
        } else {
            // prompt argument
            if let Err(err) = self.runner.prepare(&program) {
                return r.and(self.repl.commit_error(err));
            }
            let argument_pane = input::InputPane::new(&ty.input, &ty.defs, "Run");
            self.repl.draft.argument = Some(argument_pane);
            self.repl.cursor.stage = crate::cell::CellStage::Argument;
            self.repl.draft.program_ty = Some(ty);
        }
        r.and(ActionResult::redraw())
    }

    fn submit_argument(&mut self) -> ActionResult {
        // get argument
        let res = self.repl.draft.get_argument_bin();
        let Some(argument) = res else {
            return ActionResult::default();
        };

        // try: command
        let prompt = self.repl.draft.prompt.trim().to_string();
        if let Some(res) = self.commands.parse_and_find(&prompt) {
            let cmd = match res {
                Err(message) => return self.repl.commit_error(message),
                Ok(c) => c,
            };
            return cmd.run(self, argument);
        }

        // base case: lutra program
        match self.runner.execute(&argument) {
            Ok(()) => ActionResult::redraw(),
            Err(e) => self.repl.commit_error(e),
        }
    }

    fn handle_runner_event(&mut self, event: runner::RunnerEvent) -> ActionResult {
        match event.response {
            runner::proto::ResponseKind::Execute(result) => {
                let result = result.0.map_err(|e| e.to_string());
                self.repl.set_execution_result(result, event.duration);
                ActionResult::redraw()
            }
            runner::proto::ResponseKind::Schema(result) => {
                let CompileResult::Success { project } = &self.project.compilation else {
                    return ActionResult::default();
                };
                let schema = match result.0 {
                    Err(e) => {
                        return self.repl.commit_error(format!("pulling schema: {e}"));
                    }
                    Ok(s) => s,
                };
                let outcome = lutra_project_tools::update_schema(project, &schema);
                let outcome = match outcome {
                    Err(e) => return self.repl.commit_error(e.to_string()),
                    Ok(v) => v,
                };
                let msg = if let Some(written_path) = outcome {
                    View::from_text(&format!("Written: {}", written_path.display())).to_owned()
                } else {
                    View::from_text(&schema).to_owned()
                };
                self.repl.commit_message(msg)
            }
            _ => ActionResult::default(),
        }
    }

    pub(crate) fn handle(&mut self, action: Action) -> ActionResult {
        match action {
            Action::Exit => ActionResult::shutdown(),
            Action::ExitIfEmpty => {
                if self.repl.draft.is_empty() {
                    ActionResult::shutdown()
                } else {
                    ActionResult::default()
                }
            }
            Action::SubmitPrompt => self.submit_prompt(),
            Action::SubmitArgument => self.submit_argument(),
            Action::InspectOutput => self.inspect_output(),
            Action::ClearCell => {
                self.runner.release();
                self.repl.handle(Action::ClearCell)
            }

            Action::ScrollDiagnosticsUp => {
                self.scroll_diagnostics_up(10);
                ActionResult::redraw()
            }
            Action::ScrollDiagnosticsDown => {
                self.scroll_diagnostics_down(10);
                ActionResult::redraw()
            }

            Action::SourceUpdated(source) => {
                self.project.source = Some(source);
                self.recompile();

                let startup = !self.startup_done;
                self.startup_done = true;

                if startup && let CompileResult::Success { project } = &self.project.compilation {
                    self.repl.draft.prompt = "project".to_string();
                    self.repl.try_describe(project);
                    return self.repl.commit_cell(false).and(ActionResult::redraw());
                }

                ActionResult::redraw()
            }

            Action::RunnerMessage(response) => match self.runner.handle_response(response) {
                Some(event) => self.handle_runner_event(event),
                None => ActionResult::default(),
            },

            Action::Terminal(event::Event::Resize(..)) => ActionResult::redraw(),
            Action::Terminal(event) => {
                // debug
                if let event::Event::Key(key) = &event {
                    let ctx = self.key_context();
                    self.status.debug = Some(format!("{key:?}"));
                    if let Some(action) = self.keybindings.process_key(*key, ctx) {
                        return ActionResult::action(action);
                    }
                }

                // fallback - pass to ReplPane
                self.repl.handle(Action::Terminal(event))
            }

            // fallback - pass to ReplPane.
            action => self.repl.handle(action),
        }
    }
}
