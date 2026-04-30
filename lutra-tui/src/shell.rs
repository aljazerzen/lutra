use std::collections::VecDeque;
use std::io;
use std::path;

use crossterm::event;
use crossterm::terminal;

use crate::commands::CommandRegistry;
use crate::input;
use crate::keybindings::{KeyBindings, KeyContext};
use crate::panels::StatusBar;
use crate::project::{CompileResult, ProjectState};
use crate::renderer::ShellRenderer;
use crate::repl::ReplPane;
use crate::runner;
use crate::terminal::Rect;
use crate::terminal::{
    Action, ActionResult, Component, Effect, Line, Span, Style, View, wrap_line,
};
use crate::watcher::FileWatcher;

/// Starts the interactive shell environment.
///
/// Watches the project directory for changes and provides a terminal-native
/// shell for evaluating expressions.
pub fn run_shell(
    project_path: Option<path::PathBuf>,
    runner_cfg: runner::RunnerConfig,
    runner: lutra_runner::channel::Client,
    runner_thread: std::thread::JoinHandle<()>,
) -> anyhow::Result<()> {
    // Create action channel
    let (action_tx, action_rx) = std::sync::mpsc::channel();

    // Init event sources
    let watcher = FileWatcher::new(project_path.clone(), action_tx.clone())?;
    let runner = runner::RunnerProxy::try_new(runner, runner_thread, action_tx.clone())?;

    // Create app
    let mut app = Shell::new(project_path, runner_cfg, runner.get_client());

    // Enter terminal
    let mut renderer = ShellRenderer::new()?;
    let _event_thread = crate::terminal::spawn_event_reader(action_tx);

    // Print welcome message
    renderer.print(welcome_view())?;

    // Initial render
    let area: Rect = terminal::size()?.into();
    renderer.render(app.view(area), area)?;

    // Run the app
    let run_result = run_shell_loop(&mut app, &mut renderer, action_rx);

    // Always restore terminal state
    renderer.restore()?;

    // Stop app & event sources
    drop(app);
    drop(watcher);
    runner.join();

    Ok(run_result?)
}

fn run_shell_loop(
    app: &mut Shell,
    renderer: &mut ShellRenderer,
    action_rx: std::sync::mpsc::Receiver<Action>,
) -> io::Result<()> {
    while let Ok(action) = action_rx.recv() {
        let mut redraw = false;
        let mut effects = Vec::new();
        let mut queue = VecDeque::new();
        queue.push_back(action);

        while let Some(action) = queue.pop_front() {
            let res = app.handle(action);
            let (actions, new_effects) = res.into_parts();
            effects.extend(new_effects);
            queue.extend(actions);
        }

        for effect in effects {
            match effect {
                Effect::Redraw => redraw = true,
                Effect::Shutdown => return Ok(()),
                Effect::Print { view } => {
                    renderer.print(view)?;
                }
            }
        }

        if redraw {
            let area: Rect = terminal::size()?.into();
            renderer.render(app.view(area), area)?;
        }
    }

    Ok(())
}

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
        runner_cfg: runner::RunnerConfig,
        runner_client: lutra_runner::channel::ClientSender,
    ) -> Self {
        let mut s = Self {
            project: ProjectState::new(),
            runner: runner::RunnerSession::new(runner_client, runner_cfg.format),
            keybindings: KeyBindings::new(),
            commands: CommandRegistry::default(),
            repl: ReplPane::new(),
            status: StatusBar::new(project_path, &runner_cfg),
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
        self.repl.draft.input = None;
        self.runner.release();
        if self.repl.cursor.on_input() {
            self.repl.cursor.stage = crate::cell::CellStage::Program;
        }
    }

    fn has_diagnostics(&self) -> bool {
        matches!(
            &self.project.compilation,
            CompileResult::Failed { diagnostics } if !diagnostics.is_empty()
        )
    }

    fn diagnostics_wrapped_lines(&self, cols: u16) -> View<'_> {
        let CompileResult::Failed { diagnostics } = &self.project.compilation else {
            return View::new();
        };

        let mut view = View::new();
        wrap_line(
            &mut view,
            &Line::new(format!(
                "{} compile error{}",
                diagnostics.len(),
                if diagnostics.len() == 1 { "" } else { "s" }
            )),
            cols,
        );
        view.push_line(Line::default());

        for (i, diagnostic) in diagnostics.iter().enumerate() {
            if i > 0 {
                view.push_line(Line::default());
            }
            for text_line in diagnostic.display().lines() {
                wrap_line(&mut view, &Line::new(text_line.to_string()), cols);
            }
        }

        view
    }

    pub(crate) fn view(&self, area: Rect) -> View<'_> {
        let (body_area, _status) = area.split_bottom(2);

        let mut view = if self.has_diagnostics() {
            let diagnostics = self.diagnostics_wrapped_lines(area.cols).lines;
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
        view.push_line(Line::styled("─".repeat(area.cols as usize), Style::muted()));
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

    pub(crate) fn pull(&mut self) -> ActionResult {
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

    fn submit_prompt(&mut self) -> ActionResult {
        let r = self.repl.checkout_from_history();
        self.runner.release();

        let prompt = self.repl.draft.program.trim().to_string();
        if prompt.is_empty() {
            return r;
        }

        // try: command
        if let Some(res) = self.commands.parse_and_find(&prompt) {
            let cmd = match res {
                Err(message) => return r.and(self.repl.commit_error(message)),
                Ok(c) => c,
            };

            return r.and(if cmd.ty().input.is_unit() {
                // run immediately
                cmd.run(self, vec![])
            } else {
                // prompt input
                let mut input = input::InputPane::new(&cmd.ty().input, &cmd.ty().defs, "Ok");
                if let Some(value) = cmd.default_input() {
                    input.set_value(value.clone())
                }
                self.repl.set_program_ty(cmd.ty().clone());
                self.repl.set_input_pane(input);
                self.repl.cursor.stage = crate::cell::CellStage::Input;
                ActionResult::redraw()
            });
        }

        let CompileResult::Success { project } = &self.project.compilation else {
            return r;
        };

        // try: inspect
        if self.repl.try_inspect(project) {
            return r.and(self.repl.commit_cell(false));
        }

        // base case: lutra program
        let res = lutra_compiler::compile(project, &prompt, None, self.runner.format());
        if let Err(err) = res {
            return r.and(self.repl.commit_compile_error(err.to_string()));
        }
        let (program, ty) = res.unwrap();

        if ty.input.is_unit() {
            // execute immediately
            self.repl.draft.input = None;
            self.repl.set_program_ty(ty);
            if let Err(err) = self.runner.run(&program, vec![]) {
                return r.and(self.repl.commit_error(err));
            }
        } else {
            // prompt input
            if let Err(err) = self.runner.prepare(&program) {
                return r.and(self.repl.commit_error(err));
            }
            let input_pane = input::InputPane::new(&ty.input, &ty.defs, "Run");
            self.repl.set_input_pane(input_pane);
            self.repl.cursor.stage = crate::cell::CellStage::Input;
            self.repl.set_program_ty(ty);
        }
        r.and(ActionResult::redraw())
    }

    fn submit_input(&mut self) -> ActionResult {
        // get input
        let res = self.repl.draft.get_input_bin();
        let Some(input) = res else {
            return ActionResult::default();
        };

        // try: command
        let prompt = self.repl.draft.program.trim().to_string();
        if let Some(res) = self.commands.parse_and_find(&prompt) {
            let cmd = match res {
                Err(message) => return self.repl.commit_error(message),
                Ok(c) => c,
            };
            return cmd.run(self, input);
        }

        // base case: lutra program
        match self.runner.execute(input) {
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
            Action::SubmitInput => self.submit_input(),
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
                    self.repl.draft.program = "project".to_string();
                    self.repl.try_inspect(project);
                    return self.repl.commit_cell(false).and(ActionResult::redraw());
                }

                ActionResult::redraw()
            }

            Action::RunnerMessage(response) => match self.runner.handle_response(response) {
                Some(event) => self.handle_runner_event(event),
                None => ActionResult::default(),
            },

            Action::Terminal(event::Event::Resize(_, _)) => ActionResult::redraw(),
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

fn welcome_view() -> View<'static> {
    let mut view = View::new();
    view.push_line(Line::from(vec![
        Span::styled("Lutra", Style::accent().bold()),
        Span::styled(" v", Style::muted()),
        Span::styled(env!("CARGO_PKG_VERSION"), Style::muted()),
    ]));
    view.push_line(Line::styled(
        "Tip:  Enter to run  ·  ↑↓ for history  ·  Esc to clear  ·  Ctrl+Q to exit",
        Style::muted(),
    ));
    view.prefix(Span::styled("▌ ", Style::muted()));
    view.push_line(Line::empty());
    view
}
