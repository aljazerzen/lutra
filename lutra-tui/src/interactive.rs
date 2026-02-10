use std::path::PathBuf;
use std::sync::mpsc;

use crossterm::event;
use ratatui::prelude::*;

use crate::keybindings::{KeyBindings, KeyContext};
use crate::layout::{CenterPanel, Layout, PanelFocus};
use crate::panels::{DefinitionsPanel, DiagnosticsPanel, StatusBar, Tabs};
use crate::project::{CompileResult, ProjectState};
use crate::runner;
use crate::terminal::{Action, ActionResult, Component};
use crate::watcher::FileWatcher;

/// Starts the interactive project environment.
///
/// Watches the project directory for changes and provides a TUI
/// for browsing definitions, viewing diagnostics, and running programs.
pub fn run_interactive(
    project_path: PathBuf,
    runner_cfg: runner::RunnerConfig,
) -> anyhow::Result<()> {
    // Create action channel
    let (action_tx, action_rx) = mpsc::channel();

    // Init event sources
    let watcher = FileWatcher::new(project_path.clone(), action_tx.clone())?;
    let runner = runner::Runner::try_new(&runner_cfg, action_tx.clone())?;

    // Create app
    let mut app = InteractiveApp::new(project_path.clone(), runner_cfg, runner.get_client());

    // Enter terminal
    let mut term = ratatui::init();
    let _event_thread = crate::terminal::spawn_event_reader(action_tx);

    // Run the app
    let r = crate::terminal::run_action_loop(&mut app, &mut term, action_rx);

    // Restore terminal
    ratatui::restore();

    // Stop app & event sources
    drop(app);
    drop(watcher);
    runner.join();

    r
}

/// The main interactive application state.
pub struct InteractiveApp {
    // -- state --
    /// Project state (sources, compilation results).
    pub project: ProjectState,

    /// Layout configuration.
    pub layout: Layout,

    /// Runner configuration.
    pub runner: runner::RunnerConfig,

    /// Keybindings configuration.
    keybindings: KeyBindings,

    // -- handles --
    #[allow(dead_code)]
    pub runner_client: lutra_runner::channel::ClientSender,

    // -- components --
    /// Definitions panel.
    pub definitions: DefinitionsPanel,

    /// Diagnostics panel.
    pub diagnostics: DiagnosticsPanel,

    /// Tabbed program panes.
    pub run_panels: Tabs,

    /// Status bar.
    pub status: StatusBar,
}

impl InteractiveApp {
    /// Creates a new interactive app for the given project path.
    pub fn new(
        project_path: PathBuf,
        runner: runner::RunnerConfig,
        runner_client: lutra_runner::channel::ClientSender,
    ) -> Self {
        let status = StatusBar::new(&project_path, &runner);

        let mut app = Self {
            project: ProjectState::new(project_path),
            layout: Layout::new(),
            runner,
            keybindings: KeyBindings::new(),
            runner_client: runner_client.clone(),
            definitions: DefinitionsPanel::new(),
            diagnostics: DiagnosticsPanel::new(),
            run_panels: Tabs::new(runner_client),
            status,
        };

        // Initial compilation
        app.recompile();

        // Set initial focus
        app.update_panel_focus();

        app
    }

    /// Recompile and update all panels.
    fn recompile(&mut self) {
        self.status.set_compiling();
        self.project.recompile();

        // Update panels based on compilation result
        self.status.update(&self.project.compilation);
        self.definitions.update(&self.project);
        self.diagnostics.update(&self.project);

        // Update error indicators in definitions panel
        if let crate::project::CompileResult::Failed { diagnostics } = &self.project.compilation {
            self.definitions.update_from_diagnostics(diagnostics);
        }

        // Clear all program panes on recompile (programs may have changed)
        self.run_panels.clear_all();

        self.layout.update(&self.project);
    }

    /// Update which panel has focus.
    fn update_panel_focus(&mut self) {
        self.definitions.focused = matches!(self.layout.focus, PanelFocus::Definitions);
        self.diagnostics.focused = matches!(self.layout.focus, PanelFocus::Diagnostics);
        self.run_panels
            .set_focused(matches!(self.layout.focus, PanelFocus::Run));
    }

    /// Start running a specific definition.
    fn action_open_program(&mut self, path: &lutra_bin::ir::Path) {
        let CompileResult::Success { project, .. } = &self.project.compilation else {
            return;
        };

        self.run_panels
            .open_program(path.clone(), project, &self.runner);

        // Switch focus to program pane
        self.layout.focus = PanelFocus::Run;
        self.update_panel_focus();
    }

    /// Start running the currently selected definition.
    fn action_open_selected(&mut self) {
        let Some(path) = self.definitions.selected_path() else {
            return;
        };
        self.action_open_program(&path);
    }

    /// Get the current key binding context based on focus state.
    fn get_key_context(&self) -> KeyContext {
        match self.layout.focus {
            PanelFocus::Run => {
                if self.run_panels.is_form_field_focused() {
                    KeyContext::RunInput
                } else {
                    KeyContext::Run
                }
            }
            PanelFocus::Definitions => KeyContext::Definitions,
            PanelFocus::Diagnostics => KeyContext::Diagnostics,
        }
    }

    /// Process a terminal event with focus-aware keybindings.
    fn process_terminal_event(&mut self, event: event::Event) -> ActionResult {
        match event {
            event::Event::Resize(_, _) => ActionResult::redraw(),
            event::Event::Key(key) => {
                let context = self.get_key_context();
                if let Some(command) = self.keybindings.process_key_event(key, context) {
                    // Keybinding matched - recursively handle the command
                    return self.handle(command);
                }
                // No keybinding matched - pass raw terminal event to focused component
                match self.layout.focus {
                    PanelFocus::Definitions => self.definitions.handle(Action::Terminal(event)),
                    PanelFocus::Diagnostics => self.diagnostics.handle(Action::Terminal(event)),
                    PanelFocus::Run => self.run_panels.handle(Action::Terminal(event)),
                }
            }
            _ => {
                // Other events (mouse, focus, paste) - pass to focused component
                match self.layout.focus {
                    PanelFocus::Definitions => self.definitions.handle(Action::Terminal(event)),
                    PanelFocus::Diagnostics => self.diagnostics.handle(Action::Terminal(event)),
                    PanelFocus::Run => self.run_panels.handle(Action::Terminal(event)),
                }
            }
        }
    }
}

impl Component for InteractiveApp {
    fn render(&self, frame: &mut Frame, area: Rect) {
        let areas = self.layout.compute_areas(area);

        // Render definitions panel
        self.definitions.render(frame, areas.definitions);

        // Render center panel (diagnostics or run)
        match self.layout.center {
            CenterPanel::Diagnostics => {
                self.diagnostics.render(frame, areas.center);
            }
            CenterPanel::Run => {
                self.run_panels.render(frame, areas.center);
            }
        }

        // Render status bar
        self.status.render(frame, areas.status);
    }

    fn handle(&mut self, action: Action) -> ActionResult {
        match action {
            // Terminal events - process with focus-aware keybindings
            Action::Terminal(event) => self.process_terminal_event(event),

            // Global commands
            Action::Exit => ActionResult::shutdown(),
            Action::RunDefinition(path) => {
                self.action_open_program(&path);
                ActionResult::redraw()
            }
            Action::ExecuteProgram => {
                // ProgramPane has its own sender, just trigger execution
                if let Some(panel) = self.run_panels.active_panel_mut()
                    && let Err(e) = panel.prepare_and_execute()
                {
                    panel.set_error(e);
                }
                ActionResult::redraw()
            }
            Action::RunSelected => {
                self.action_open_selected();
                ActionResult::redraw()
            }
            Action::CycleFocus => {
                self.layout.cycle_focus();
                self.update_panel_focus();
                ActionResult::redraw()
            }
            Action::Recompile => {
                self.recompile();
                ActionResult::redraw()
            }
            Action::RunnerMessage(msg) => {
                self.run_panels.handle_runner_message(msg);
                ActionResult::redraw()
            }
            Action::CloseTab => {
                self.run_panels.close_active_tab();
                ActionResult::redraw()
            }
            Action::NextTab => {
                self.run_panels.next_tab();
                self.layout.focus = PanelFocus::Run;
                self.update_panel_focus();
                ActionResult::redraw()
            }
            Action::PrevTab => {
                self.run_panels.prev_tab();
                self.layout.focus = PanelFocus::Run;
                self.update_panel_focus();
                ActionResult::redraw()
            }
            Action::SwitchToTab(index) => {
                self.run_panels.switch_to_tab(index);
                self.layout.focus = PanelFocus::Run;
                self.update_panel_focus();
                ActionResult::redraw()
            }
            Action::ReturnToInput => {
                // Delegate to run panels
                self.run_panels.handle(action)
            }
        }
    }
}
