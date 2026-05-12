use std::collections::VecDeque;
use std::io;
use std::path;

use crossterm::event::Event;
use crossterm::terminal;

use crate::printer::{Printer, TerminalPrinter};
use crate::runner;
use crate::shell::Shell;
use crate::terminal::Rect;
use crate::terminal::{Action, Effect, Line, Span, Style, View};
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
    // create action channel
    let (action_tx, action_rx) = std::sync::mpsc::channel();

    // init event sources
    let watcher = FileWatcher::new(project_path.clone(), action_tx.clone())?;
    let runner = runner::RunnerProxy::try_new(runner, runner_thread, action_tx.clone())?;

    // create app
    let app = Shell::new(project_path, runner_cfg, runner.get_client());

    // enter terminal
    let printer = TerminalPrinter::new()?;
    let _event_thread = crate::terminal::spawn_event_reader(action_tx);
    let viewport: Rect = terminal::size()?.into();

    let mut driver = Driver {
        app,
        printer,
        viewport,
        queue: Default::default(),
    };

    // start
    if let Err(e) = driver.start() {
        let _ = driver.printer.restore();
        return Err(e.into());
    }

    // run until completion
    let mut result = Ok(false);
    while let Ok(action) = action_rx.recv() {
        result = driver.step(action);
        if result.is_err() || result.as_ref().is_ok_and(|x| *x) {
            break;
        }
    }

    // restore terminal state
    let restore_res = driver.printer.restore();

    // stop app & event sources
    drop(driver.app);
    drop(watcher);
    runner.join();

    result?;
    restore_res?;
    Ok(())
}

pub(crate) struct Driver<P: Printer> {
    pub app: Shell,
    pub printer: P,
    pub viewport: Rect,
    pub queue: VecDeque<Action>,
}

impl<P: Printer> Driver<P> {
    pub(crate) fn start(&mut self) -> io::Result<()> {
        self.printer.commit_view(welcome_view())?;
        let area = self.viewport;
        self.printer.update_view(self.app.render(area), area)?;
        Ok(())
    }

    /// Process a single action through the shell state machine.
    ///
    /// Drains the internal action queue to quiescence, applies effects, and
    /// flushes any pending committed cells to `printer` before the live render.
    ///
    /// Returns `Ok(true)` if the app requested shutdown.
    pub(crate) fn step(&mut self, action: Action) -> io::Result<bool> {
        let mut redraw = false;
        let mut effects = Vec::new();

        if let Action::Terminal(Event::Resize(cols, rows)) = action {
            redraw = true;
            self.viewport = Rect { rows, cols };
        } else {
            self.queue.push_back(action);
        }

        while let Some(action) = self.queue.pop_front() {
            let res = self.app.handle(action);
            let (actions, new_effects) = res.into_parts();
            effects.extend(new_effects);
            self.queue.extend(actions);
        }

        for effect in effects {
            match effect {
                Effect::Redraw => redraw = true,
                Effect::Shutdown => return Ok(true),
            }
        }

        if redraw {
            let area = self.viewport;

            // commit views
            let prints = std::mem::take(&mut self.app.repl.pending_prints);
            let print_area = Rect::vertical(area.cols);
            for cell in prints {
                let mut view = cell.render(print_area, None, View::new()).to_owned();
                view.push_line(Line::empty());
                self.printer.commit_view(view)?;
            }

            // update the work view
            self.printer.update_view(self.app.render(area), area)?;
        }

        Ok(false)
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
