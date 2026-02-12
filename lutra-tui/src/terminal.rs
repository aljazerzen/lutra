use std::collections::VecDeque;

use crossterm::event;
use lutra_bin::ir;
use ratatui::prelude::*;

/// Top-level component of a TUI.
///
/// Other components should always have similar functions for rendering and handling actions,
/// but with additional parameters (additional state from the parent).
pub trait Component {
    fn handle(&mut self, action: Action) -> ActionResult;

    fn render(&self, frame: &mut Frame, area: Rect);
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum Action {
    /// Terminal event (raw, uninterpreted)
    Terminal(event::Event),

    /// Runner responses
    RunnerMessage(lutra_runner::channel::messages::ServerMessage),

    // App-level commands (high-level)
    Exit,
    Recompile,
    CycleFocus,
    ExecuteProgram,
    RunSelected,
    RunDefinition(ir::Path),
    ReturnToInput,
    CloseTab,
    NextTab,
    PrevTab,
    SwitchToTab(usize),
    DefSearchOpen,
    DefSearchClose,
    ToggleAutoRun,
}

impl Action {
    /// Extract a key event from a Terminal action.
    pub fn as_key(&self) -> Option<event::KeyEvent> {
        match self {
            Action::Terminal(event::Event::Key(key)) => Some(*key),
            _ => None,
        }
    }
}

#[derive(Default)]
pub struct ActionResult {
    redraw: bool,
    shutdown: bool,
    actions: Vec<Action>,
}

impl ActionResult {
    pub fn redraw() -> Self {
        Self {
            redraw: true,
            ..Default::default()
        }
    }
    pub fn shutdown() -> Self {
        Self {
            shutdown: true,
            ..Default::default()
        }
    }
    pub fn action(action: Action) -> Self {
        Self {
            actions: vec![action],
            ..Default::default()
        }
    }
}

/// Spawns a background thread that reads terminal events and sends them as actions.
///
/// The thread will exit when the channel is closed (receiver dropped) or when
/// an error occurs reading events (e.g., terminal closed).
pub(super) fn spawn_event_reader(
    action_tx: std::sync::mpsc::Sender<Action>,
) -> std::thread::JoinHandle<()> {
    std::thread::Builder::new()
        .name("event-reader".to_string())
        .spawn(move || {
            loop {
                // This blocks until an event arrives
                match event::read() {
                    Ok(event) => {
                        // Send terminal event as an action
                        if action_tx.send(Action::Terminal(event)).is_err() {
                            // Channel closed, exit thread
                            break;
                        }
                    }
                    Err(e) => {
                        // Error reading event (terminal closed/corrupted?)
                        eprintln!("Event reader error: {e}");
                        break;
                    }
                }
            }
        })
        .expect("Failed to spawn event reader thread")
}

/// Run a Component-based app with action-based event processing.
///
/// All events (terminal, file watcher, etc.) flow through the action channel.
/// The caller should spawn an event reader thread and any other action sources
/// before calling this function.
pub(super) fn run_action_loop<R: ratatui::backend::Backend>(
    app: &mut impl Component,
    terminal: &mut Terminal<R>,
    action_rx: std::sync::mpsc::Receiver<Action>,
) -> Result<(), anyhow::Error>
where
    <R as ratatui::backend::Backend>::Error: Send + Sync + 'static,
{
    // Initial draw
    terminal.draw(|frame| app.render(frame, frame.area()))?;

    // Wait for input actions (terminal, file watcher)
    while let Ok(action) = action_rx.recv() {
        let mut redraw = false;
        let mut queue = VecDeque::new();
        queue.push_back(action);

        // Process actions
        while let Some(action) = queue.pop_front() {
            let res = app.handle(action);
            if res.shutdown {
                return Ok(());
            }
            redraw = redraw || res.redraw;
            // Each action can trigger other actions
            queue.extend(res.actions);
        }

        // Redraw if needed
        if redraw {
            terminal.draw(|frame| app.render(frame, frame.area()))?;
        }
    }

    Ok(())
}
