use std::collections::VecDeque;

use crossterm::event::{self, KeyCode, KeyModifiers};
use lutra_bin::ir;
use ratatui::prelude::*;

/// Top-level component of a TUI.
///
/// Other components should always have similar functions for rendering and handling actions,
/// but with additional parameters (additional state from the parent).
pub trait Component {
    fn handle(&mut self, action: Action) -> EventResult;

    fn render(&self, frame: &mut Frame, area: Rect);
}

#[derive(Debug)]
pub enum Action {
    // Terminal events (from event reader thread)
    Terminal(event::Event),

    // Generic UI actions
    Write(String),
    Erase,
    MoveUp,
    MoveDown,
    MoveRight,
    MoveLeft,
    MovePageUp,
    MovePageDown,
    Select,

    // InteractiveApp-specific actions
    RunDefinition(ir::Path),
    ExecuteProgram,
    RunSelected,
    CycleFocus,
    Exit,
    Recompile,
    ReturnToInput,
    RunnerMessage(lutra_runner::channel::messages::ServerMessage),

    // Tab management
    CloseTab,
    NextTab,
    PrevTab,
    SwitchToTab(usize),
}

#[derive(Default)]
pub struct EventResult {
    pub redraw: bool,
    pub shutdown: bool,
    pub actions: Vec<Action>,
}

impl EventResult {
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
pub(super) fn run_app<R: ratatui::backend::Backend>(
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
            let res = process_action(app, action);
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

/// Process an action by either handling it as a terminal event or passing to app.update().
fn process_action(app: &mut impl Component, action: Action) -> EventResult {
    match action {
        Action::Terminal(event) => process_terminal_event(event),
        _ => {
            // All other actions go directly to app.update()
            app.handle(action)
        }
    }
}

fn process_terminal_event(event: event::Event) -> EventResult {
    match event {
        event::Event::Resize(_, _) => EventResult::redraw(),
        event::Event::Key(key_event) => {
            // Generic quit handling (for simple apps)
            if matches!(key_event.code, KeyCode::Esc)
                || (key_event.code == KeyCode::Char('c')
                    && key_event.modifiers.contains(KeyModifiers::CONTROL))
            {
                return EventResult::action(Action::Exit);
            }

            // Fall back to generic mapping
            EventResult::action(match key_event.code {
                KeyCode::Left if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
                    Action::PrevTab
                }
                KeyCode::Right if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
                    Action::NextTab
                }
                KeyCode::Left => Action::MoveLeft,
                KeyCode::Right => Action::MoveRight,
                KeyCode::Down => Action::MoveDown,
                KeyCode::Up => Action::MoveUp,
                KeyCode::PageUp => Action::MovePageUp,
                KeyCode::PageDown => Action::MovePageDown,
                KeyCode::Enter => Action::Select,
                KeyCode::Tab => Action::CycleFocus,
                KeyCode::Backspace => Action::Erase,
                KeyCode::Char('x') => Action::CloseTab,
                KeyCode::Char(c @ '1'..='9') => {
                    // Convert '1' -> 0, '2' -> 1, etc.
                    let index = (c as usize) - ('1' as usize);
                    Action::SwitchToTab(index)
                }
                KeyCode::Char(c) => Action::Write(c.to_string()),
                KeyCode::F(5) => Action::RunSelected,
                _ => return EventResult::default(),
            })
        }
        event::Event::FocusGained => EventResult::default(),
        event::Event::FocusLost => EventResult::default(),
        event::Event::Mouse(_) => EventResult::default(),
        event::Event::Paste(text) => EventResult::action(Action::Write(text)),
    }
}
