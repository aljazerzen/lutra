use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use crate::cell::CellStage;
use crate::terminal::Action;

/// Centralized keybinding configuration.
///
/// In the future, this can be loaded from a config file to support custom keybindings.
pub struct KeyBindings;

/// Context for key binding resolution.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeyContext {
    /// Compilation errors are displayed; navigation scrolls diagnostics.
    Diagnostics,
    /// Completion suggestions are visible; Up/Down/Enter navigate them.
    Completions,
    /// Normal cell editing, scoped to the active cell stage.
    Cell(CellStage),
}

impl KeyBindings {
    pub fn new() -> Self {
        Self
    }

    /// Translate a key event to a semantic action based on the current context.
    ///
    /// Returns `None` for keys that should be forwarded as raw terminal events
    /// (i.e. printable character input and backspace handled directly by the frame).
    ///
    /// Note: Ctrl+D is intentionally excluded — it only exits when the program
    /// buffer is empty, a state check that belongs in the dispatcher.
    pub fn process_key(&self, key: KeyEvent, ctx: KeyContext) -> Option<Action> {
        // Global bindings — apply in every context.
        match key.code {
            KeyCode::Char('q') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                return Some(Action::Exit);
            }
            KeyCode::Char('d') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                return Some(Action::ExitIfEmpty);
            }
            KeyCode::Char('c') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                return Some(Action::ClearCell);
            }
            _ => {}
        }

        // Context-specific bindings.
        match ctx {
            KeyContext::Diagnostics => match key.code {
                KeyCode::PageUp => Some(Action::ScrollDiagnosticsUp),
                KeyCode::PageDown => Some(Action::ScrollDiagnosticsDown),
                _ => None,
            },

            KeyContext::Completions => match key.code {
                KeyCode::Up => Some(Action::CompletionUp),
                KeyCode::Down => Some(Action::CompletionDown),
                KeyCode::Enter | KeyCode::Tab | KeyCode::Right => Some(Action::CompletionApply),
                KeyCode::Esc => Some(Action::CompletionHide),
                // All other keys fall through to Program-focus behavior.
                _ => self.process_key_at_cell(key, CellStage::Program),
            },

            KeyContext::Cell(stage) => self.process_key_at_cell(key, stage),
        }
    }

    fn process_key_at_cell(&self, key: KeyEvent, focus: CellStage) -> Option<Action> {
        match key.code {
            KeyCode::Esc => return Some(Action::ClearCell),
            KeyCode::Tab => return Some(Action::CycleFocusNext),
            KeyCode::BackTab => return Some(Action::CycleFocusPrev),
            _ => {}
        }

        match focus {
            CellStage::Browse => match key.code {
                KeyCode::Up => Some(Action::HistoryUp),
                KeyCode::Down => Some(Action::HistoryDown),
                KeyCode::Enter => Some(Action::HistorySelect),
                _ => self.process_edit_key(key),
            },
            CellStage::Program => match key.code {
                KeyCode::Up => Some(Action::HistoryUp),
                KeyCode::Enter => Some(Action::SubmitPrompt),
                _ => self.process_edit_key(key),
            },
            CellStage::Input => None,
            CellStage::Output => match key.code {
                KeyCode::Up => Some(Action::TableMoveUp),
                KeyCode::Down => Some(Action::TableMoveDown),
                KeyCode::Left => Some(Action::TableMoveLeft),
                KeyCode::Right => Some(Action::TableMoveRight),
                KeyCode::Home => Some(Action::TableMoveHome),
                KeyCode::End => Some(Action::TableMoveEnd),
                KeyCode::PageUp => Some(Action::TableMovePageUp),
                KeyCode::PageDown => Some(Action::TableMovePageDown),
                KeyCode::Enter => Some(Action::ClearCell),
                _ => None,
            },
        }
    }

    fn process_edit_key(&self, key: KeyEvent) -> Option<Action> {
        match key.code {
            KeyCode::Left if key.modifiers.contains(KeyModifiers::CONTROL) => {
                Some(Action::EditMoveWordBackward)
            }
            KeyCode::Right if key.modifiers.contains(KeyModifiers::CONTROL) => {
                Some(Action::EditMoveWordForward)
            }
            KeyCode::Left => Some(Action::EditMoveBackward),
            KeyCode::Right => Some(Action::EditMoveForward),
            KeyCode::Home => Some(Action::EditMoveHome),
            KeyCode::End => Some(Action::EditMoveEnd),
            KeyCode::Backspace if key.modifiers.contains(KeyModifiers::CONTROL) => {
                Some(Action::EditDeleteWordBackward)
            }
            KeyCode::Backspace if key.modifiers.contains(KeyModifiers::ALT) => {
                Some(Action::EditDeleteWordBackward)
            }
            KeyCode::Char('h') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                Some(Action::EditDeleteWordBackward)
            }
            KeyCode::Char('w') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                Some(Action::EditDeleteWordBackward)
            }
            KeyCode::Backspace => Some(Action::EditDeleteBackward),

            KeyCode::Delete if key.modifiers.contains(KeyModifiers::CONTROL) => {
                Some(Action::EditDeleteWordForward)
            }
            KeyCode::Delete => Some(Action::EditDeleteForward),
            _ => None,
        }
    }
}
