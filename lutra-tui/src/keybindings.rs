use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use crate::terminal::Action;

/// Centralized keybinding configuration.
///
/// In the future, this can be loaded from a config file to support custom keybindings.
pub struct KeyBindings;

/// Context for key binding resolution.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeyContext {
    Definitions,
    Diagnostics,
    Run,
    RunInput,
}

impl KeyBindings {
    pub fn new() -> Self {
        Self
    }

    /// Process a key event in the given context and return a command action if matched.
    ///
    /// Returns `None` if the key should instead be passed through to the focused component.
    pub fn process_key_event(&self, key: KeyEvent, context: KeyContext) -> Option<Action> {
        // Context-specific bindings
        let action = match context {
            KeyContext::Definitions => None,
            KeyContext::Diagnostics => None,
            KeyContext::Run | KeyContext::RunInput => match key.code {
                // Close tab
                KeyCode::Char('w') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                    Some(Action::CloseTab)
                }
                _ => None,
            },
        };
        if let Some(action) = action {
            return Some(action);
        }

        // Global bindings
        if let Some(action) = self.process_global_bindings(key) {
            return Some(action);
        }

        None
    }

    /// Global keybindings that work in all contexts.
    fn process_global_bindings(&self, key: KeyEvent) -> Option<Action> {
        match key.code {
            // Exit
            KeyCode::Esc => Some(Action::Exit),
            KeyCode::Char('q') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                Some(Action::Exit)
            }

            // Focus cycling
            KeyCode::Tab => Some(Action::CycleFocus),

            // Tab navigation (Ctrl+Left/Right)
            KeyCode::Left if key.modifiers.contains(KeyModifiers::CONTROL) => Some(Action::PrevTab),
            KeyCode::Right if key.modifiers.contains(KeyModifiers::CONTROL) => {
                Some(Action::NextTab)
            }

            // Search (Ctrl+P)
            KeyCode::Char('p') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                Some(Action::DefSearchOpen)
            }

            _ => None,
        }
    }
}
