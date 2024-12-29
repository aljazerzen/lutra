mod form;

use std::collections::VecDeque;

use crossterm::event::{self, KeyCode, KeyEventKind};
use lutra_bin::ir;
use ratatui::prelude::*;

use crate::terminal::{App, EventResult};
use form::{Form, FormName};

/// Starts a TUI prompt for type `ty` on stdout terminal.
pub fn prompt_for_ty(
    ty: &ir::Ty,
    initial: Option<lutra_bin::Value>,
) -> Result<lutra_bin::Value, anyhow::Error> {
    let mut app = InputApp::new(ty);

    if let Some(val) = initial {
        app.form.set_value(val);
    }

    crate::terminal::within_alternate_screen(|term| crate::terminal::run_app(&mut app, term))??;

    Ok(app.form.get_value())
}

pub struct InputApp {
    cursor: Cursor,
    form: Form,
}

#[derive(Default)]
struct Cursor {
    pub form_path: Vec<usize>,
}

#[derive(Debug)]
enum Action {
    Write(String),
    Erase,
    MoveUp,
    MoveDown,
    MoveRight,
    MoveLeft,
    Select,
}

impl InputApp {
    pub fn new(ty: &ir::Ty) -> Self {
        let form = Form::new(ty, FormName::default());

        let mut app = InputApp {
            form,
            cursor: Cursor::default(),
        };
        app.update_cursor_path_position(|p| p);
        app
    }
}

impl App for InputApp {
    fn render(&self, frame: &mut Frame) {
        self.form.render(frame, frame.area());
    }

    fn handle_event(&mut self, event: event::Event) -> EventResult {
        let mut res = EventResult::default();
        match event {
            event::Event::Key(key)
                if key.kind == KeyEventKind::Press && key.code == KeyCode::Char('q') =>
            {
                res.shutdown = true;
            }
            event::Event::Resize(_, _) => {
                res.redraw = true;
            }
            event::Event::Key(event) => {
                let action = match event.code {
                    KeyCode::Left => Action::MoveLeft,
                    KeyCode::Right => Action::MoveRight,
                    KeyCode::Down => Action::MoveDown,
                    KeyCode::Up => Action::MoveUp,
                    KeyCode::Enter => Action::Select,
                    KeyCode::Backspace => Action::Erase,
                    KeyCode::Char(char) => Action::Write(char.to_string()),
                    _ => return res,
                };
                self.update(action);
                res.redraw = true;
            }
            event::Event::FocusGained => {}
            event::Event::FocusLost => {}
            event::Event::Mouse(_) => {}
            event::Event::Paste(text) => {
                self.update(Action::Write(text));
                res.redraw = true;
            }
        }
        res
    }
}

impl InputApp {
    fn update(&mut self, action: Action) {
        let mut queue = VecDeque::new();
        queue.push_back(action);

        while let Some(action) = queue.pop_front() {
            // primary handler
            match action {
                Action::MoveUp => {
                    self.update_cursor_path_position(|p| p.saturating_sub(1));
                }
                Action::MoveDown => {
                    self.update_cursor_path_position(|p| p.saturating_add(1));
                }
                _ => {
                    let focused = self.form.get_mut(&self.cursor.form_path);
                    if let Some(focused) = focused {
                        let consumed = focused.update(&action);
                        if consumed {
                            continue;
                        }
                    }
                }
            }

            // fallback handler
            match action {
                Action::MoveLeft => {
                    self.move_cursor_to_parent();
                }
                _ => {}
            }
        }
    }

    fn update_cursor_path_position(&mut self, update: impl FnOnce(usize) -> usize) {
        let (mut position, found) = self.form.take_focus();
        if !found {
            position = 0;
        }

        let updated_position = update(position);
        let res = self.form.insert_focus(updated_position);

        let Ok(path) = res else {
            // revert
            self.form.insert_focus(position).ok();
            return;
        };
        self.cursor.form_path = path;
    }

    fn move_cursor_to_parent(&mut self) {
        self.form.take_focus();
        self.cursor.form_path.pop();
        let form = self.form.get_mut(&self.cursor.form_path).unwrap();
        form.focus = true;
    }
}
