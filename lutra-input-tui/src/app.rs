use std::collections::VecDeque;

use crossterm::event::{self, KeyCode, KeyEvent, KeyEventKind};
use lutra_bin::ir;
use ratatui::prelude::*;

use crate::form::{Form, FormName};

pub struct App {
    cursor: Cursor,
    form: Form,
}

impl App {
    pub fn new(ty: &ir::Ty) -> Self {
        let form = Form::new(ty, FormName::default());

        let mut app = App {
            form,
            cursor: Cursor::default(),
        };
        app.update_cursor_path_position(|p| p);
        app
    }

    pub fn get_value(&self) -> lutra_bin::Value {
        self.form.get_value()
    }

    pub fn render(&self, frame: &mut Frame) {
        self.form.render(frame, frame.area());
    }

    pub fn handle_event(&mut self, event: event::Event) -> EventResult {
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
                self.update(Action::KeyEvent(event));
                res.redraw = true;
            }
            event::Event::FocusGained => {}
            event::Event::FocusLost => {}
            event::Event::Mouse(_) => {}
            event::Event::Paste(_) => {}
        }
        res
    }

    fn update(&mut self, action: Action) {
        let mut queue = VecDeque::new();
        queue.push_back(action);

        while let Some(action) = queue.pop_front() {
            match action {
                Action::KeyEvent(event) if event.code == KeyCode::Up => {
                    self.update_cursor_path_position(|p| p.saturating_sub(1));
                }
                Action::KeyEvent(event) if event.code == KeyCode::Down => {
                    self.update_cursor_path_position(|p| p.saturating_add(1));
                }
                Action::KeyEvent(_) => {
                    let focused = self.form.get_mut(&self.cursor.form_path);
                    if let Some(focused) = focused {
                        queue.extend(focused.update(&action));
                    }
                }
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
}

#[derive(Default)]
pub struct EventResult {
    pub redraw: bool,
    pub shutdown: bool,
}

pub enum Action {
    KeyEvent(KeyEvent),
}

#[derive(Default)]
pub struct Cursor {
    pub form_path: Vec<usize>,
}
