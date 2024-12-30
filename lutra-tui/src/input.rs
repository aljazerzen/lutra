mod form;

use std::{collections::VecDeque, usize};

use lutra_bin::ir;
use ratatui::prelude::*;

use crate::terminal::{Action, Component, EventResult};
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

pub fn show_value(ty: &ir::Ty, initial: Option<lutra_bin::Value>) -> Result<(), anyhow::Error> {
    let mut app = InputApp::new(ty);

    if let Some(val) = initial {
        app.form.set_value(val);
    }
    app.update_cursor_path_position(|_| usize::MAX);

    crate::terminal::within_alternate_screen(|term| crate::terminal::run_app(&mut app, term))??;

    Ok(())
}

pub struct InputApp {
    cursor: Cursor,
    form: Form,
    btn_focus: bool,
}

#[derive(Default)]
struct Cursor {
    pub form_path: Vec<usize>,
}

impl InputApp {
    pub fn new(ty: &ir::Ty) -> Self {
        let form = Form::new(ty, FormName::default());

        let mut app = InputApp {
            form,
            cursor: Cursor::default(),
            btn_focus: false,
        };
        app.update_cursor_path_position(|_| 0);
        app
    }
}

impl Component for InputApp {
    fn render(&self, frame: &mut Frame) {
        let area = self.form.render(frame, frame.area());
        if self.btn_focus {
            frame.render_widget("[done]".black().on_white(), area);
        }
    }

    fn update(&mut self, action: Action) -> EventResult {
        let mut res = EventResult::default();

        let mut queue = VecDeque::new();
        queue.push_back(action);

        while let Some(action) = queue.pop_front() {
            // primary handler
            if self.btn_focus {
                if let Action::Select = action {
                    res.shutdown = true;
                }
            } else {
                let focused = self.form.get_mut(&self.cursor.form_path);
                if let Some(focused) = focused {
                    let consumed = focused.update(&action);
                    if consumed {
                        continue;
                    }
                }
            }

            // fallback handler
            match action {
                Action::MoveUp => {
                    self.update_cursor_path_position(|p| p.saturating_sub(1));
                }
                Action::MoveDown => {
                    self.update_cursor_path_position(|p| p.saturating_add(1));
                }
                Action::Select => {
                    self.update_cursor_path_position(|p| p.saturating_add(1));
                }
                _ => {}
            }
        }
        res
    }
}

impl InputApp {
    fn update_cursor_path_position(&mut self, update: impl FnOnce(usize) -> usize) {
        let (position, found) = self.form.take_focus();
        if !found {
            self.btn_focus = false;
        }

        let updated_position = update(position);
        let res = self.form.insert_focus(updated_position);

        let Ok(path) = res else {
            // position does not exist, focus the "done" button
            self.btn_focus = true;
            return;
        };
        self.cursor.form_path = path;
    }

    #[allow(dead_code)]
    fn move_cursor_to_parent(&mut self) {
        let form = self.form.get_mut(&self.cursor.form_path).unwrap();
        form.focus = false;

        self.cursor.form_path.pop();
        let form = self.form.get_mut(&self.cursor.form_path).unwrap();
        form.focus = true;
    }
}
