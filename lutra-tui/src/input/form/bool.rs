use crossterm::event::KeyCode;
use ratatui::prelude::*;

use crate::utils::{clip_left, clip_top};

use super::{Action, Form, FormResult};

pub struct BoolForm {
    value: bool,
}

impl BoolForm {
    pub fn new(value: bool) -> BoolForm {
        BoolForm { value }
    }

    pub fn render(&self, form: &Form, frame: &mut Frame<'_>, area: Rect) -> Rect {
        let area_value = super::render_name_colon(form, frame, area);

        if form.cursor {
            frame.render_widget("[ ]".black().on_white(), area_value);
        } else {
            frame.render_widget("[ ]".white(), area_value);
        };
        if self.value {
            let area_x = clip_left(area_value, 1);
            if form.cursor {
                frame.render_widget("x".black(), area_x);
            } else {
                frame.render_widget("x".white(), area_x);
            }
        }

        clip_top(area, 1)
    }

    pub fn update(&mut self, action: &Action) -> FormResult {
        let Some(key) = action.as_key() else {
            return FormResult::None;
        };

        match key.code {
            KeyCode::Char('x') => {
                self.value = true;
                FormResult::Redraw
            }
            KeyCode::Char(' ') => {
                self.value = !self.value;
                FormResult::Redraw
            }
            KeyCode::Backspace => {
                self.value = false;
                FormResult::Redraw
            }
            KeyCode::Enter => {
                self.value = !self.value;
                FormResult::Redraw
            }
            _ => FormResult::None,
        }
    }

    pub(crate) fn get_value(&self) -> lutra_bin::Value {
        lutra_bin::Value::Prim8(self.value as u8)
    }

    pub(crate) fn set_value(&mut self, value: lutra_bin::Value) {
        let lutra_bin::Value::Prim8(value) = value else {
            panic!()
        };
        self.value = value != 0;
    }
}
