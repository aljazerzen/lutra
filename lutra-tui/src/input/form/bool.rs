use ratatui::{layout::Offset, prelude::*};

use super::{Action, Form};

pub struct BoolForm {
    value: bool,
}

impl BoolForm {
    pub fn new(value: bool) -> BoolForm {
        BoolForm { value }
    }

    pub fn render(&self, form: &Form, frame: &mut Frame<'_>, area: Rect) -> Rect {
        let area_value = super::render_name_colon(form, frame, area);

        if form.focus {
            frame.render_widget("[ ]".black().on_white(), area_value);
        } else {
            frame.render_widget("[ ]".white(), area_value);
        };
        if self.value {
            let area_x = area_value.offset(Offset { x: 1, y: 0 });
            if form.focus {
                frame.render_widget("x".black(), area_x);
            } else {
                frame.render_widget("x".white(), area_x);
            }
        }

        area.offset(Offset { x: 0, y: 1 })
    }

    pub fn update(&mut self, action: &Action) -> bool {
        match action {
            Action::Write(text) => {
                for char in text.chars() {
                    match char {
                        'x' => self.value = true,
                        ' ' => self.value = !self.value,
                        _ => {}
                    }
                }
                true
            }
            Action::Erase => {
                self.value = false;
                true
            }
            Action::Select => {
                self.value = !self.value;
                true
            }
            _ => false,
        }
    }

    pub(crate) fn get_value(&self) -> lutra_bin::Value {
        lutra_bin::Value::Bool(self.value)
    }

    pub(crate) fn set_value(&mut self, value: lutra_bin::Value) {
        let lutra_bin::Value::Bool(value) = value else {
            panic!()
        };
        self.value = value;
    }
}
