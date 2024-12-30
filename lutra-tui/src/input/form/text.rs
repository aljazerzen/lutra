use ratatui::{layout::Offset, prelude::*};

use super::{Action, Form};

pub struct TextForm {
    value: String,
}

impl TextForm {
    pub fn new(value: String) -> TextForm {
        TextForm { value }
    }

    pub fn render(&self, form: &Form, frame: &mut Frame<'_>, area: Rect) -> Rect {
        let area_value = super::render_name_colon(form, frame, area);

        let value_text = if self.value.is_empty() {
            "<empty>"
        } else {
            self.value.as_str()
        };
        if self.value.is_empty() {
            frame.render_widget(value_text.dark_gray().italic(), area_value);
        } else {
            frame.render_widget(value_text.white(), area_value);
        };

        if form.focus {
            let cursor_pos = self.value.len();
            let cursor_char = value_text.chars().nth(cursor_pos).unwrap_or(' ');

            let mut area_cursor = area_value.offset(Offset {
                x: cursor_pos as i32,
                y: 0,
            });
            area_cursor.width = 1;
            frame.render_widget(cursor_char.to_string().black().on_white(), area_cursor);
        }

        area.offset(Offset { x: 0, y: 1 })
    }

    pub fn update(&mut self, action: &Action) -> bool {
        match action {
            Action::Write(text) => {
                self.value.extend(text.chars());
                true
            }
            Action::Erase => {
                self.value.pop();
                true
            }
            _ => false,
        }
    }

    pub(crate) fn get_value(&self) -> lutra_bin::Value {
        lutra_bin::Value::Text(self.value.clone())
    }

    pub(crate) fn set_value(&mut self, value: lutra_bin::Value) {
        let lutra_bin::Value::Text(value) = value else {
            panic!()
        };
        self.value = value;
    }
}
