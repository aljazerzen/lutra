use crossterm::event::KeyCode;
use ratatui::{layout::Offset, prelude::*};

use crate::app::Action;

use super::Form;

pub struct TextForm {
    value: String,
}

impl TextForm {
    pub fn new(value: String) -> TextForm {
        TextForm { value }
    }

    pub fn render(&self, form: &Form, frame: &mut Frame<'_>, area: Rect) -> Rect {
        let area_value = render_name_colon(form, frame, area);

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

    pub fn update(&mut self, action: &Action) -> Vec<Action> {
        let Action::KeyEvent(event) = action;

        match event.code {
            KeyCode::Char(ch) => {
                self.value.push(ch);
            }
            KeyCode::Backspace => {
                self.value.pop();
            }
            _ => {}
        }
        vec![]
    }

    pub(crate) fn get_value(&self) -> lutra_bin::Value {
        lutra_bin::Value::Text(self.value.clone())
    }
}

pub fn render_name_colon(form: &Form, frame: &mut Frame, area: Rect) -> Rect {
    let name = form.get_name();

    let name_span = if form.focus {
        name.white().bold()
    } else {
        name.white()
    };
    frame.render_widget(name_span, area);

    let area_colon = area.offset(Offset {
        x: name.len() as i32,
        y: 0,
    });
    frame.render_widget(":".white(), area_colon);

    area_colon
        .offset(Offset { x: 2, y: 0 })
        .intersection(frame.area())
}
