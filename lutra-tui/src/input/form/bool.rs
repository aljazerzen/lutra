use crossterm::event::KeyCode;

use crate::terminal::{Action, Span, Style, View};

use super::{Form, FormResult};

#[derive(Clone)]
pub struct BoolForm {
    value: bool,
}

impl BoolForm {
    pub fn new(value: bool) -> BoolForm {
        BoolForm { value }
    }

    pub fn render<'a>(&self, form: &'a Form, focused: bool) -> View<'a> {
        let mut line = form.render_name_prefix(focused);
        let box_style = if focused && form.cursor {
            Style::cursor()
        } else {
            Style::new()
        };
        let value = if self.value { "[x]" } else { "[ ]" };
        line.push_span(Span::styled(value, box_style));

        let mut view = View::from(line);
        if focused && form.cursor {
            view.set_cursor_inline();
        }
        view
    }

    pub fn handle(&mut self, action: &Action) -> FormResult {
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
