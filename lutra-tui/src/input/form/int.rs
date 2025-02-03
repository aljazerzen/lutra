use ratatui::prelude::*;

use super::text::TextForm;
use super::{Action, Form};

pub struct IntForm {
    inner: TextForm,
}

impl IntForm {
    pub fn new(value: i64) -> IntForm {
        IntForm {
            inner: TextForm::new(value.to_string()),
        }
    }

    pub fn render(&self, form: &Form, frame: &mut Frame<'_>, area: Rect) -> Rect {
        self.inner.render(form, frame, area)
    }

    pub fn update(&mut self, action: &Action) -> bool {
        match action {
            Action::Write(text) => {
                let text = text.chars().filter(|c| c.is_ascii_digit()).collect();
                self.inner.update(&Action::Write(text))
            }
            _ => self.inner.update(action),
        }
    }

    pub(crate) fn get_value(&self) -> lutra_bin::Value {
        let lutra_bin::Value::Text(value) = self.inner.get_value() else {
            panic!()
        };

        let value = value.parse::<i64>().unwrap_or_default();
        lutra_bin::Value::Int64(value)
    }

    pub(crate) fn set_value(&mut self, value: lutra_bin::Value) {
        let lutra_bin::Value::Int64(value) = value else {
            panic!()
        };
        let value = lutra_bin::Value::Text(value.to_string());

        self.inner.set_value(value);
    }
}
