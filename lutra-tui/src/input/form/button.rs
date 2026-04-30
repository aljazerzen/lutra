use crossterm::event::KeyCode;

use crate::input::form::{Form, FormResult};
use crate::terminal::{Action, Line, Span, Style, View};

/// A form and a button after.
#[derive(Clone)]
pub struct ButtonForm {
    pub inner: Box<Form>,

    text: &'static str,
}

impl ButtonForm {
    pub fn new(inner: Form, text: &'static str) -> Self {
        ButtonForm {
            inner: Box::new(inner),
            text,
        }
    }

    pub fn render<'a>(&'a self, form: &'a Form, focused: bool) -> View<'a> {
        let mut view = self.inner.view(focused);

        let style = if focused && form.cursor {
            Style::cursor()
        } else {
            Style::new()
        };
        let button_line = Line::from(vec![Span::styled(format!("[{}]", self.text), style)]);
        view.push_line(button_line);
        if focused && form.cursor {
            view.set_cursor_inline();
        }
        view
    }

    pub fn handle(&self, action: &Action) -> FormResult {
        let Some(key) = action.as_key() else {
            return FormResult::None;
        };

        match key.code {
            KeyCode::Enter => FormResult::Submit,
            _ => FormResult::None,
        }
    }
}
