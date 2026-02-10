use crossterm::event::KeyCode;
use ratatui::prelude::*;

use crate::input::form::{Form, FormResult};
use crate::terminal::Action;
use crate::utils::clip_top;

/// A form and a button after.
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

    pub fn render(&self, form: &Form, frame: &mut Frame, area: Rect) -> Rect {
        let area = self.inner.render(frame, area);

        // space
        let area = clip_top(area, 1);

        // button
        let style = if form.cursor {
            Style::default().bg(Color::White).fg(Color::Black)
        } else {
            Style::default()
        };
        let btn = Span::styled(format!("[{}]", self.text), style);
        frame.render_widget(btn, area);
        clip_top(area, 1)
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
