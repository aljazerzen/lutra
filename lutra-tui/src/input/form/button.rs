use ratatui::prelude::*;

use crate::input::form::{Form, FormResult, clip_top};
use crate::terminal::Action;

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

    pub fn update(&self, action: &Action) -> FormResult {
        if let Action::Select = action {
            FormResult::Submit
        } else {
            FormResult::None
        }
    }
}
