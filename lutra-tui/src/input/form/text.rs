use crossterm::event::{Event, KeyCode, KeyModifiers};

use super::{Form, FormResult};
use crate::editor::{self, Edit};
use crate::terminal::{Action, Span, Style, View};

#[derive(Clone)]
pub struct TextForm {
    value: String,
    cursor: usize,
}

impl TextForm {
    pub fn new(value: String) -> TextForm {
        let cursor = value.len();
        TextForm { value, cursor }
    }

    pub fn render<'a>(&'a self, form: &'a Form, focused: bool) -> View<'a> {
        let mut line = form.render_name_prefix(focused);

        if self.value.is_empty() {
            line.push_span(Span::styled("<empty>", Style::muted()));
        } else {
            let (left, right) = self.value.split_at(self.cursor);
            line.push_span(left);
            line.push_span(right);
        };
        let mut view = View::from(line);

        if focused && form.cursor {
            let line = view.lines.last_mut().unwrap();
            let last_span = line.spans.pop().unwrap();
            view.set_cursor_inline();
            let line = view.lines.last_mut().unwrap();
            line.push_span(last_span);
        }

        view
    }

    pub fn handle(&mut self, action: &Action) -> FormResult {
        match action {
            Action::Terminal(Event::Paste(text)) => {
                self.apply_edit(|_, at| Edit::insert(at, text.clone()));
                FormResult::Redraw
            }
            _ => {
                let Some(key) = action.as_key() else {
                    return FormResult::None;
                };

                match key.code {
                    KeyCode::Char(c) if !key.modifiers.contains(KeyModifiers::CONTROL) => {
                        self.apply_edit(|_, at| Edit::insert(at, c.to_string()));
                        FormResult::Redraw
                    }
                    KeyCode::Left if key.modifiers.contains(KeyModifiers::CONTROL) => {
                        self.cursor = editor::move_word_backward(&self.value, self.cursor);
                        FormResult::Redraw
                    }
                    KeyCode::Right if key.modifiers.contains(KeyModifiers::CONTROL) => {
                        self.cursor = editor::move_word_forward(&self.value, self.cursor);
                        FormResult::Redraw
                    }
                    KeyCode::Left => {
                        self.cursor = editor::move_backward(&self.value, self.cursor);
                        FormResult::Redraw
                    }
                    KeyCode::Right => {
                        self.cursor = editor::move_forward(&self.value, self.cursor);
                        FormResult::Redraw
                    }
                    KeyCode::Home => {
                        self.cursor = 0;
                        FormResult::Redraw
                    }
                    KeyCode::End => {
                        self.cursor = self.value.len();
                        FormResult::Redraw
                    }
                    KeyCode::Backspace if key.modifiers.contains(KeyModifiers::CONTROL) => {
                        self.apply_edit(|text, at| {
                            Edit::delete(text, at, editor::move_word_backward)
                        });
                        FormResult::Redraw
                    }
                    KeyCode::Backspace => {
                        self.apply_edit(|text, at| Edit::delete(text, at, editor::move_backward));
                        FormResult::Redraw
                    }
                    KeyCode::Delete if key.modifiers.contains(KeyModifiers::CONTROL) => {
                        self.apply_edit(|text, at| {
                            Edit::delete(text, at, editor::move_word_forward)
                        });
                        FormResult::Redraw
                    }
                    KeyCode::Delete => {
                        self.apply_edit(|text, at| Edit::delete(text, at, editor::move_forward));
                        FormResult::Redraw
                    }
                    _ => FormResult::None,
                }
            }
        }
    }

    pub(crate) fn get_value(&self) -> lutra_bin::Value {
        lutra_bin::Value::Text(self.value.clone())
    }

    pub(crate) fn set_value(&mut self, value: lutra_bin::Value) {
        let lutra_bin::Value::Text(value) = value else {
            panic!()
        };
        self.cursor = value.len();
        self.value = value;
    }

    fn apply_edit(&mut self, edit: impl FnOnce(&str, usize) -> Edit) {
        let edit = edit(&self.value, self.cursor);
        edit.apply(&mut self.value, &mut self.cursor);
    }
}
