use crossterm::event::KeyCode;
use lutra_bin::ir;

use super::{Form, FormName, FormResult, TyDefs};
use crate::terminal::{Action, Span, Style, View};

const ACTIONS: [ArrayAction; 2] = [ArrayAction::Push, ArrayAction::Pop];

#[derive(Clone)]
pub struct ArrayForm {
    focused_action: usize,
    pub items: Vec<Form>,
}

#[derive(PartialEq, Eq)]
enum ArrayAction {
    Push,
    Pop,
}

impl AsRef<str> for ArrayAction {
    fn as_ref(&self) -> &str {
        match self {
            ArrayAction::Push => "push",
            ArrayAction::Pop => "pop",
        }
    }
}

impl ArrayForm {
    pub fn new() -> ArrayForm {
        ArrayForm {
            focused_action: 0,
            items: vec![],
        }
    }

    pub fn render<'a>(&'a self, form: &'a Form, focused: bool) -> View<'a> {
        let mut view = View::from(form.render_name_prefix(focused));

        for (index, action) in ACTIONS.iter().enumerate() {
            if index > 0 {
                view.push_span(Span::new("  "));
            }
            let selected = focused && form.cursor && self.focused_action == index;
            let style = if selected {
                Style::cursor()
            } else {
                Style::new()
            };
            view.push_span(Span::styled(action.as_ref().to_string(), style));

            if selected {
                view.set_cursor_inline();
            }
        }

        for item in &self.items {
            let mut child = item.view(focused);
            child.prefix_first(Span::new("- "));
            child.prefix(Span::new("  "));
            view.extend_lines(child);
        }

        view
    }

    pub fn handle(&mut self, action: &Action, self_ty: &ir::Ty, ty_defs: &TyDefs) -> FormResult {
        let Some(key) = action.as_key() else {
            return FormResult::None;
        };

        match key.code {
            KeyCode::Left => {
                self.focused_action = self.focused_action.saturating_sub(1);
                FormResult::Redraw
            }
            KeyCode::Right => {
                self.focused_action = self.focused_action.saturating_add(1);

                if self.focused_action >= ACTIONS.len() {
                    self.focused_action = ACTIONS.len() - 1;
                }
                FormResult::Redraw
            }
            KeyCode::Enter => {
                if let Some(action) = ACTIONS.get(self.focused_action) {
                    match action {
                        ArrayAction::Push => {
                            self.push(self_ty, ty_defs);
                        }
                        ArrayAction::Pop => {
                            self.items.pop();
                        }
                    }
                }
                FormResult::Redraw
            }
            _ => FormResult::None,
        }
    }

    fn push(&mut self, self_ty: &ir::Ty, ty_defs: &TyDefs) {
        let items_ty = self_ty.kind.as_array().unwrap();
        let name = FormName {
            name: None,
            position: Some(self.items.len()),
        };
        self.items.push(Form::new(items_ty, name, ty_defs.clone()));
    }

    fn set_length(&mut self, len: usize, self_ty: &ir::Ty, ty_defs: &TyDefs) {
        let curr_len = self.items.len();
        if curr_len < len {
            for _ in curr_len..len {
                self.push(self_ty, ty_defs);
            }
        } else {
            self.items.drain(len..);
        }
    }

    pub(crate) fn get_value(&self) -> lutra_bin::Value {
        lutra_bin::Value::Array(self.items.iter().map(|f| f.get_value()).collect())
    }

    pub(crate) fn set_value(
        &mut self,
        value: lutra_bin::Value,
        self_ty: &ir::Ty,
        ty_defs: &TyDefs,
    ) {
        let lutra_bin::Value::Array(values) = value else {
            panic!()
        };
        self.set_length(values.len(), self_ty, ty_defs);
        for (item, value) in std::iter::zip(&mut self.items, values) {
            item.set_value(value);
        }
    }
}
