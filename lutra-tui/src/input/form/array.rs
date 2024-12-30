use lutra_bin::ir;
use ratatui::{layout::Offset, prelude::*};

use super::{Action, Form, FormName};

const ACTIONS: [ArrayAction; 2] = [ArrayAction::Push, ArrayAction::Pop];

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

    pub fn render(&self, form: &Form, frame: &mut Frame, area: Rect) -> Rect {
        let focus_area = super::render_name_colon(form, frame, area);

        if form.focus {
            let mut area = focus_area;
            area.x -= 1;
            for (index, action) in [ArrayAction::Push, ArrayAction::Pop].iter().enumerate() {
                let is_selected = self.focused_action == index;

                let action_str = action.as_ref();
                let widget = if is_selected {
                    action_str.black().on_white()
                } else {
                    action_str.white()
                };
                frame.render_widget(widget, area.offset(Offset { x: 1, y: 0 }));
                area = area.offset(Offset {
                    x: action_str.len() as i32 + 1,
                    y: 0,
                });
            }
        }

        let mut area = area;
        area.y += 1;
        area.height -= 1;

        for item in &self.items {
            frame.render_widget("-".white(), area);

            let area_item = area.offset(Offset { x: 2, y: 0 });
            let area_item = item.render(frame, area_item);
            area.height -= area_item.y - area.y;
            area.y = area_item.y;
        }
        area
    }

    pub fn update(&mut self, action: &Action, self_ty: &ir::Ty) -> bool {
        match action {
            Action::MoveLeft => {
                self.focused_action = self.focused_action.saturating_sub(1);
                true
            }
            Action::MoveRight => {
                self.focused_action = self.focused_action.saturating_add(1);

                if self.focused_action >= ACTIONS.len() {
                    self.focused_action = ACTIONS.len() - 1;
                }
                true
            }
            Action::Select => {
                if let Some(action) = ACTIONS.get(self.focused_action) {
                    match action {
                        ArrayAction::Push => {
                            self.push(self_ty);
                        }
                        ArrayAction::Pop => {
                            self.items.pop();
                        }
                    }
                }
                true
            }
            _ => {
                false
            }
        }
    }

    fn push(&mut self, self_ty: &ir::Ty) {
        let items_ty = self_ty.kind.as_array().unwrap().clone();

        let name = FormName {
            name: None,
            position: Some(self.items.len()),
        };
        self.items.push(Form::new(&items_ty, name));
    }

    fn set_length(&mut self, len: usize, self_ty: &ir::Ty) {
        let curr_len = self.items.len();
        if curr_len < len {
            for _ in curr_len..len {
                self.push(self_ty);
            }
        } else {
            self.items.drain(len..);
        }
    }

    pub(crate) fn get_value(&self) -> lutra_bin::Value {
        lutra_bin::Value::Array(self.items.iter().map(|f| f.get_value()).collect())
    }

    pub(crate) fn set_value(&mut self, value: lutra_bin::Value, self_ty: &ir::Ty) {
        let lutra_bin::Value::Array(values) = value else {
            panic!()
        };
        self.set_length(values.len(), self_ty);
        for (item, value) in std::iter::zip(&mut self.items, values) {
            item.set_value(value);
        }
    }
}
