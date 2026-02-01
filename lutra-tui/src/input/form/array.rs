use lutra_bin::ir;
use ratatui::prelude::*;

use crate::input::form::{clip_left, clip_top};

use super::{Action, Form, FormName, FormResult, TyDefs};

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

        if form.cursor {
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
                frame.render_widget(widget, clip_left(area, 1));
                area = clip_left(area, action_str.len() as u16 + 1);
            }
        }

        let mut area = clip_top(area, 1);

        for item in &self.items {
            frame.render_widget("-".white(), area);

            let area_item = clip_left(area, 2);
            let after_item = item.render(frame, area_item);
            area = clip_top(area, after_item.y - area.y)
        }
        area
    }

    pub fn update(&mut self, action: &Action, self_ty: &ir::Ty, ty_defs: &TyDefs) -> FormResult {
        match action {
            Action::MoveLeft => {
                self.focused_action = self.focused_action.saturating_sub(1);
                FormResult::Redraw
            }
            Action::MoveRight => {
                self.focused_action = self.focused_action.saturating_add(1);

                if self.focused_action >= ACTIONS.len() {
                    self.focused_action = ACTIONS.len() - 1;
                }
                FormResult::Redraw
            }
            Action::Select => {
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
