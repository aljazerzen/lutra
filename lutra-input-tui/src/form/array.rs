use crossterm::event::KeyCode;
use lutra_parser::parser::pr;
use ratatui::{layout::Offset, prelude::*};

use crate::app::Action;

use super::{text::render_name_colon, Form, FormName};

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
        let focus_area = render_name_colon(form, frame, area);

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

    pub fn update(&mut self, action: &Action, self_ty: &pr::Ty) -> Vec<Action> {
        match action {
            Action::KeyEvent(event) if event.code == KeyCode::Left => {
                self.focused_action = self.focused_action.saturating_sub(1);
            }
            Action::KeyEvent(event) if event.code == KeyCode::Right => {
                self.focused_action = self.focused_action.saturating_add(1);

                if self.focused_action >= ACTIONS.len() {
                    self.focused_action = ACTIONS.len() - 1;
                }
            }
            Action::KeyEvent(event) if event.code == KeyCode::Enter => {
                if let Some(action) = ACTIONS.get(self.focused_action) {
                    match action {
                        ArrayAction::Push => {
                            let items_ty = self_ty.kind.as_array().unwrap().clone();

                            let name = FormName {
                                name: None,
                                position: Some(self.items.len()),
                            };
                            self.items.push(Form::new(&items_ty, name));
                        }
                        ArrayAction::Pop => {
                            self.items.pop();
                        }
                    }
                }
            }
            _ => {}
        }
        vec![]
    }

    pub(crate) fn get_value(&self) -> lutra_bin::Value {
        lutra_bin::Value::Array(
            self.items.iter().map(|f| f.get_value()).collect(),
        )
    }
}
