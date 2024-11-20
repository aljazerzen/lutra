use crossterm::event::KeyCode;
use lutra_bin::ir;
use ratatui::{layout::Offset, prelude::*};

use crate::app::Action;

use super::{text::render_name_colon, Form, FormKind, FormName};

pub struct EnumForm {
    pub selected: usize,
    pub variants: Vec<Form>,
}
impl EnumForm {
    pub fn new(variants: &[ir::TyEnumVariant]) -> EnumForm {
        let variants = variants
            .iter()
            .enumerate()
            .map(|(pos, variant)| {
                let name = FormName {
                    name: Some(variant.name.clone()),
                    position: Some(pos),
                };

                Form::new(&variant.ty, name)
            })
            .collect();

        EnumForm {
            selected: 0,
            variants,
        }
    }

    pub fn get_selected_mut(&mut self) -> Option<&mut Form> {
        self.variants.get_mut(self.selected)
    }

    pub fn render(&self, form: &Form, frame: &mut Frame, area: Rect) -> Rect {
        let value_area = render_name_colon(form, frame, area);

        if let Some(selected) = self.variants.get(self.selected) {
            if !form.focus {
                let value = selected.get_name();
                frame.render_widget(value.white(), value_area);
            } else {
                let mut area = value_area;

                for (pos, variant) in self.variants.iter().enumerate() {
                    let is_selected = pos == self.selected;
                    let name = variant.get_name();
                    if is_selected {
                        frame.render_widget(name.black().on_white(), area);
                    } else {
                        frame.render_widget(name.white(), area);
                    }
                    area.x += name.len() as u16 + 1;
                    area.width -= name.len() as u16 + 1;
                }
            }

            let mut inner_area = area.offset(Offset { x: 2, y: 1 });
            if let FormKind::Tuple(tuple) = &selected.kind {
                for field in &tuple.fields {
                    inner_area = field.render(frame, inner_area);
                }
            } else {
                inner_area = selected.render(frame, inner_area);
            }

            inner_area.x -= 2;
            inner_area.width += 2;
            return inner_area;
        }

        area.offset(Offset { x: 0, y: 1 })
    }

    pub fn update(&mut self, action: &Action) -> Vec<Action> {
        match action {
            Action::KeyEvent(event) if event.code == KeyCode::Left => {
                self.selected = self.selected.saturating_sub(1);
            }
            Action::KeyEvent(event) if event.code == KeyCode::Right => {
                self.selected = self.selected.saturating_add(1);
                if self.selected >= self.variants.len() {
                    self.selected = self.variants.len() - 1;
                }
            }
            _ => {}
        }
        vec![]
    }

    pub(crate) fn get_value(&self) -> lutra_bin::Value {
        let selected_form = self.variants.get(self.selected).unwrap();
        lutra_bin::Value::Enum(self.selected, Box::new(selected_form.get_value()))
    }
}
