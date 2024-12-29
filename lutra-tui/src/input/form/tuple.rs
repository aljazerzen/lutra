use lutra_bin::ir;
use ratatui::{
    layout::{Offset, Rect},
    style::Stylize,
    Frame,
};

use crate::input::Action;

use super::{Form, FormName};

pub struct TupleForm {
    pub fields: Vec<Form>,
    pub is_folded: bool,
}

impl TupleForm {
    pub fn new(fields: &[ir::TyTupleField]) -> Self {
        let fields: Vec<_> = fields
            .iter()
            .enumerate()
            .map(|(position, field)| {
                let ty = &field.ty;
                let name = FormName {
                    name: field.name.clone(),
                    position: Some(position),
                };
                Form::new(ty, name)
            })
            .collect();
        TupleForm {
            is_folded: fields.len() > 10,
            fields,
        }
    }

    pub fn render(&self, form: &Form, frame: &mut Frame, area: Rect) -> Rect {
        let name = form.get_name();

        frame.render_widget(name.white(), area);
        let after_name_area = area.offset(Offset {
            x: name.len() as i32 + 1,
            y: 0,
        });

        if self.is_folded {
            if form.focus {
                frame.render_widget("...".black().on_white(), after_name_area);
            } else {
                frame.render_widget("...".white(), after_name_area);
            }

            area.offset(Offset { x: 0, y: 1 })
        } else {
            if form.focus {
                frame.render_widget(" ".black().on_white(), after_name_area);
            }

            let mut next_field_area = area.offset(Offset { x: 2, y: 1 });
            for field in &self.fields {
                next_field_area = field.render(frame, next_field_area);
            }

            // deindent
            next_field_area.x -= 2;
            next_field_area.width += 2;

            next_field_area
        }
    }

    pub(crate) fn get_value(&self) -> lutra_bin::Value {
        lutra_bin::Value::Tuple(self.fields.iter().map(|f| f.get_value()).collect())
    }

    pub(crate) fn set_value(&mut self, value: lutra_bin::Value) {
        let lutra_bin::Value::Tuple(values) = value else {
            panic!()
        };
        for (field, value) in std::iter::zip(&mut self.fields, values) {
            field.set_value(value);
        }
    }

    pub(crate) fn update(&mut self, action: &Action) -> bool {
        match action {
            Action::Select => {
                self.is_folded = !self.is_folded;
                true
            }
            _ => false,
        }
    }
}
