use crossterm::event::KeyCode;
use lutra_bin::ir;
use ratatui::prelude::*;

use super::{Form, FormName, FormResult, TyDefs};
use crate::terminal::Action;
use crate::utils::{clip_left, clip_top};

pub struct TupleForm {
    pub fields: Vec<Form>,
    pub is_folded: bool,
}

impl TupleForm {
    pub fn new(fields: &[ir::TyTupleField], ty_defs: TyDefs) -> Self {
        let fields: Vec<_> = fields
            .iter()
            .enumerate()
            .map(|(position, field)| {
                let ty = &field.ty;
                let name = FormName {
                    name: field.name.clone(),
                    position: Some(position),
                };
                Form::new(ty, name, ty_defs.clone())
            })
            .collect();
        TupleForm {
            is_folded: fields.len() > 10,
            fields,
        }
    }

    pub fn render(&self, form: &Form, frame: &mut Frame, area: Rect) -> Rect {
        let name = form.get_name();

        frame.render_widget(name.as_ref().white(), area);
        let after_name_area = clip_left(area, name.len() as u16 + 1);

        if self.is_folded {
            if form.cursor {
                frame.render_widget("...".black().on_white(), after_name_area);
            } else {
                frame.render_widget("...".white(), after_name_area);
            }

            clip_top(area, 1)
        } else {
            if form.cursor {
                frame.render_widget(" ".black().on_white(), after_name_area);
            }

            let mut next_field_area = clip_top(clip_left(area, 2), 1);
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

    pub(crate) fn update(&mut self, action: &Action) -> FormResult {
        let Some(key) = action.as_key() else {
            return FormResult::None;
        };

        match key.code {
            KeyCode::Enter => {
                self.is_folded = !self.is_folded;
                FormResult::Redraw
            }
            _ => FormResult::None,
        }
    }
}
