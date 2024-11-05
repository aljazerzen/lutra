use lutra_frontend::pr;
use ratatui::{
    layout::{Offset, Rect},
    style::Stylize,
    Frame,
};

use super::{Form, FormName};

pub struct TupleForm {
    pub fields: Vec<Form>,
}

impl TupleForm {
    pub fn new(fields: &[pr::TyTupleField]) -> Self {
        let fields = fields
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
        TupleForm { fields }
    }

    pub fn render(&self, form: &Form, frame: &mut Frame, area: Rect) -> Rect {
        let name = form.get_name();

        frame.render_widget(name.white(), area);

        let mut next_field_area = area.offset(Offset { x: 2, y: 1 });
        for field in &self.fields {
            next_field_area = field.render(frame, next_field_area);
        }

        // deindent
        next_field_area.x -= 2;
        next_field_area.width += 2;

        next_field_area
    }

    pub(crate) fn get_value(&self) -> lutra_bin::Value {
        lutra_bin::Value::Tuple(self.fields.iter().map(|f| f.get_value()).collect())
    }
}
