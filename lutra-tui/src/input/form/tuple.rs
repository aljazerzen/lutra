use crossterm::event::KeyCode;
use lutra_bin::ir;

use super::{Form, FormName, FormResult, TyDefs};
use crate::terminal::{Action, Line, Span, Style, View};

#[derive(Clone)]
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

    pub fn render<'a>(&'a self, form: &Form, focused: bool) -> View<'a> {
        let name = form.get_name();
        let mut line = Line::empty();
        line.push_span(Span::styled(name.into_owned(), form.name_style(focused)));

        if self.is_folded {
            line.push_span(Span::new(" "));
            let ellipsis_style = if focused && form.cursor {
                Style::cursor()
            } else {
                Style::muted()
            };
            let cursor_col = line.width() as u16;
            line.push_span(Span::styled("...", ellipsis_style));
            let mut view = View::from(line);
            if focused && form.cursor {
                view.cursor = Some((0, cursor_col));
            }
            return view;
        }

        let mut view = View::from(line);
        if focused && form.cursor {
            view.cursor = Some((0, 0));
        }

        for field in &self.fields {
            let mut child = field.view(focused);
            child.prefix(Span::new("  "));
            view.extend_lines(child);
        }
        view
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

    pub(crate) fn handle(&mut self, action: &Action) -> FormResult {
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
