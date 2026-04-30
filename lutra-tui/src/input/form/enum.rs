use crossterm::event::KeyCode;
use lutra_bin::ir;

use super::{Form, FormKind, FormName, FormResult, TyDefs};
use crate::terminal::{Action, Span, Style, View};

#[derive(Clone)]
pub struct EnumForm {
    pub selected: usize,
    pub variants: Vec<Form>,
}
impl EnumForm {
    pub fn new(variants: &[ir::TyEnumVariant], ty_defs: TyDefs) -> EnumForm {
        let variants = variants
            .iter()
            .enumerate()
            .map(|(pos, variant)| {
                let name = FormName {
                    name: Some(variant.name.clone()),
                    position: Some(pos),
                };

                Form::new(&variant.ty, name, ty_defs.clone())
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

    pub fn render<'a>(&'a self, form: &'a Form, focused: bool) -> View<'a> {
        let mut view = View::from(form.render_name_prefix(focused));
        let form_selected = focused && form.cursor;

        for (pos, variant) in self.variants.iter().enumerate() {
            if pos > 0 {
                view.push_span(Span::new("  "));
            }

            let variant_selected = pos == self.selected;
            let style = match (form_selected, variant_selected) {
                (true, true) => Style::cursor(),
                (true, false) => Style::new(),

                (false, true) => Style::new().bold(),
                (false, false) => Style::muted(),
            };
            view.push_span(Span::styled(variant.get_name().into_owned(), style));

            // cursor
            if form_selected && variant_selected {
                view.set_cursor_inline();
            }
        }

        if let Some(selected) = self.variants.get(self.selected) {
            if let FormKind::Tuple(tuple) = &selected.kind {
                for field in &tuple.fields {
                    let mut child = field.view(focused);
                    child.prefix(Span::new("  "));
                    view.extend_lines(child);
                }
            } else {
                let mut child = selected.view(focused);
                child.prefix(Span::new("  "));
                view.extend_lines(child);
            }
        }

        view
    }

    pub fn handle(&mut self, action: &Action) -> FormResult {
        let Some(key) = action.as_key() else {
            return FormResult::None;
        };

        match key.code {
            KeyCode::Left => {
                self.selected = self.selected.saturating_sub(1);
                FormResult::Redraw
            }
            KeyCode::Right => {
                self.selected = self.selected.saturating_add(1);
                if self.selected >= self.variants.len() {
                    self.selected = self.variants.len() - 1;
                }
                FormResult::Redraw
            }
            _ => FormResult::None,
        }
    }

    pub(crate) fn get_value(&self) -> lutra_bin::Value {
        let selected_form = self.variants.get(self.selected).unwrap();
        lutra_bin::Value::Enum(self.selected, Box::new(selected_form.get_value()))
    }

    pub(crate) fn set_value(&mut self, value: lutra_bin::Value) {
        let lutra_bin::Value::Enum(tag, value) = value else {
            panic!()
        };
        self.selected = tag;
        self.variants.get_mut(tag).unwrap().set_value(*value);
    }
}
