mod array;
mod bool;
mod r#enum;
mod int;
mod text;
mod tuple;

use bool::BoolForm;
use int::IntForm;
use layout::Offset;
use lutra_bin::ir;
use ratatui::prelude::*;
use std::borrow::Cow;

use super::Action;

use self::array::ArrayForm;
use self::r#enum::EnumForm;
use self::text::TextForm;
use self::tuple::TupleForm;

pub struct Form {
    pub name: FormName,
    pub ty: ir::Ty,
    pub kind: FormKind,
    pub focus: bool,
}

pub enum FormKind {
    Bool(BoolForm),
    Text(TextForm),
    Int(IntForm),
    Tuple(TupleForm),
    Enum(EnumForm),
    Array(ArrayForm),
}

impl Form {
    pub fn new(ty: &ir::Ty, name: FormName) -> Self {
        let kind: FormKind = match &ty.kind {
            ir::TyKind::Primitive(ir::TyPrimitive::text) => {
                FormKind::Text(TextForm::new(String::new()))
            }
            ir::TyKind::Primitive(ir::TyPrimitive::int64) => FormKind::Int(IntForm::new(0)),
            ir::TyKind::Primitive(ir::TyPrimitive::bool) => FormKind::Bool(BoolForm::new(false)),
            ir::TyKind::Primitive(_) => todo!(),

            ir::TyKind::Enum(variants) => FormKind::Enum(EnumForm::new(variants)),
            ir::TyKind::Tuple(fields) => FormKind::Tuple(TupleForm::new(fields)),

            ir::TyKind::Array(_) => FormKind::Array(ArrayForm::new()),

            ir::TyKind::Ident(_) | ir::TyKind::Function(_) => unimplemented!(),
        };

        let name = ty
            .name
            .clone()
            .map(|name| FormName {
                name: Some(name),
                position: None,
            })
            .unwrap_or(name);

        Form {
            name,
            ty: ty.clone(),
            focus: false,
            kind,
        }
    }

    fn get_name(&self) -> Cow<'_, str> {
        self.name.as_str().unwrap_or(Cow::Borrowed("(unnamed)"))
    }

    pub fn render(&self, frame: &mut Frame, area: Rect) -> Rect {
        if area.is_empty() {
            return area;
        }

        // render inner
        match &self.kind {
            FormKind::Bool(form) => form.render(self, frame, area),
            FormKind::Text(form) => form.render(self, frame, area),
            FormKind::Int(form) => form.render(self, frame, area),
            FormKind::Tuple(form) => form.render(self, frame, area),
            FormKind::Enum(form) => form.render(self, frame, area),
            FormKind::Array(form) => form.render(self, frame, area),
        }
    }

    pub fn update(&mut self, action: &Action) -> bool {
        match &mut self.kind {
            FormKind::Bool(form) => form.update(action),
            FormKind::Text(form) => form.update(action),
            FormKind::Int(form) => form.update(action),
            FormKind::Tuple(form) => form.update(action),
            FormKind::Enum(form) => form.update(action),
            FormKind::Array(form) => form.update(action, &self.ty),
        }
    }

    pub fn get_mut(&mut self, path: &[usize]) -> Option<&mut Form> {
        if path.is_empty() || matches!(self.kind, FormKind::Text(_)) {
            return Some(self);
        }
        let path_step = *path.first().unwrap();
        match &mut self.kind {
            FormKind::Tuple(form) => {
                let field = form.fields.get_mut(path_step)?;
                field.get_mut(&path[1..])
            }
            FormKind::Enum(form) => {
                let variant = form.variants.get_mut(path_step)?;
                variant.get_mut(&path[1..])
            }
            FormKind::Array(form) => {
                let variant = form.items.get_mut(path_step)?;
                variant.get_mut(&path[1..])
            }
            FormKind::Text(_) | FormKind::Int(_) | FormKind::Bool(_) => unreachable!(),
        }
    }

    /// Walks over all focusable forms and stops when a focused form is found.
    /// Returns number of passed forms and boolean indicating that focus has been found.
    pub fn take_focus(&mut self) -> (usize, bool) {
        if self.focus {
            self.focus = false;
            return (0, true);
        }
        let passed_forms = match &mut self.kind {
            FormKind::Tuple(form) => {
                if !form.is_folded {
                    let mut position = 0;
                    for field in &mut form.fields {
                        let (p, f) = field.take_focus();
                        position += p;
                        if f {
                            return (position, true);
                        }
                    }
                    position
                } else {
                    1
                }
            }
            FormKind::Enum(form) => {
                let mut position = 1;
                if let Some(selected) = form.get_selected_mut() {
                    let (p, f) = selected.take_focus();
                    position += p;
                    if f {
                        return (position, true);
                    }
                }
                position
            }
            FormKind::Array(form) => {
                let mut position = 1;
                for item in &mut form.items {
                    let (p, f) = item.take_focus();
                    position += p;
                    if f {
                        return (position, true);
                    }
                }
                position
            }
            FormKind::Text(_) | FormKind::Int(_) | FormKind::Bool(_) => 1,
        };
        (passed_forms, false)
    }

    /// Walks over focusable forms and sets focus at the specified position. If that
    /// position is not found, `Err(position - number of focusable nodes)` is returned.
    pub fn insert_focus(&mut self, mut position: usize) -> Result<Vec<usize>, usize> {
        match &mut self.kind {
            FormKind::Tuple(form) =>
            {
                #[allow(clippy::collapsible_else_if)]
                if !form.is_folded {
                    for (pos, field) in form.fields.iter_mut().enumerate() {
                        match field.insert_focus(position) {
                            Ok(mut path) => {
                                path.insert(0, pos);
                                return Ok(path);
                            }
                            Err(p) => position = p,
                        }
                    }
                    Err(position)
                } else {
                    if position == 0 {
                        self.focus = true;
                        Ok(vec![])
                    } else {
                        Err(position - 1)
                    }
                }
            }
            FormKind::Enum(form) => {
                if position == 0 {
                    self.focus = true;
                    Ok(vec![])
                } else {
                    if let Some(variant) = form.get_selected_mut() {
                        match variant.insert_focus(position - 1) {
                            Ok(mut path) => {
                                path.insert(0, form.selected);
                                return Ok(path);
                            }
                            Err(p) => position = p,
                        }
                    }
                    Err(position)
                }
            }
            FormKind::Array(form) => {
                if position == 0 {
                    self.focus = true;
                    return Ok(vec![]);
                }
                position -= 1;
                for (pos, field) in form.items.iter_mut().enumerate() {
                    match field.insert_focus(position) {
                        Ok(mut path) => {
                            path.insert(0, pos);
                            return Ok(path);
                        }
                        Err(p) => position = p,
                    }
                }
                Err(position)
            }
            FormKind::Text(_) | FormKind::Int(_) | FormKind::Bool(_) => {
                if position == 0 {
                    self.focus = true;
                    Ok(vec![])
                } else {
                    Err(position - 1)
                }
            }
        }
    }

    pub fn get_value(&self) -> lutra_bin::Value {
        match &self.kind {
            FormKind::Bool(form) => form.get_value(),
            FormKind::Text(form) => form.get_value(),
            FormKind::Int(form) => form.get_value(),
            FormKind::Tuple(form) => form.get_value(),
            FormKind::Enum(form) => form.get_value(),
            FormKind::Array(form) => form.get_value(),
        }
    }

    pub fn set_value(&mut self, value: lutra_bin::Value) {
        match &mut self.kind {
            FormKind::Bool(form) => form.set_value(value),
            FormKind::Text(form) => form.set_value(value),
            FormKind::Int(form) => form.set_value(value),
            FormKind::Tuple(form) => form.set_value(value),
            FormKind::Enum(form) => form.set_value(value),
            FormKind::Array(form) => form.set_value(value, &self.ty),
        }
    }
}

#[derive(Default)]
pub struct FormName {
    name: Option<String>,
    position: Option<usize>,
}

impl FormName {
    fn as_str(&self) -> Option<Cow<'_, str>> {
        if let Some(name) = &self.name {
            return Some(Cow::from(name));
        }
        if let Some(pos) = &self.position {
            return Some(Cow::from(pos.to_string()));
        }
        None
    }
}

pub fn render_name_colon(form: &Form, frame: &mut Frame, area: Rect) -> Rect {
    let name = form.get_name();

    let name_span = if form.focus {
        name.white().bold()
    } else {
        name.white()
    };
    frame.render_widget(name_span, area);

    let area_colon = area.offset(Offset {
        x: name.len() as i32,
        y: 0,
    });
    frame.render_widget(":".white(), area_colon);

    area_colon
        .offset(Offset { x: 2, y: 0 })
        .intersection(frame.area())
}
