mod array;
mod bool;
mod button;
mod r#enum;
mod number;
mod text;
mod tuple;

use lutra_bin::ir;
use ratatui::prelude::*;
use std::borrow::Cow;
use std::collections::HashMap;
use std::rc::Rc;

use super::Action;

use self::array::ArrayForm;
use self::bool::BoolForm;
use self::button::ButtonForm;
use self::r#enum::EnumForm;
use self::number::NumberForm;
use self::text::TextForm;
use self::tuple::TupleForm;

pub type TyDefs = Rc<HashMap<ir::Path, ir::Ty>>;

pub struct Form {
    pub name: FormName,
    pub ty: ir::Ty,
    pub kind: FormKind,
    pub cursor: bool,
    pub ty_defs: TyDefs,
}

pub enum FormKind {
    Bool(BoolForm),
    Text(TextForm),
    Number(NumberForm),
    Tuple(TupleForm),
    TupleUnit,
    Enum(EnumForm),
    Array(ArrayForm),
    Button(ButtonForm),
}

pub enum FormResult {
    None,
    Redraw,
    Submit,
}

/// Recursively resolve type identifiers to concrete types
fn resolve_ty<'a>(ty: &'a ir::Ty, ty_defs: &'a TyDefs) -> &'a ir::Ty {
    match &ty.kind {
        ir::TyKind::Ident(path) => {
            let resolved = ty_defs
                .get(path)
                .unwrap_or_else(|| panic!("Type identifier {:?} not found in definitions", path));
            resolve_ty(resolved, ty_defs)
        }
        _ => ty,
    }
}

impl Form {
    pub fn new_with_submit(
        ty: &ir::Ty,
        name: FormName,
        ty_defs: TyDefs,
        submit_text: &'static str,
    ) -> Self {
        let form = Form::new(ty, name, ty_defs.clone());

        Self {
            name: FormName::default(),
            kind: FormKind::Button(ButtonForm::new(form, submit_text)),
            cursor: false,
            ty: ir::Ty::new_unit(), // does not matter
            ty_defs,
        }
    }

    pub fn new(ty: &ir::Ty, name: FormName, ty_defs: TyDefs) -> Self {
        // Resolve any type identifiers
        let resolved_ty = resolve_ty(ty, &ty_defs);

        let kind: FormKind = match &resolved_ty.kind {
            ir::TyKind::Primitive(ir::TyPrimitive::text) => {
                FormKind::Text(TextForm::new(String::new()))
            }
            ir::TyKind::Primitive(ir::TyPrimitive::bool) => FormKind::Bool(BoolForm::new(false)),

            // All numeric types use NumberForm
            ir::TyKind::Primitive(
                prim @ (ir::TyPrimitive::int8
                | ir::TyPrimitive::int16
                | ir::TyPrimitive::int32
                | ir::TyPrimitive::int64
                | ir::TyPrimitive::uint8
                | ir::TyPrimitive::uint16
                | ir::TyPrimitive::uint32
                | ir::TyPrimitive::uint64
                | ir::TyPrimitive::float32
                | ir::TyPrimitive::float64),
            ) => FormKind::Number(NumberForm::new(*prim, "0".into())),

            ir::TyKind::Enum(variants) => FormKind::Enum(EnumForm::new(variants, ty_defs.clone())),
            ir::TyKind::Tuple(fields) if fields.is_empty() => FormKind::TupleUnit,
            ir::TyKind::Tuple(fields) => FormKind::Tuple(TupleForm::new(fields, ty_defs.clone())),

            ir::TyKind::Array(_) => FormKind::Array(ArrayForm::new()),

            ir::TyKind::Ident(_) => panic!("Type identifier should have been resolved"),
            ir::TyKind::Function(_) => panic!("Function types cannot be input in forms"),
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
            cursor: false,
            kind,
            ty_defs,
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
            FormKind::Number(form) => form.render(self, frame, area),
            FormKind::Tuple(form) => form.render(self, frame, area),
            FormKind::TupleUnit => area,
            FormKind::Enum(form) => form.render(self, frame, area),
            FormKind::Array(form) => form.render(self, frame, area),
            FormKind::Button(form) => form.render(self, frame, area),
        }
    }

    pub fn update(&mut self, action: &Action) -> FormResult {
        match &mut self.kind {
            FormKind::Bool(form) => form.update(action),
            FormKind::Text(form) => form.update(action),
            FormKind::Number(form) => form.update(action),
            FormKind::Tuple(form) => form.update(action),
            FormKind::TupleUnit => FormResult::None,
            FormKind::Enum(form) => form.update(action),
            FormKind::Array(form) => form.update(action, &self.ty, &self.ty_defs),
            FormKind::Button(form) => form.update(action),
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
            FormKind::TupleUnit => None,
            FormKind::Enum(form) => {
                let variant = form.variants.get_mut(path_step)?;
                variant.get_mut(&path[1..])
            }
            FormKind::Array(form) => {
                let variant = form.items.get_mut(path_step)?;
                variant.get_mut(&path[1..])
            }
            FormKind::Button(form) => form.inner.get_mut(&path[1..]),
            FormKind::Text(_) | FormKind::Number(_) | FormKind::Bool(_) => unreachable!(),
        }
    }

    /// Walks over all focusable forms and stops when the cursor form is found.
    /// Returns number of passed forms and boolean indicating that cursor has been found.
    pub fn take_cursor(&mut self) -> (usize, bool) {
        if self.cursor && !matches!(self.kind, FormKind::Button(_)) {
            self.cursor = false;
            return (0, true);
        }
        let passed_forms = match &mut self.kind {
            FormKind::Tuple(form) => {
                if !form.is_folded {
                    let mut position = 0;
                    for field in &mut form.fields {
                        let (p, f) = field.take_cursor();
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
            FormKind::TupleUnit => 0,
            FormKind::Enum(form) => {
                let mut position = 1;
                if let Some(selected) = form.get_selected_mut() {
                    let (p, f) = selected.take_cursor();
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
                    let (p, f) = item.take_cursor();
                    position += p;
                    if f {
                        return (position, true);
                    }
                }
                position
            }
            FormKind::Button(form) => {
                let (p, f) = form.inner.take_cursor();
                if f {
                    return (p, true);
                } else if self.cursor {
                    self.cursor = false;
                    return (p, true);
                } else {
                    p + 1
                }
            }
            FormKind::Text(_) | FormKind::Number(_) | FormKind::Bool(_) => 1,
        };
        (passed_forms, false)
    }

    /// Walks over focusable forms and sets cursor at the specified position. If that
    /// position is not found, `Err(position - number of focusable nodes)` is returned.
    pub fn insert_cursor(&mut self, mut position: usize) -> Result<Vec<usize>, usize> {
        match &mut self.kind {
            FormKind::Tuple(form) =>
            {
                #[allow(clippy::collapsible_else_if)]
                if !form.is_folded {
                    for (pos, field) in form.fields.iter_mut().enumerate() {
                        match field.insert_cursor(position) {
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
                        self.cursor = true;
                        Ok(vec![])
                    } else {
                        Err(position - 1)
                    }
                }
            }
            FormKind::TupleUnit => Err(position),
            FormKind::Enum(form) => {
                if position == 0 {
                    self.cursor = true;
                    Ok(vec![])
                } else {
                    if let Some(variant) = form.get_selected_mut() {
                        match variant.insert_cursor(position - 1) {
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
                    self.cursor = true;
                    return Ok(vec![]);
                }
                position -= 1;
                for (pos, field) in form.items.iter_mut().enumerate() {
                    match field.insert_cursor(position) {
                        Ok(mut path) => {
                            path.insert(0, pos);
                            return Ok(path);
                        }
                        Err(p) => position = p,
                    }
                }
                Err(position)
            }
            FormKind::Text(_) | FormKind::Number(_) | FormKind::Bool(_) => {
                if position == 0 {
                    self.cursor = true;
                    Ok(vec![])
                } else {
                    Err(position - 1)
                }
            }
            FormKind::Button(form) => match form.inner.insert_cursor(position) {
                Ok(mut path) => {
                    // found, prefix with 0
                    path.insert(0, 0);
                    Ok(path)
                }
                Err(0) => {
                    // cursor on button
                    self.cursor = true;
                    Ok(vec![])
                }
                Err(p) => Err(p - 1), // cursor somewhere after button
            },
        }
    }

    pub fn get_value(&self) -> lutra_bin::Value {
        match &self.kind {
            FormKind::Bool(form) => form.get_value(),
            FormKind::Text(form) => form.get_value(),
            FormKind::Number(form) => form.get_value(),
            FormKind::Tuple(form) => form.get_value(),
            FormKind::TupleUnit => lutra_bin::Value::unit(),
            FormKind::Enum(form) => form.get_value(),
            FormKind::Array(form) => form.get_value(),
            FormKind::Button(form) => form.inner.get_value(),
        }
    }

    pub fn set_value(&mut self, value: lutra_bin::Value) {
        match &mut self.kind {
            FormKind::Bool(form) => form.set_value(value),
            FormKind::Text(form) => form.set_value(value),
            FormKind::Number(form) => form.set_value(value),
            FormKind::Tuple(form) => form.set_value(value),
            FormKind::TupleUnit => {}
            FormKind::Enum(form) => form.set_value(value),
            FormKind::Array(form) => form.set_value(value, &self.ty, &self.ty_defs),
            FormKind::Button(form) => form.inner.set_value(value),
        }
    }
}

#[derive(Default)]
pub struct FormName {
    name: Option<String>,
    position: Option<usize>,
}

impl From<String> for FormName {
    fn from(value: String) -> Self {
        Self {
            name: Some(value),
            position: None,
        }
    }
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

    let name_span = if form.cursor {
        name.as_ref().white().bold()
    } else {
        name.as_ref().white()
    };
    frame.render_widget(name_span, area);

    let area_colon = clip_left(area, name.len() as u16);
    frame.render_widget(":".white(), area_colon);

    clip_left(area_colon, 2)
}

fn clip_left(mut area: Rect, left: u16) -> Rect {
    let left = left.min(area.width);
    area.x += left;
    area.width -= left;
    area
}
fn clip_top(mut area: Rect, top: u16) -> Rect {
    let top = top.min(area.height);
    area.y += top;
    area.height -= top;
    area
}
