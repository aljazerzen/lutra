use crossterm::event::Event;
use lutra_bin::ir;

use super::text::TextForm;
use super::{Form, FormResult};
use crate::terminal::{Action, View};

#[derive(Clone)]
pub struct NumberForm {
    inner: TextForm,
    primitive: ir::TyPrimitive,
}

impl NumberForm {
    pub fn new(primitive: ir::TyPrimitive, value: String) -> NumberForm {
        NumberForm {
            inner: TextForm::new(value),
            primitive,
        }
    }

    pub fn render<'a>(&'a self, form: &'a Form, focused: bool) -> View<'a> {
        self.inner.render(form, focused)
    }

    pub fn handle(&mut self, action: &Action) -> FormResult {
        match action {
            Action::Terminal(Event::Paste(text)) => {
                if text.chars().all(|c| self.is_allowed_char(c)) {
                    self.inner.handle(action)
                } else {
                    FormResult::None
                }
            }
            _ => {
                let Some(key) = action.as_key() else {
                    return FormResult::None;
                };

                if let crossterm::event::KeyCode::Char(c) = key.code
                    && !key
                        .modifiers
                        .contains(crossterm::event::KeyModifiers::CONTROL)
                    && !self.is_allowed_char(c)
                {
                    return FormResult::None;
                }

                self.inner.handle(action)
            }
        }
    }

    pub(crate) fn get_value(&self) -> lutra_bin::Value {
        let Ok(text) = self.inner.get_value().expect_text_cloned() else {
            panic!("NumberForm inner value should be Text")
        };

        match self.primitive {
            ir::TyPrimitive::prim8 => {
                let val = text.parse::<i8>().unwrap_or_default();
                lutra_bin::Value::Prim8(val as u8)
            }
            ir::TyPrimitive::prim16 => {
                let val = text.parse::<i16>().unwrap_or_default();
                lutra_bin::Value::Prim16(val as u16)
            }
            ir::TyPrimitive::prim32 => {
                let val = text.parse::<i32>().unwrap_or_default();
                lutra_bin::Value::Prim32(val as u32)
            }
            ir::TyPrimitive::prim64 => {
                let val = text.parse::<i64>().unwrap_or_default();
                lutra_bin::Value::Prim64(val as u64)
            }
        }
    }

    pub(crate) fn set_value(&mut self, value: lutra_bin::Value) {
        let text = match (&value, self.primitive) {
            (lutra_bin::Value::Prim8(v), ir::TyPrimitive::prim8) => (*v as i8).to_string(),
            (lutra_bin::Value::Prim16(v), ir::TyPrimitive::prim16) => (*v as i16).to_string(),
            (lutra_bin::Value::Prim32(v), ir::TyPrimitive::prim32) => (*v as i32).to_string(),
            (lutra_bin::Value::Prim64(v), ir::TyPrimitive::prim64) => (*v as i64).to_string(),
            _ => panic!(
                "Value type mismatch: expected {:?}, got {:?}",
                self.primitive, value
            ),
        };

        self.inner.set_value(lutra_bin::Value::new_text(&text));
    }

    fn is_allowed_char(&self, c: char) -> bool {
        match self.primitive {
            ir::TyPrimitive::prim8
            | ir::TyPrimitive::prim16
            | ir::TyPrimitive::prim32
            | ir::TyPrimitive::prim64 => c.is_ascii_digit() || c == '-',
        }
    }
}
