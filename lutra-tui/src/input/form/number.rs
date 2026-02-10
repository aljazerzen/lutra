use crossterm::event::KeyCode;
use lutra_bin::ir;
use ratatui::prelude::*;

use super::text::TextForm;
use super::{Action, Form, FormResult};

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

    pub fn render(&self, form: &Form, frame: &mut Frame<'_>, area: Rect) -> Rect {
        self.inner.render(form, frame, area)
    }

    pub fn handle(&mut self, action: &Action) -> FormResult {
        let Some(key) = action.as_key() else {
            return FormResult::None;
        };

        match key.code {
            KeyCode::Char(c) => {
                // Filter characters based on primitive type
                let allowed = match self.primitive {
                    // Signed integers: allow minus sign and digits
                    ir::TyPrimitive::int8
                    | ir::TyPrimitive::int16
                    | ir::TyPrimitive::int32
                    | ir::TyPrimitive::int64 => c.is_ascii_digit() || c == '-',

                    // Unsigned integers: only digits
                    ir::TyPrimitive::uint8
                    | ir::TyPrimitive::uint16
                    | ir::TyPrimitive::uint32
                    | ir::TyPrimitive::uint64 => c.is_ascii_digit(),

                    // Floats: allow digits, decimal point, minus, and scientific notation
                    ir::TyPrimitive::float32 | ir::TyPrimitive::float64 => {
                        c.is_ascii_digit() || c == '.' || c == '-' || c == 'e' || c == 'E'
                    }

                    _ => panic!(
                        "NumberForm used with non-numeric primitive: {:?}",
                        self.primitive
                    ),
                };

                if allowed {
                    self.inner.handle(action)
                } else {
                    FormResult::None
                }
            }
            KeyCode::Backspace => self.inner.handle(action),
            _ => FormResult::None,
        }
    }

    pub(crate) fn get_value(&self) -> lutra_bin::Value {
        let lutra_bin::Value::Text(text) = self.inner.get_value() else {
            panic!("NumberForm inner value should be Text")
        };

        match self.primitive {
            // 8-bit types → Value::Prim8
            ir::TyPrimitive::int8 => {
                let val = text.parse::<i8>().unwrap_or_default();
                lutra_bin::Value::Prim8(val as u8)
            }
            ir::TyPrimitive::uint8 => {
                let val = text.parse::<u8>().unwrap_or_default();
                lutra_bin::Value::Prim8(val)
            }

            // 16-bit types → Value::Prim16
            ir::TyPrimitive::int16 => {
                let val = text.parse::<i16>().unwrap_or_default();
                lutra_bin::Value::Prim16(val as u16)
            }
            ir::TyPrimitive::uint16 => {
                let val = text.parse::<u16>().unwrap_or_default();
                lutra_bin::Value::Prim16(val)
            }

            // 32-bit types → Value::Prim32
            ir::TyPrimitive::int32 => {
                let val = text.parse::<i32>().unwrap_or_default();
                lutra_bin::Value::Prim32(val as u32)
            }
            ir::TyPrimitive::uint32 => {
                let val = text.parse::<u32>().unwrap_or_default();
                lutra_bin::Value::Prim32(val)
            }
            ir::TyPrimitive::float32 => {
                let val = text.parse::<f32>().unwrap_or_default();
                lutra_bin::Value::Prim32(val.to_bits())
            }

            // 64-bit types → Value::Prim64
            ir::TyPrimitive::int64 => {
                let val = text.parse::<i64>().unwrap_or_default();
                lutra_bin::Value::Prim64(val as u64)
            }
            ir::TyPrimitive::uint64 => {
                let val = text.parse::<u64>().unwrap_or_default();
                lutra_bin::Value::Prim64(val)
            }
            ir::TyPrimitive::float64 => {
                let val = text.parse::<f64>().unwrap_or_default();
                lutra_bin::Value::Prim64(val.to_bits())
            }

            _ => panic!(
                "NumberForm used with non-numeric primitive: {:?}",
                self.primitive
            ),
        }
    }

    pub(crate) fn set_value(&mut self, value: lutra_bin::Value) {
        let text = match (&value, self.primitive) {
            // 8-bit types
            (lutra_bin::Value::Prim8(v), ir::TyPrimitive::int8) => (*v as i8).to_string(),
            (lutra_bin::Value::Prim8(v), ir::TyPrimitive::uint8) => v.to_string(),

            // 16-bit types
            (lutra_bin::Value::Prim16(v), ir::TyPrimitive::int16) => (*v as i16).to_string(),
            (lutra_bin::Value::Prim16(v), ir::TyPrimitive::uint16) => v.to_string(),

            // 32-bit types
            (lutra_bin::Value::Prim32(v), ir::TyPrimitive::int32) => (*v as i32).to_string(),
            (lutra_bin::Value::Prim32(v), ir::TyPrimitive::uint32) => v.to_string(),
            (lutra_bin::Value::Prim32(v), ir::TyPrimitive::float32) => {
                f32::from_bits(*v).to_string()
            }

            // 64-bit types
            (lutra_bin::Value::Prim64(v), ir::TyPrimitive::int64) => (*v as i64).to_string(),
            (lutra_bin::Value::Prim64(v), ir::TyPrimitive::uint64) => v.to_string(),
            (lutra_bin::Value::Prim64(v), ir::TyPrimitive::float64) => {
                f64::from_bits(*v).to_string()
            }

            _ => panic!(
                "Value type mismatch: expected {:?}, got {:?}",
                self.primitive, value
            ),
        };

        self.inner.set_value(lutra_bin::Value::Text(text));
    }
}
