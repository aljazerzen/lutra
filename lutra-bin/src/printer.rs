#![cfg(feature = "std")]

use std::collections::HashMap;
use std::string;

use crate::ir;
use crate::visitor::Visitor;
use crate::{Error, Result};

pub fn print_source(buf: &[u8], ty: &ir::Ty, ty_defs: &[ir::TyDef]) -> Result<String> {
    let mut printer = Printer {
        indent: 0,
        ty_defs: ty_defs.iter().map(|def| (&def.name, &def.ty)).collect(),
    };

    printer.visit(buf, ty)
}

#[derive(Clone)]
struct Printer<'t> {
    indent: usize,
    ty_defs: HashMap<&'t ir::Path, &'t ir::Ty>,
}

const INDENT: usize = 2;

impl<'t> Printer<'t> {
    fn indent(&mut self) {
        self.indent += INDENT;
    }

    fn deindent(&mut self) {
        self.indent -= INDENT;
    }

    fn new_line(&self) -> String {
        let mut r = "\n".to_string();
        r += &" ".repeat(self.indent);
        r
    }
}

impl<'t, B> Visitor<'t, B> for Printer<'t>
where
    B: bytes::Buf + Clone,
{
    type Res = String;

    fn get_ty(&self, name: &ir::Path) -> &'t ir::Ty {
        self.ty_defs
            .get(name)
            .unwrap_or_else(|| panic!("bad program: cannot find {name:?}"))
    }

    fn visit_bool(&mut self, v: bool) -> Result<Self::Res, Error> {
        Ok(if v {
            "true".to_string()
        } else {
            "false".to_string()
        })
    }

    fn visit_int8(&mut self, v: i8) -> Result<Self::Res, Error> {
        Ok(format!("{v}"))
    }
    fn visit_int16(&mut self, v: i16) -> Result<Self::Res, Error> {
        Ok(format!("{v}"))
    }
    fn visit_int32(&mut self, v: i32) -> Result<Self::Res, Error> {
        Ok(format!("{v}"))
    }
    fn visit_int64(&mut self, v: i64) -> Result<Self::Res, Error> {
        Ok(format!("{v}"))
    }
    fn visit_uint8(&mut self, v: u8) -> Result<Self::Res, Error> {
        Ok(format!("{v}"))
    }
    fn visit_uint16(&mut self, v: u16) -> Result<Self::Res, Error> {
        Ok(format!("{v}"))
    }
    fn visit_uint32(&mut self, v: u32) -> Result<Self::Res, Error> {
        Ok(format!("{v}"))
    }
    fn visit_uint64(&mut self, v: u64) -> Result<Self::Res, Error> {
        Ok(format!("{v}"))
    }
    fn visit_float32(&mut self, v: f32) -> Result<Self::Res, Error> {
        Ok(format!("{v:#?}"))
    }
    fn visit_float64(&mut self, v: f64) -> Result<Self::Res, Error> {
        Ok(format!("{v:#?}"))
    }

    fn visit_text(&mut self, mut contents: B, len: usize) -> Result<Self::Res, Error> {
        let mut buf = vec![0; len];
        contents.copy_to_slice(&mut buf);

        let s = string::String::from_utf8(buf).map_err(|_| Error::InvalidData)?;
        Ok(quote_text(&s))
    }

    fn visit_date(&mut self, days: i32) -> std::result::Result<Self::Res, crate::Error> {
        #[cfg(feature = "chrono")]
        {
            if let Some(date) = chrono::NaiveDate::from_epoch_days(days) {
                return Ok(format!("@{date}"));
            }
            // fallback to printing integers
            // (this might happen when date is out range)
        }
        Visitor::<'t, B>::visit_int32(self, days)
    }

    fn visit_time(&mut self, micros_midnight: u64) -> std::result::Result<Self::Res, crate::Error> {
        Ok(print_time(micros_midnight))
    }

    fn visit_duration(&mut self, micros_t: i64) -> std::result::Result<Self::Res, crate::Error> {
        Ok(print_duration(micros_t))
    }

    fn visit_timestamp(&mut self, micros: i64) -> std::result::Result<Self::Res, crate::Error> {
        #[cfg(feature = "chrono")]
        {
            if let Some(dt) = chrono::DateTime::from_timestamp_micros(micros) {
                let dt = dt.to_rfc3339_opts(chrono::SecondsFormat::Micros, true);
                return Ok(format!("@{}", dt.trim_end_matches('Z')));
            };
        }
        Visitor::<'t, B>::visit_int64(self, micros)
    }

    fn visit_decimal(&mut self, cent: i64) -> std::result::Result<Self::Res, crate::Error> {
        Ok(format!("{}.{:02}", cent / 100, (cent % 100).abs()))
    }

    fn visit_tuple(
        &mut self,
        fields: impl Iterator<Item = (B, &'t ir::TyTupleField)>,
    ) -> Result<Self::Res, Error> {
        let mut r = "{".to_string();
        let mut had_a_field = false;
        self.indent();
        for (field, ty) in fields {
            had_a_field = true;
            r += &self.new_line();

            if let Some(name) = &ty.name {
                r += name;
                r += " = ";
            }

            r += &self.visit(field, &ty.ty)?;
            r += ",";
        }
        self.deindent();
        if had_a_field {
            r += &self.new_line();
        }
        r += "}";
        Ok(r)
    }

    fn visit_array(
        &mut self,
        items: impl Iterator<Item = B>,
        ty_items: &'t ir::Ty,
    ) -> Result<Self::Res, Error> {
        let mut r = "[".to_string();

        let mut items = items.peekable();
        if items.peek().is_some() {
            self.indent();
            for item in items {
                r += &self.new_line();
                r += &self.visit(item, ty_items)?;
                r += ",";
            }
            self.deindent();
            r += &self.new_line();
        }

        r += "]";
        Ok(r)
    }

    fn visit_enum(
        &mut self,
        tag: usize,
        inner: B,
        ty_variants: &'t [ir::TyEnumVariant],
    ) -> Result<Self::Res, Error> {
        let variant = ty_variants.get(tag).ok_or(Error::InvalidData)?;

        let mut r = variant.name.to_string();

        let is_unit = variant.ty.kind.as_tuple().is_some_and(|x| x.is_empty());
        if !is_unit {
            r += "(";
            r += &self.visit(inner, &variant.ty)?;
            r += ")";
        }

        Ok(r)
    }
}

pub fn print_time(micros_midnight: u64) -> String {
    let micros = micros_midnight % 1_000_000;
    let sec_t = micros_midnight / 1_000_000;
    let sec = sec_t % 60;
    let min_t = sec_t / 60;
    let min = min_t % 60;
    let h = min_t / 60;
    format!("@{h:02}:{min:02}:{sec:02}.{micros:06}")
}

pub fn print_duration(micros_t: i64) -> String {
    let sign = if micros_t < 0 { "-" } else { "" };
    let abs = micros_t.unsigned_abs();
    let micros = abs % 1_000_000;
    let sec_t = abs / 1_000_000;
    let sec = sec_t % 60;
    let min_t = sec_t / 60;
    let min = min_t % 60;
    let h = min_t / 60;
    format!("@{sign}{h:02}:{min:02}:{sec:02}.{micros:06}")
}

fn quote_text(text: &str) -> String {
    let mut result = String::new();
    result.push('"');

    for c in text.chars() {
        match c {
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            c if c.is_ascii_control() => {
                let hex = format!("\\x{:02X}", c as u8);
                result.push_str(&hex);
            }
            _ => result.push(c),
        }
    }
    result.push('"');
    result
}
