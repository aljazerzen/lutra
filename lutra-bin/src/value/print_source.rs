#![cfg(feature = "std")]

use std::collections::HashMap;

use crate::ir;
use crate::{Result, Value};

use super::fold::ValueVisitor;

impl Value {
    pub fn print_source(&self, ty: &ir::Ty, ty_defs: &[ir::TyDef]) -> Result<String> {
        let ty_defs: HashMap<_, _, _> = ty_defs.iter().map(|def| (&def.name, &def.ty)).collect();
        let mut printer = Printer {
            indent: 0,
            ty_defs: &ty_defs,
        };

        printer.visit_value(self, ty)
    }
}

#[derive(Clone)]
struct Printer<'t> {
    indent: usize,
    ty_defs: &'t HashMap<&'t ir::Path, &'t ir::Ty>,
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

impl<'t> ValueVisitor<'t> for Printer<'t> {
    type Res = String;

    fn get_ty(&self, name: &ir::Path) -> &'t ir::Ty {
        self.ty_defs.get(name).unwrap()
    }

    fn visit_bool(&mut self, v: bool) -> Result<Self::Res, crate::Error> {
        Ok(if v {
            "true".to_string()
        } else {
            "false".to_string()
        })
    }

    fn visit_int8(&mut self, v: i8) -> Result<Self::Res, crate::Error> {
        Ok(format!("{v}"))
    }
    fn visit_int16(&mut self, v: i16) -> Result<Self::Res, crate::Error> {
        Ok(format!("{v}"))
    }
    fn visit_int32(&mut self, v: i32) -> Result<Self::Res, crate::Error> {
        Ok(format!("{v}"))
    }
    fn visit_int64(&mut self, v: i64) -> Result<Self::Res, crate::Error> {
        Ok(format!("{v}"))
    }
    fn visit_uint8(&mut self, v: u8) -> Result<Self::Res, crate::Error> {
        Ok(format!("{v}"))
    }
    fn visit_uint16(&mut self, v: u16) -> Result<Self::Res, crate::Error> {
        Ok(format!("{v}"))
    }
    fn visit_uint32(&mut self, v: u32) -> Result<Self::Res, crate::Error> {
        Ok(format!("{v}"))
    }
    fn visit_uint64(&mut self, v: u64) -> Result<Self::Res, crate::Error> {
        Ok(format!("{v}"))
    }
    fn visit_float32(&mut self, v: f32) -> Result<Self::Res, crate::Error> {
        Ok(format!("{v:#?}"))
    }
    fn visit_float64(&mut self, v: f64) -> Result<Self::Res, crate::Error> {
        Ok(format!("{v:#?}"))
    }

    fn visit_text(&mut self, v: &str) -> Result<Self::Res, crate::Error> {
        // TODO escape strings
        Ok(format!("\"{v}\""))
    }

    fn visit_date(&mut self, days: i32) -> Result<Self::Res, crate::Error> {
        #[cfg(feature = "chrono")]
        {
            if let Some(date) = chrono::NaiveDate::from_epoch_days(days) {
                return Ok(format!("@{date}"));
            }
        }
        ValueVisitor::<'t>::visit_int32(self, days)
    }

    fn visit_time(&mut self, micros_t: i64) -> Result<Self::Res, crate::Error> {
        let micros = (micros_t % 1000000).abs();
        let sec_t = micros_t / 1000000;

        let sec = (sec_t % 60).abs();
        let min_t = sec_t / 60;

        let min = (min_t % 60).abs();
        let h_t = min_t / 60;

        Ok(format!("@{h_t:02}:{min:02}:{sec:02}.{micros:06}"))
    }

    fn visit_timestamp(&mut self, micros: i64) -> Result<Self::Res, crate::Error> {
        #[cfg(feature = "chrono")]
        {
            if let Some(dt) = chrono::DateTime::from_timestamp_micros(micros) {
                let dt = dt.to_rfc3339_opts(chrono::SecondsFormat::Micros, true);
                return Ok(format!("@{}", dt.trim_end_matches('Z')));
            }
        }
        ValueVisitor::<'t>::visit_int64(self, micros)
    }

    fn visit_decimal(&mut self, cent: i64) -> Result<Self::Res, crate::Error> {
        Ok(format!("{}.{:02}", cent / 100, (cent % 100).abs()))
    }

    fn visit_tuple(
        &mut self,
        fields: &[Value],
        ty_fields: &'t [ir::TyTupleField],
    ) -> Result<Self::Res, crate::Error> {
        let mut r = "{".to_string();
        self.indent();
        for (field, ty) in fields.iter().zip(ty_fields) {
            r += &self.new_line();

            if let Some(name) = &ty.name {
                r += name;
                r += " = ";
            }

            r += &self.visit_value(field, &ty.ty)?;
            r += ",";
        }
        self.deindent();
        r += &self.new_line();
        r += "}";
        Ok(r)
    }

    fn visit_array(
        &mut self,
        items: &[Value],
        ty_items: &'t ir::Ty,
    ) -> Result<Self::Res, crate::Error> {
        let mut r = "[".to_string();

        if !items.is_empty() {
            self.indent();
            for item in items {
                r += &self.new_line();
                r += &self.visit_value(item, ty_items)?;
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
        inner: &Value,
        ty_variants: &'t [ir::TyEnumVariant],
    ) -> Result<Self::Res, crate::Error> {
        let variant = ty_variants.get(tag).ok_or(crate::Error::InvalidData)?;

        let mut r = variant.name.to_string();

        let is_unit = variant.ty.kind.as_tuple().is_some_and(|x| x.is_empty());
        if !is_unit {
            r += "(";
            r += &self.visit_value(inner, &variant.ty)?;
            r += ")";
        }

        Ok(r)
    }
}

#[cfg(test)]
mod tests {
    use crate::Value;
    use crate::ir;

    fn path(segments: &[&str]) -> ir::Path {
        ir::Path(segments.iter().map(|segment| (*segment).into()).collect())
    }

    fn ty_ident(segments: &[&str]) -> ir::Ty {
        ir::Ty::new(path(segments))
    }

    fn ty_def(segments: &[&str], ty: ir::Ty) -> ir::TyDef {
        ir::TyDef {
            name: path(segments),
            ty,
        }
    }

    #[test]
    fn nominal_numeric_dispatch_uses_type_ident() {
        let ty = ty_ident(&["alias", "Signed"]);
        let ty_defs = vec![
            ty_def(&["alias", "Signed"], ty_ident(&["std", "Int32"])),
            ty_def(&["std", "Int32"], ir::Ty::new(ir::TyPrimitive::uint32)),
        ];

        let value = Value::Prim32((-1_i32) as u32);

        assert_eq!(value.print_source(&ty, &ty_defs).unwrap(), "-1");
    }

    #[test]
    fn decimal_alias_keeps_special_printing() {
        let ty = ty_ident(&["alias", "Money"]);
        let ty_defs = vec![
            ty_def(&["alias", "Money"], ty_ident(&["std", "Decimal"])),
            ty_def(&["std", "Decimal"], ir::Ty::new(ir::TyPrimitive::int64)),
        ];

        let value = Value::Prim64((-123_i64) as u64);

        assert_eq!(value.print_source(&ty, &ty_defs).unwrap(), "-1.23");
    }
}
