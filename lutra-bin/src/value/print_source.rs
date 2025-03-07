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

    fn get_mat_ty(&self, ty: &'t ir::Ty) -> &'t ir::Ty {
        if let ir::TyKind::Ident(path) = &ty.kind {
            self.ty_defs.get(path).unwrap()
        } else {
            ty
        }
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
        Ok(format!("{v}"))
    }
    fn visit_float64(&mut self, v: f64) -> Result<Self::Res, crate::Error> {
        Ok(format!("{v}"))
    }

    fn visit_text(&mut self, v: &str) -> Result<Self::Res, crate::Error> {
        // TODO escape strings
        Ok(format!("\"{v}\""))
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

        let is_unit = variant.ty.kind.as_tuple().map_or(false, |x| x.is_empty());
        if !is_unit {
            r += "(";
            self.indent();

            r += &self.new_line();
            r += &self.visit_value(inner, &variant.ty)?;

            self.deindent();
            r += &self.new_line();
            r += ")";
        }

        Ok(r)
    }
}
