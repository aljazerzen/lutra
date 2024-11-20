use lutra_frontend::pr;

use crate::{Result, Value};

use super::fold::ValueVisitor;

impl Value {
    pub fn print_source(&self, ty: &pr::Ty) -> Result<String> {
        let mut printer = Printer::default();

        printer.visit_value(self, ty)
    }
}

#[derive(Clone, Default)]
struct Printer {
    indent: usize,
}

const INDENT: usize = 2;

impl Printer {
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

impl ValueVisitor for Printer {
    type Res = String;

    fn visit_int(&mut self, v: i64) -> Result<Self::Res, crate::Error> {
        Ok(format!("{v}"))
    }

    fn visit_float(&mut self, v: f64) -> Result<Self::Res, crate::Error> {
        Ok(format!("{v:1.1}"))
    }

    fn visit_bool(&mut self, v: bool) -> Result<Self::Res, crate::Error> {
        Ok(if v {
            "true".to_string()
        } else {
            "false".to_string()
        })
    }

    fn visit_text(&mut self, v: &str) -> Result<Self::Res, crate::Error> {
        // TODO escape strings
        Ok(format!("\"{v}\""))
    }

    fn visit_tuple(
        &mut self,
        fields: &[Value],
        ty_fields: &[pr::TyTupleField],
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
        ty_items: &pr::Ty,
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
        ty_variants: &[pr::TyEnumVariant],
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
