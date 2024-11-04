use lutra_parser::pr;

use crate::{Result, Value};

use super::fold::ValueVisitor;

impl Value {
    pub fn print_pretty(&self, ty: &pr::Ty) -> Result<String> {
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
        if let Some(mut table) = try_tabular(ty_items) {
            for item in items {
                let mut row = tabular::Row::new();
                apply_tabular_value(&mut row, item);
                table.add_row(row);
            }

            // TODO: this is slow
            let table = table.to_string();
            let mut r = String::new();
            for row in table.split('\n') {
                r += &self.new_line();
                r += &row;
            }
            return Ok(r);
        }

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
        ty_variants: &[(String, pr::Ty)],
    ) -> Result<Self::Res, crate::Error> {
        let (variant_name, variant_ty) = ty_variants.get(tag).ok_or(crate::Error::InvalidData)?;

        let mut r = variant_name.to_string();

        let is_unit = variant_ty.kind.as_tuple().map_or(false, |x| x.is_empty());
        if !is_unit {
            r += "(";
            self.indent();

            r += &self.new_line();
            r += &self.visit_value(inner, variant_ty)?;

            self.deindent();
            r += &self.new_line();
            r += ")";
        }

        Ok(r)
    }
}

fn try_tabular(items_ty: &pr::Ty) -> Option<tabular::Table> {
    match &items_ty.kind {
        pr::TyKind::Primitive(primitive) => {
            let t = tabular::Table::new(column_spec_primitive(&primitive));

            Some(t)
        }
        pr::TyKind::Tuple(_) => {
            let column_spec = column_spec_ty(items_ty)?;

            let mut table = tabular::Table::new(&column_spec);

            let mut row = tabular::Row::new();
            let mut path = Vec::new();
            apply_header(&mut row, &mut path, items_ty);
            table.add_row(row);

            let mut divider = tabular::Row::new();
            for _ in 0..table.column_count() {
                divider.add_cell("---");
            }
            table.add_row(divider);

            Some(table)
        }
        pr::TyKind::Array(_) => None,
        pr::TyKind::Enum(_) => None,
        _ => todo!(),
    }
}

fn column_spec_primitive(primitive: &pr::PrimitiveSet) -> &'static str {
    match primitive {
        pr::PrimitiveSet::Int | pr::PrimitiveSet::Float => "{:>}",
        pr::PrimitiveSet::Bool => "{:^}",
        pr::PrimitiveSet::Text => "{:<}",
        _ => todo!(),
    }
}

fn column_spec_ty(ty: &pr::Ty) -> Option<String> {
    match &ty.kind {
        pr::TyKind::Primitive(p) => Some(column_spec_primitive(p).to_string()),
        pr::TyKind::Tuple(fields) => {
            let specs = fields
                .iter()
                .map(|f| column_spec_ty(&f.ty))
                .collect::<Option<Vec<String>>>()?;

            Some(specs.join(" "))
        }
        pr::TyKind::Array(_) => None,
        pr::TyKind::Enum(_) => None,
        _ => todo!(),
    }
}

fn apply_header(row: &mut tabular::Row, prefix: &mut Vec<String>, ty: &pr::Ty) {
    match &ty.kind {
        pr::TyKind::Primitive(_) => {
            row.add_cell(prefix.join("."));
        }
        pr::TyKind::Tuple(fields) => {
            for (index, field) in fields.iter().enumerate() {
                let name = field
                    .name
                    .clone()
                    .unwrap_or_else(|| format!("<field {index}>"));
                prefix.push(name);

                apply_header(row, prefix, &field.ty);

                prefix.pop();
            }
        }

        pr::TyKind::Array(_) => unreachable!(),
        pr::TyKind::Enum(_) => unreachable!(),
        _ => todo!(),
    }
}

fn apply_tabular_value(row: &mut tabular::Row, value: &Value) {
    match value {
        Value::Int(v) => {
            row.add_cell(v.to_string());
        }
        Value::Float(v) => {
            row.add_cell(v.to_string());
        }
        Value::Bool(v) => {
            row.add_cell(if *v { "x" } else { " " });
        }
        Value::Text(v) => {
            row.add_cell(v.to_string());
        }
        Value::Tuple(fields) => {
            for f in fields {
                apply_tabular_value(row, f);
            }
        }
        Value::Array(_) => unreachable!(),
        Value::Enum(_, _) => unreachable!(),
    }
}
