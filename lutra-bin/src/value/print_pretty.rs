#![cfg(feature = "std")]

use std::collections::HashMap;

use crate::ir;
use crate::{Result, Value};

use super::fold::ValueVisitor;

impl Value {
    pub fn print_pretty(&self, ty: &ir::Ty, ty_defs: &[ir::TyDef]) -> Result<String> {
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
                r += row;
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

fn try_tabular(items_ty: &ir::Ty) -> Option<tabular::Table> {
    match &items_ty.kind {
        ir::TyKind::Primitive(primitive) => {
            let t = tabular::Table::new(column_spec_primitive(primitive));

            Some(t)
        }
        ir::TyKind::Tuple(_) => {
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
        ir::TyKind::Array(_) => None,
        ir::TyKind::Enum(_) => None,
        _ => todo!(),
    }
}

fn column_spec_primitive(primitive: &ir::TyPrimitive) -> &'static str {
    match primitive {
        ir::TyPrimitive::bool => "{:^}",
        ir::TyPrimitive::int8
        | ir::TyPrimitive::int16
        | ir::TyPrimitive::int32
        | ir::TyPrimitive::int64
        | ir::TyPrimitive::uint8
        | ir::TyPrimitive::uint16
        | ir::TyPrimitive::uint32
        | ir::TyPrimitive::uint64
        | ir::TyPrimitive::float32
        | ir::TyPrimitive::float64 => "{:>}",
        ir::TyPrimitive::text => "{:<}",
    }
}

fn column_spec_ty(ty: &ir::Ty) -> Option<String> {
    match &ty.kind {
        ir::TyKind::Primitive(p) => Some(column_spec_primitive(p).to_string()),
        ir::TyKind::Tuple(fields) => {
            let specs = fields
                .iter()
                .map(|f| column_spec_ty(&f.ty))
                .collect::<Option<Vec<String>>>()?;

            Some(specs.join(" "))
        }
        ir::TyKind::Array(_) => None,
        ir::TyKind::Enum(_) => None,
        _ => todo!(),
    }
}

fn apply_header(row: &mut tabular::Row, prefix: &mut Vec<String>, ty: &ir::Ty) {
    match &ty.kind {
        ir::TyKind::Primitive(_) => {
            row.add_cell(prefix.join("."));
        }
        ir::TyKind::Tuple(fields) => {
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

        ir::TyKind::Array(_) => unreachable!(),
        ir::TyKind::Enum(_) => unreachable!(),
        _ => todo!(),
    }
}

fn apply_tabular_value(row: &mut tabular::Row, value: &Value) {
    match value {
        Value::Bool(v) => {
            row.add_cell(if *v { "x" } else { " " });
        }
        Value::Int8(v) => {
            row.add_cell(v.to_string());
        }
        Value::Int16(v) => {
            row.add_cell(v.to_string());
        }
        Value::Int32(v) => {
            row.add_cell(v.to_string());
        }
        Value::Int64(v) => {
            row.add_cell(v.to_string());
        }
        Value::Uint8(v) => {
            row.add_cell(v.to_string());
        }
        Value::Uint16(v) => {
            row.add_cell(v.to_string());
        }
        Value::Uint32(v) => {
            row.add_cell(v.to_string());
        }
        Value::Uint64(v) => {
            row.add_cell(v.to_string());
        }
        Value::Float32(v) => {
            row.add_cell(v.to_string());
        }
        Value::Float64(v) => {
            row.add_cell(v.to_string());
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
