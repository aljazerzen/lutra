use std::borrow::Cow;

use bytes::BufMut;
use lutra_bin::{ir, rr};
use postgres_types as pg_ty;
use tinyjson::JsonValue;

use crate::Context;

pub fn to_sql<'d>(program: &rr::SqlProgram, input: &'d [u8], ctx: &Context) -> Args<'d> {
    let arg_tys = ctx.fields_of_ty(&program.input_ty);

    let mut args = Vec::new();
    let mut offset = 0;
    for arg_ty in arg_tys {
        match &ctx.get_ty_mat(arg_ty).kind {
            ir::TyKind::Primitive(_) => {
                args.push(Arg {
                    data: Cow::Borrowed(&input[offset..]),
                });
            }

            ir::TyKind::Array(_) | ir::TyKind::Enum(_) => {
                let value =
                    lutra_bin::Value::decode(&input[offset..], arg_ty, &program.defs).unwrap();
                let mut out = String::new();
                lutra_to_json(value, &mut out);
                args.push(Arg {
                    data: Cow::Owned(out.into_bytes()),
                });
            }
            ir::TyKind::Tuple(_) | ir::TyKind::Function(_) | ir::TyKind::Ident(_) => todo!(),
        }

        offset += arg_ty.layout.as_ref().unwrap().head_size.div_ceil(8) as usize;
    }

    Args { args }
}

impl<'a> Context<'a> {
    fn fields_of_ty(&'a self, ty: &'a ir::Ty) -> Box<dyn Iterator<Item = &'a ir::Ty> + 'a> {
        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Tuple(fields) => {
                Box::new(fields.iter().flat_map(|f| self.fields_of_ty(&f.ty)))
            }
            _ => Box::new(Some(ty).into_iter()),
        }
    }
}
pub struct Args<'a> {
    args: Vec<Arg<'a>>,
}

impl<'a> Args<'a> {
    pub fn as_refs(&self) -> Vec<&(dyn pg_ty::ToSql + Sync)> {
        self.args
            .iter()
            .map(|x| x as &(dyn pg_ty::ToSql + Sync))
            .collect()
    }
}

#[derive(Debug)]
struct Arg<'a> {
    data: Cow<'a, [u8]>,
}

impl<'a> pg_ty::ToSql for Arg<'a> {
    fn to_sql(
        &self,
        ty: &pg_ty::Type,
        out: &mut bytes::BytesMut,
    ) -> Result<pg_ty::IsNull, Box<dyn std::error::Error + Sync + Send>>
    where
        Self: Sized,
    {
        match ty.name() {
            "bool" => out.put_slice(&self.data[0..1]),
            "int1" => todo!(),
            "int2" => out.extend(self.data[0..2].iter().rev()),
            "int4" => out.extend(self.data[0..4].iter().rev()),
            "int8" => out.extend(self.data[0..8].iter().rev()),
            "uint8" => todo!(),
            "uint16" => todo!(),
            "uint32" => todo!(),
            "uint64" => todo!(),
            "float32" => out.put_slice(&self.data[0..4]),
            "float64" => out.put_slice(&self.data[0..8]),
            "text" => {
                let (offset, len) = lutra_bin::ArrayReader::read_head(&self.data);
                out.put_slice(&self.data[offset..(offset + len)])
            }
            "json" => out.put_slice(&self.data),
            "jsonb" => {
                out.put_u8(1); // version 1
                out.put_slice(&self.data)
            }
            _ => todo!(),
        }
        Ok(pg_ty::IsNull::No)
    }

    fn accepts(_ty: &pg_ty::Type) -> bool {
        true
    }

    pg_ty::to_sql_checked!();
}

// TODO: this is very inefficient.
// We should instead instantiate a transcoder, which would read lb and directly translate to JSON
fn lutra_to_json(value: lutra_bin::Value, out: &mut String) {
    match value {
        lutra_bin::Value::Bool(v) => out.push_str(if v { "true" } else { "false" }),
        lutra_bin::Value::Int8(v) => out.push_str(&v.to_string()),
        lutra_bin::Value::Int16(v) => out.push_str(&v.to_string()),
        lutra_bin::Value::Int32(v) => out.push_str(&v.to_string()),
        lutra_bin::Value::Int64(v) => out.push_str(&v.to_string()),
        lutra_bin::Value::Uint8(v) => out.push_str(&v.to_string()),
        lutra_bin::Value::Uint16(v) => out.push_str(&v.to_string()),
        lutra_bin::Value::Uint32(v) => out.push_str(&v.to_string()),
        lutra_bin::Value::Uint64(v) => out.push_str(&v.to_string()),
        lutra_bin::Value::Float32(v) => out.push_str(&v.to_string()),
        lutra_bin::Value::Float64(v) => out.push_str(&v.to_string()),
        lutra_bin::Value::Text(v) => out.push_str(&JsonValue::String(v).stringify().unwrap()),
        lutra_bin::Value::Tuple(items) | lutra_bin::Value::Array(items) => {
            out.push('[');
            for (index, field) in items.into_iter().enumerate() {
                if index > 0 {
                    out.push(',');
                }
                lutra_to_json(field, out);
            }
            out.push(']');
        }
        lutra_bin::Value::Enum(_tag, _value) => todo!(),
    }
}
