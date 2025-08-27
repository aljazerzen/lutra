use std::borrow::Cow;

use bytes::BufMut;
use lutra_bin::{ir, rr};
use postgres_types as pg_ty;
use tinyjson::JsonValue;

use crate::Context;

pub fn to_sql<'a>(program: &'a rr::SqlProgram, input: &'a [u8], ctx: &'a Context) -> Args<'a> {
    let mut args = Vec::new();
    ctx.encode_args(input, &program.input_ty, &mut args, program);

    Args { args }
}

impl<'a> Context<'a> {
    fn encode_args(
        &'a self,
        input: &'a [u8],
        ty: &'a ir::Ty,
        args: &mut Vec<Arg<'a>>,
        program: &rr::SqlProgram,
    ) {
        let ty_mat = self.get_ty_mat(ty);
        match &ty_mat.kind {
            ir::TyKind::Primitive(_) => {
                args.push(Arg {
                    data: Cow::Borrowed(input),
                    ty: Cow::Borrowed(&ty_mat.kind),
                });
            }

            // serialize to JSON
            ir::TyKind::Array(_) => {
                args.push(Arg {
                    data: Cow::Owned(serialize_input_to_json(input, ty, program)),
                    ty: Cow::Borrowed(&ty_mat.kind),
                });
            }

            ir::TyKind::Tuple(fields) => {
                let mut offset = 0;
                for field in fields {
                    self.encode_args(&input[offset..], &field.ty, args, program);
                    offset += field.ty.layout.as_ref().unwrap().head_size.div_ceil(8) as usize;
                }
            }
            ir::TyKind::Enum(variants) => {
                let format = lutra_bin::layout::enum_format(variants);

                let (tag, inner) =
                    lutra_bin::decode_enum_head(input, format.tag_bytes, format.has_ptr);
                args.push(Arg {
                    data: Cow::Owned((tag as u16).to_le_bytes().to_vec()),
                    ty: Cow::Owned(ir::TyKind::Primitive(ir::TyPrimitive::int16)),
                }); // tag
                for (position, variant) in variants.iter().enumerate() {
                    if position == tag as usize {
                        let variant = variants.get(tag as usize).unwrap();
                        self.encode_args(inner, &variant.ty, args, program);
                    } else {
                        self.push_nulls(&variant.ty, args);
                    }
                }
            }

            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => panic!(),
        }
    }

    fn push_nulls(&'a self, ty: &'a ir::Ty, args: &mut Vec<Arg<'a>>) {
        let ty_mat = self.get_ty_mat(ty);
        match &ty_mat.kind {
            ir::TyKind::Primitive(_) | ir::TyKind::Array(_) => {
                args.push(Arg {
                    data: Cow::Owned(vec![]),
                    ty: Cow::Borrowed(&ty_mat.kind),
                });
            }

            ir::TyKind::Tuple(fields) => {
                for field in fields {
                    self.push_nulls(&field.ty, args);
                }
            }
            ir::TyKind::Enum(variants) => {
                args.push(Arg {
                    data: Cow::Owned(vec![]),
                    ty: Cow::Owned(ir::TyKind::Primitive(ir::TyPrimitive::int16)),
                });
                for variant in variants {
                    self.push_nulls(&variant.ty, args);
                }
            }

            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => panic!(),
        }
    }
}

fn serialize_input_to_json(input: &[u8], ty: &ir::Ty, program: &rr::SqlProgram) -> Vec<u8> {
    let value = lutra_bin::Value::decode(input, ty, &program.defs).unwrap();
    let mut out = String::new();
    lutra_to_json(value, &mut out);
    out.into_bytes()
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
    ty: Cow<'a, ir::TyKind>,
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
        if self.data.is_empty() {
            return Ok(pg_ty::IsNull::Yes);
        }

        match self.ty.as_ref() {
            ir::TyKind::Primitive(ir::TyPrimitive::bool) => out.put_slice(&self.data[0..1]),
            ir::TyKind::Primitive(ir::TyPrimitive::int8)
            | ir::TyKind::Primitive(ir::TyPrimitive::uint8) => out.put_slice(&self.data[0..1]),

            ir::TyKind::Primitive(ir::TyPrimitive::int16)
            | ir::TyKind::Primitive(ir::TyPrimitive::uint16) => {
                out.extend(self.data[0..2].iter().rev())
            }
            ir::TyKind::Primitive(ir::TyPrimitive::int32)
            | ir::TyKind::Primitive(ir::TyPrimitive::uint32) => {
                out.extend(self.data[0..4].iter().rev())
            }
            ir::TyKind::Primitive(ir::TyPrimitive::int64)
            | ir::TyKind::Primitive(ir::TyPrimitive::uint64) => {
                out.extend(self.data[0..8].iter().rev())
            }

            ir::TyKind::Primitive(ir::TyPrimitive::float32) => out.put_slice(&self.data[0..4]),
            ir::TyKind::Primitive(ir::TyPrimitive::float64) => out.put_slice(&self.data[0..8]),

            ir::TyKind::Primitive(ir::TyPrimitive::text) => {
                let (offset, len) = lutra_bin::ArrayReader::read_head(&self.data);
                out.put_slice(&self.data[offset..(offset + len)])
            }

            ir::TyKind::Array(_) => {
                // JSON serialization
                match ty.name() {
                    "json" => out.put_slice(&self.data),
                    "jsonb" => {
                        out.put_u8(1); // version 1
                        out.put_slice(&self.data)
                    }
                    _ => panic!(),
                }
            }

            ir::TyKind::Tuple(_)
            | ir::TyKind::Enum(_)
            | ir::TyKind::Ident(_)
            | ir::TyKind::Function(_) => unreachable!(),
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
        lutra_bin::Value::Array(items) => {
            lutra_to_json_array(items.into_iter(), out);
        }
        lutra_bin::Value::Tuple(fields) => {
            lutra_to_json_array(fields.into_iter(), out);
        }
        lutra_bin::Value::Enum(tag, inner) => {
            *out += "{\"";
            *out += &tag.to_string();
            *out += "\":";
            lutra_to_json(*inner, out);
            *out += "}";
        }
    }
}

fn lutra_to_json_array(items: impl Iterator<Item = lutra_bin::Value>, out: &mut String) {
    out.push('[');
    for (index, field) in items.into_iter().enumerate() {
        if index > 0 {
            out.push(',');
        }
        lutra_to_json(field, out);
    }
    out.push(']');
}
