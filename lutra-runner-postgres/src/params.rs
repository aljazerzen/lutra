use std::{borrow::Cow, collections::HashMap};

use bytes::BufMut;
use lutra_bin::{Visitor, ir, rr};
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
                let format = lutra_bin::layout::enum_format(variants, &ty_mat.variants_recursive);

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
    let mut json_encoder = JsonEncoder {
        out: String::with_capacity(input.len()),

        // reuse this HashMap over the whole [to_sql] invocation
        defs: program.defs.iter().map(|d| (&d.name, &d.ty)).collect(),
    };
    json_encoder.visit(input, ty).unwrap();
    json_encoder.out.into_bytes()
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
                let (offset, len) = lutra_bin::ArrayReader::<&[u8]>::read_head(&self.data);
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

struct JsonEncoder<'t> {
    out: String,
    defs: HashMap<&'t ir::Path, &'t ir::Ty>,
}

impl<'t, 'a> lutra_bin::Visitor<'t, &'a [u8]> for JsonEncoder<'t> {
    type Res = ();

    fn get_ty(&self, name: &ir::Path) -> &'t ir::Ty {
        self.defs.get(name).unwrap()
    }

    fn visit_bool(&mut self, v: bool) -> Result<Self::Res, lutra_bin::Error> {
        self.out.push_str(if v { "true" } else { "false" });
        Ok(())
    }

    fn visit_int8(&mut self, v: i8) -> Result<Self::Res, lutra_bin::Error> {
        self.out.push_str(&v.to_string());
        Ok(())
    }

    fn visit_int16(&mut self, v: i16) -> Result<Self::Res, lutra_bin::Error> {
        self.out.push_str(&v.to_string());
        Ok(())
    }

    fn visit_int32(&mut self, v: i32) -> Result<Self::Res, lutra_bin::Error> {
        self.out.push_str(&v.to_string());
        Ok(())
    }

    fn visit_int64(&mut self, v: i64) -> Result<Self::Res, lutra_bin::Error> {
        self.out.push_str(&v.to_string());
        Ok(())
    }

    fn visit_uint8(&mut self, v: u8) -> Result<Self::Res, lutra_bin::Error> {
        self.out.push_str(&v.to_string());
        Ok(())
    }

    fn visit_uint16(&mut self, v: u16) -> Result<Self::Res, lutra_bin::Error> {
        self.out.push_str(&v.to_string());
        Ok(())
    }

    fn visit_uint32(&mut self, v: u32) -> Result<Self::Res, lutra_bin::Error> {
        self.out.push_str(&v.to_string());
        Ok(())
    }

    fn visit_uint64(&mut self, v: u64) -> Result<Self::Res, lutra_bin::Error> {
        self.out.push_str(&v.to_string());
        Ok(())
    }

    fn visit_float32(&mut self, v: f32) -> Result<Self::Res, lutra_bin::Error> {
        self.out.push_str(&v.to_string());
        Ok(())
    }

    fn visit_float64(&mut self, v: f64) -> Result<Self::Res, lutra_bin::Error> {
        self.out.push_str(&v.to_string());
        Ok(())
    }

    fn visit_text(&mut self, content: &[u8], len: usize) -> Result<Self::Res, lutra_bin::Error> {
        let s = String::from_utf8(content[0..len].to_vec())
            .map_err(|_| lutra_bin::Error::InvalidData)?;

        self.out
            .push_str(&JsonValue::String(s).stringify().unwrap());
        Ok(())
    }

    fn visit_tuple(
        &mut self,
        fields: impl Iterator<Item = (&'a [u8], &'t ir::TyTupleField)>,
    ) -> Result<Self::Res, lutra_bin::Error> {
        let fields = fields.map(|(b, f)| (b, &f.ty));
        self.encode_json_array(fields);
        Ok(())
    }

    fn visit_array(
        &mut self,
        items: impl Iterator<Item = &'a [u8]>,
        ty_items: &'t ir::Ty,
    ) -> Result<Self::Res, lutra_bin::Error> {
        let items = items.map(|v| (v, ty_items));
        self.encode_json_array(items);
        Ok(())
    }

    fn visit_enum(
        &mut self,
        tag: usize,
        inner: &'a [u8],
        ty_variants: &'t [ir::TyEnumVariant],
    ) -> Result<Self::Res, lutra_bin::Error> {
        self.out += "{\"";
        self.out += &tag.to_string();
        self.out += "\":";
        self.visit(inner, &ty_variants[tag].ty)?;
        self.out += "}";
        Ok(())
    }
}

impl<'t> JsonEncoder<'t> {
    fn encode_json_array<'a>(&mut self, items: impl Iterator<Item = (&'a [u8], &'t ir::Ty)>) {
        self.out.push('[');
        for (index, (field, field_ty)) in items.into_iter().enumerate() {
            if index > 0 {
                self.out.push(',');
            }
            self.visit(field, field_ty).unwrap();
        }
        self.out.push(']');
    }
}
