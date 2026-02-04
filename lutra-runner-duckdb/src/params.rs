use crate::{Context, Error};
use async_duckdb::duckdb::ToSql;
use lutra_bin::{Visitor, ir};
use std::collections::HashMap;

pub fn to_sql<'a>(input: &'a [u8], ty: &'a ir::Ty, ctx: &'a Context) -> Result<Args<'a>, Error> {
    let mut args = Vec::new();
    ctx.encode_args(input, ty, &mut args);
    Ok(Args { args })
}

impl<'a> Context<'a> {
    fn encode_args(&'a self, input: &'a [u8], ty: &'a ir::Ty, args: &mut Vec<Box<dyn ToSql + 'a>>) {
        let ty_mat = self.get_ty_mat(ty);
        match &ty_mat.kind {
            ir::TyKind::Primitive(prim) => {
                args.push(encode_primitive(input, *prim));
            }

            // serialize to JSON string for arrays
            ir::TyKind::Array(_) => {
                let json_str = serialize_to_json(input, ty, &self.types);
                args.push(Box::new(json_str));
            }

            ir::TyKind::Tuple(fields) => {
                let mut offset = 0;
                for field in fields {
                    self.encode_args(&input[offset..], &field.ty, args);
                    offset += field.ty.layout.as_ref().unwrap().head_size.div_ceil(8) as usize;
                }
            }

            ir::TyKind::Enum(variants) => {
                let format = lutra_bin::layout::enum_format(variants, &ty_mat.variants_recursive);
                let (tag, inner) =
                    lutra_bin::decode_enum_head(input, format.tag_bytes, format.has_ptr);

                // Tag as int16
                args.push(Box::new(tag as i16));

                // Encode variant payloads (null for non-selected variants)
                for (position, variant) in variants.iter().enumerate() {
                    if position == tag as usize {
                        self.encode_args(inner, &variant.ty, args);
                    } else {
                        self.push_nulls(&variant.ty, args);
                    }
                }
            }

            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => panic!("unexpected type"),
        }
    }

    fn push_nulls(&'a self, ty: &'a ir::Ty, args: &mut Vec<Box<dyn ToSql + 'a>>) {
        let ty_mat = self.get_ty_mat(ty);
        match &ty_mat.kind {
            ir::TyKind::Primitive(_) | ir::TyKind::Array(_) => {
                args.push(Box::new(None::<i32>)); // Use Option<i32> as a generic null
            }

            ir::TyKind::Tuple(fields) => {
                for field in fields {
                    self.push_nulls(&field.ty, args);
                }
            }

            ir::TyKind::Enum(variants) => {
                args.push(Box::new(None::<i16>)); // null tag
                for variant in variants {
                    self.push_nulls(&variant.ty, args);
                }
            }

            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => panic!("unexpected type"),
        }
    }
}

fn encode_primitive<'a>(data: &'a [u8], prim: ir::TyPrimitive) -> Box<dyn ToSql + 'a> {
    match prim {
        ir::TyPrimitive::bool => Box::new(data[0] != 0),
        ir::TyPrimitive::int8 => Box::new(i8::from_le_bytes([data[0]])),
        ir::TyPrimitive::uint8 => Box::new(u8::from_le_bytes([data[0]])),
        ir::TyPrimitive::int16 => Box::new(i16::from_le_bytes([data[0], data[1]])),
        ir::TyPrimitive::uint16 => Box::new(u16::from_le_bytes([data[0], data[1]])),
        ir::TyPrimitive::int32 => {
            Box::new(i32::from_le_bytes([data[0], data[1], data[2], data[3]]))
        }
        ir::TyPrimitive::uint32 => {
            Box::new(u32::from_le_bytes([data[0], data[1], data[2], data[3]]))
        }
        ir::TyPrimitive::int64 => Box::new(i64::from_le_bytes([
            data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
        ])),
        ir::TyPrimitive::uint64 => Box::new(u64::from_le_bytes([
            data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
        ])),
        ir::TyPrimitive::float32 => {
            Box::new(f32::from_le_bytes([data[0], data[1], data[2], data[3]]))
        }
        ir::TyPrimitive::float64 => Box::new(f64::from_le_bytes([
            data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
        ])),
        ir::TyPrimitive::text => {
            let (offset, len) = lutra_bin::ArrayReader::<&[u8]>::read_head(data);
            let text_bytes = &data[offset..(offset + len)];
            let text = String::from_utf8_lossy(text_bytes).into_owned();
            Box::new(text)
        }
    }
}

fn serialize_to_json(input: &[u8], ty: &ir::Ty, defs: &HashMap<&ir::Path, &ir::Ty>) -> String {
    let mut encoder = JsonEncoder {
        out: String::with_capacity(input.len()),
        defs: defs.clone(),
    };
    encoder.visit(input, ty).unwrap();
    encoder.out
}

pub struct Args<'a> {
    args: Vec<Box<dyn ToSql + 'a>>,
}

impl<'a> Args<'a> {
    pub fn as_params(&self) -> impl async_duckdb::duckdb::Params + '_ {
        async_duckdb::duckdb::params_from_iter(
            self.args.iter().map(|arg| arg.as_ref() as &dyn ToSql),
        )
    }
}

struct JsonEncoder<'t> {
    out: String,
    defs: HashMap<&'t ir::Path, &'t ir::Ty>,
}

impl<'t, 'a> Visitor<'t, &'a [u8]> for JsonEncoder<'t> {
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

        // Escape JSON string
        use tinyjson::JsonValue;
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
