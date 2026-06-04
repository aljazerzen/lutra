use crate::{Context, Error};
use duckdb::{ToSql, types::Null};
use lutra_bin::{Visitor, ir};
use std::collections::HashMap;

pub fn to_sql<'a>(input: &'a [u8], ty: &'a ir::Ty, ctx: &'a Context) -> Result<Args<'a>, Error> {
    let mut args = Vec::new();
    ctx.encode_args(input, ty, &mut args);
    Ok(Args { args })
}

impl<'a> Context<'a> {
    fn encode_args(&'a self, input: &'a [u8], ty: &'a ir::Ty, args: &mut Vec<Box<dyn ToSql + 'a>>) {
        let mut ty = ty;
        while let ir::TyKind::Ident(ident) = &ty.kind {
            if let Some(param_ty) = get_param_ty_of_std_ty(ident) {
                args.push(encode_param(input, param_ty));
                return;
            }
            ty = self.get_ty(ident);
        }

        match &ty.kind {
            ir::TyKind::Primitive(p) => {
                args.push(encode_param(input, get_param_ty_of(p).unwrap()));
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

            ir::TyKind::Enum(variants) if self.is_option(variants) => {
                let format = lutra_bin::layout::enum_format(variants, &ty.variants_recursive);
                let (tag, inner) =
                    lutra_bin::decode_enum_head(input, format.tag_bytes, format.has_ptr);

                if tag == 0 {
                    self.push_nulls(&variants[1].ty, args);
                } else {
                    self.encode_args(inner, &variants[1].ty, args);
                }
            }

            ir::TyKind::Enum(variants) => {
                let format = lutra_bin::layout::enum_format(variants, &ty.variants_recursive);
                let (tag, inner) =
                    lutra_bin::decode_enum_head(input, format.tag_bytes, format.has_ptr);

                args.push(Box::new(tag as i16));

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
        let mut ty = ty;
        while let ir::TyKind::Ident(ident) = &ty.kind {
            if get_param_ty_of_std_ty(ident).is_some() {
                args.push(Box::new(Null));
                return;
            }
            ty = self.get_ty(ident);
        }

        match &ty.kind {
            ir::TyKind::Primitive(_) | ir::TyKind::Array(_) => {
                args.push(Box::new(Null));
            }

            ir::TyKind::Tuple(fields) => {
                for field in fields {
                    self.push_nulls(&field.ty, args);
                }
            }

            ir::TyKind::Enum(variants) => {
                args.push(Box::new(Null));
                for variant in variants {
                    self.push_nulls(&variant.ty, args);
                }
            }

            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => panic!("unexpected type"),
        }
    }
}

#[derive(Clone, Copy)]
enum ParamTy {
    Bool,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    F32,
    F64,
    Text,
}

fn get_param_ty_of(ty: &ir::TyPrimitive) -> Option<ParamTy> {
    match ty {
        ir::TyPrimitive::Prim8 => Some(ParamTy::I8),
        ir::TyPrimitive::Prim16 => Some(ParamTy::I16),
        ir::TyPrimitive::Prim32 => Some(ParamTy::I32),
        ir::TyPrimitive::Prim64 => Some(ParamTy::I64),
    }
}

fn get_param_ty_of_std_ty(ident: &ir::Path) -> Option<ParamTy> {
    if ident.is(&["std", "Bool"]) {
        Some(ParamTy::Bool)
    } else if ident.is(&["std", "Int8"]) {
        Some(ParamTy::I8)
    } else if ident.is(&["std", "Uint8"]) {
        Some(ParamTy::U8)
    } else if ident.is(&["std", "Int16"]) {
        Some(ParamTy::I16)
    } else if ident.is(&["std", "Uint16"]) {
        Some(ParamTy::U16)
    } else if ident.is(&["std", "Int32"]) || ident.is(&["std", "Date"]) {
        Some(ParamTy::I32)
    } else if ident.is(&["std", "Uint32"]) {
        Some(ParamTy::U32)
    } else if ident.is(&["std", "Int64"])
        || ident.is(&["std", "Time"])
        || ident.is(&["std", "Timestamp"])
        || ident.is(&["std", "Decimal"])
    {
        Some(ParamTy::I64)
    } else if ident.is(&["std", "Uint64"]) {
        Some(ParamTy::U64)
    } else if ident.is(&["std", "Float32"]) {
        Some(ParamTy::F32)
    } else if ident.is(&["std", "Float64"]) {
        Some(ParamTy::F64)
    } else if ident.is(&["std", "Text"]) {
        Some(ParamTy::Text)
    } else {
        None
    }
}

fn encode_param<'a>(data: &'a [u8], ty: ParamTy) -> Box<dyn ToSql + 'a> {
    match ty {
        ParamTy::Bool => Box::new(data[0] != 0),
        ParamTy::I8 => Box::new(i8::from_le_bytes([data[0]])),
        ParamTy::U8 => Box::new(u8::from_le_bytes([data[0]])),
        ParamTy::I16 => Box::new(i16::from_le_bytes([data[0], data[1]])),
        ParamTy::U16 => Box::new(u16::from_le_bytes([data[0], data[1]])),
        ParamTy::I32 => Box::new(i32::from_le_bytes([data[0], data[1], data[2], data[3]])),
        ParamTy::U32 => Box::new(u32::from_le_bytes([data[0], data[1], data[2], data[3]])),
        ParamTy::I64 => Box::new(i64::from_le_bytes([
            data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
        ])),
        ParamTy::U64 => Box::new(u64::from_le_bytes([
            data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
        ])),
        ParamTy::F32 => Box::new(f32::from_le_bytes([data[0], data[1], data[2], data[3]])),
        ParamTy::F64 => Box::new(f64::from_le_bytes([
            data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
        ])),
        ParamTy::Text => {
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
    pub fn as_params(&self) -> impl duckdb::Params + '_ {
        duckdb::params_from_iter(self.args.iter().map(|arg| arg.as_ref() as &dyn ToSql))
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
