use lutra_parser::parser::pr;
use std::io::Write;

use crate::layout::{self, EnumHeadFormat, EnumVariantFormat};
use crate::{Decode, Encode, Error, Reader, Result};

#[derive(Debug)]
pub enum Value<'ty> {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Tuple(Vec<(Option<&'ty str>, Value<'ty>)>),
    Array(Vec<Value<'ty>>),
    Enum(&'ty str, Box<Value<'ty>>),
}

impl<'t> Value<'t> {
    /// Convert a Lutra [Value] to .ld binary encoding.
    pub fn encode(&self, w: &mut Vec<u8>, ty: &'t pr::Ty) -> Result<()> {
        let meta = encode_body(w, self, ty)?;
        encode_head(w, self, meta, ty)?;
        Ok(())
    }
}

enum ValueBodyMeta {
    None,
    Offset(usize),
    Tuple(Vec<ValueBodyMeta>),
}

fn encode_body<'t>(w: &mut Vec<u8>, value: &Value, ty: &'t pr::Ty) -> Result<ValueBodyMeta> {
    match value {
        Value::Integer(_) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Int)?;
            Ok(ValueBodyMeta::None)
        }
        Value::Float(_) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Float)?;
            Ok(ValueBodyMeta::None)
        }
        Value::Boolean(_) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Bool)?;
            Ok(ValueBodyMeta::None)
        }
        Value::String(v) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Text)?;

            let meta = v.encode_body(w)?;
            Ok(ValueBodyMeta::Offset(meta))
        }
        Value::Tuple(fields) => {
            let ty_fields = expect_ty(ty, |k| k.as_tuple(), "tuple")?;

            let mut metas = Vec::with_capacity(fields.len());
            for ((_, f), f_ty) in fields.iter().zip(ty_fields) {
                metas.push(encode_body(w, f, &f_ty.ty)?);
            }

            Ok(ValueBodyMeta::Tuple(metas))
        }
        Value::Array(items) => {
            let items_ty = expect_ty(ty, |k| k.as_array(), "array")?;

            let mut metas = Vec::with_capacity(items.len());
            for i in items {
                metas.push(encode_body(w, i, &items_ty)?);
            }

            let items_start = w.len();
            for (i, m) in items.iter().zip(metas.into_iter()) {
                encode_head(w, i, m, &items_ty)?;
            }

            Ok(ValueBodyMeta::Offset(items_start))
        }
        Value::Enum(variant, inner) => {
            let variants = expect_ty(ty, |k| k.as_enum(), "enum")?;

            let (_, variant_format, _, variant_ty) = encode_enum_params(variant, variants)?;

            let meta = encode_body(w, &inner, variant_ty)?;

            if variant_format.is_inline {
                Ok(meta)
            } else {
                let variant_start = w.len();
                encode_head(w, &inner, meta, variant_ty)?;
                Ok(ValueBodyMeta::Offset(variant_start))
            }
        }
    }
}

fn encode_head<'t>(
    w: &mut Vec<u8>,
    value: &Value,
    body_meta: ValueBodyMeta,
    ty: &'t pr::Ty,
) -> Result<()> {
    match value {
        Value::Integer(v) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Int)?;
            v.encode_head((), w)?;
        }
        Value::Float(v) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Float)?;
            v.encode_head((), w)?;
        }
        Value::Boolean(v) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Bool)?;

            v.encode_head((), w)?;
        }
        Value::String(v) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Text)?;

            let ValueBodyMeta::Offset(bytes_offset) = body_meta else {
                unreachable!()
            };

            v.encode_head(bytes_offset, w)?;
        }
        Value::Tuple(fields) => {
            let ty_fields = expect_ty(ty, |k| k.as_tuple(), "tuple")?;

            let ValueBodyMeta::Tuple(metas) = body_meta else {
                unreachable!()
            };

            for (((_, f), m), f_ty) in fields.iter().zip(metas.into_iter()).zip(ty_fields) {
                encode_head(w, f, m, &f_ty.ty)?;
            }
        }
        Value::Array(items) => {
            expect_ty(ty, |k| k.as_array(), "array")?;

            let ValueBodyMeta::Offset(items_offset) = body_meta else {
                unreachable!()
            };

            let offset = w.len() - items_offset;
            w.write(&(offset as u32).to_le_bytes())?;
            w.write(&(items.len() as u32).to_le_bytes())?;
        }
        Value::Enum(variant, inner) => {
            let variants = expect_ty(ty, |k| k.as_enum(), "enum")?;

            let (head, variant, tag, variant_ty) = encode_enum_params(variant, variants)?;

            let tag_bytes = &(tag as u64).to_le_bytes()[0..(head.s / 8)];

            if variant.is_inline {
                w.write(tag_bytes)?;
                encode_head(w, &inner, body_meta, &variant_ty)?;
            } else {
                let ValueBodyMeta::Offset(inner_start) = body_meta else {
                    unreachable!()
                };
                let offset = w.len() - inner_start;

                w.write(tag_bytes)?;
                w.write(&(offset as u32).to_le_bytes())?;
            }

            if variant.padding > 0 {
                let mut padding = Vec::new();
                padding.resize(variant.padding / 8, 0);
                w.write(&padding)?;
            }
        }
    }

    Ok(())
}

fn encode_enum_params<'a>(
    variant: &'a str,
    ty_variants: &'a Vec<(String, pr::Ty)>,
) -> Result<(EnumHeadFormat, EnumVariantFormat, usize, &'a pr::Ty)> {
    let head_format = layout::enum_head_format(ty_variants)?;

    let tag = ty_variants.iter().position(|v| &v.0 == variant).ok_or(Error::InvalidData)?;
    let (_, variant_ty) = ty_variants.get(tag).ok_or(Error::InvalidData)?;

    let variant_format = layout::enum_variant_format(&head_format, variant_ty)?;
    Ok((head_format, variant_format, tag, variant_ty))
}

impl<'t> Value<'t> {
    /// Convert .ld binary encoding into Lutra [Value].
    pub fn decode<'b>(buf: &'b [u8], ty: &'t pr::Ty) -> Result<Value<'t>> {
        let head_size = layout::get_head_size(ty)? / 8;

        let mut reader = Reader::new(buf, buf.len() - head_size);
        decode_inner(&mut reader, ty)
    }
}

fn decode_inner<'b, 't>(r: &mut Reader<'b>, ty: &'t pr::Ty) -> Result<Value<'t>> {
    Ok(match &ty.kind {
        pr::TyKind::Primitive(pr::PrimitiveSet::Bool) => Value::Boolean(bool::decode(r)?),
        pr::TyKind::Primitive(pr::PrimitiveSet::Int) => Value::Integer(i64::decode(r)?),
        pr::TyKind::Primitive(pr::PrimitiveSet::Float) => Value::Float(f64::decode(r)?),
        pr::TyKind::Primitive(pr::PrimitiveSet::Text) => Value::String(String::decode(r)?),

        pr::TyKind::Tuple(fields) => {
            let mut res = Vec::with_capacity(fields.len());
            for field in fields {
                let name = field.name.as_deref();
                let ty = &field.ty;

                res.push((name, decode_inner(r, ty)?));
            }
            Value::Tuple(res)
        }
        pr::TyKind::Array(item_ty) => {
            let mut body = r.clone();

            let offset = r.copy_const::<4>();
            let offset = u32::from_le_bytes(offset) as usize;
            body.rewind(offset);

            let len = r.copy_const::<4>();
            let len = u32::from_le_bytes(len) as usize;

            let mut buf = Vec::with_capacity(len);
            for _ in 0..len {
                buf.push(decode_inner(&mut body, &item_ty)?);
            }

            Value::Array(buf)
        }

        pr::TyKind::Enum(variants) => {
            let mut body = r.clone();

            let head = layout::enum_head_format(variants)?;

            let mut tag_bytes = r.copy_n(head.s / 8);
            tag_bytes.resize(8, 0);
            let tag = u64::from_le_bytes(tag_bytes.try_into().unwrap()) as usize;

            let (variant_name, variant_ty) = variants.get(tag).unwrap();

            let variant_format = layout::enum_variant_format(&head, variant_ty)?;

            let inner = if variant_format.is_inline {
                decode_inner(r, variant_ty)?
            } else {
                let offset = r.copy_const::<4>();
                let offset = u32::from_le_bytes(offset) as usize;
                body.rewind(offset);
                decode_inner(&mut body, variant_ty)?
            };

            r.skip(variant_format.padding);
            Value::Enum(&variant_name, Box::new(inner))
        }

        _ => return Err(Error::InvalidType),
    })
}

fn expect_ty<'t, F, K>(ty: &'t pr::Ty, cast: F, expected: &'static str) -> Result<&'t K>
where
    F: Fn(&pr::TyKind) -> Option<&K>,
{
    cast(&ty.kind).ok_or_else(|| Error::TypeMismatch {
        expected,
        found: ty.kind.as_ref().to_string(),
    })
}

fn expect_ty_primitive<'t>(ty: &'t pr::Ty, expected: pr::PrimitiveSet) -> Result<()> {
    let found = ty.kind.as_primitive().ok_or_else(|| Error::TypeMismatch {
        expected: primitive_set_name(&expected),
        found: ty.kind.as_ref().to_string(),
    })?;

    if *found != expected {
        return Err(Error::TypeMismatch {
            expected: primitive_set_name(&expected),
            found: primitive_set_name(found).to_string(),
        });
    }
    Ok(())
}

fn primitive_set_name(expected: &pr::PrimitiveSet) -> &'static str {
    match expected {
        pr::PrimitiveSet::Int => "int",
        pr::PrimitiveSet::Float => "float",
        pr::PrimitiveSet::Bool => "bool",
        pr::PrimitiveSet::Text => "text",
        pr::PrimitiveSet::Date => "date",
        pr::PrimitiveSet::Time => "time",
        pr::PrimitiveSet::Timestamp => "timestamp",
    }
}
