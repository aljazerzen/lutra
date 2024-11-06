use std::io::Write;

use lutra_frontend::pr;

use super::{expect_ty, expect_ty_primitive, Value};
use crate::layout::{self, EnumHeadFormat, EnumVariantFormat};
use crate::{Decode, Encode, Error, Reader, Result};

impl Value {
    /// Convert a Lutra [Value] to .ld binary encoding.
    pub fn encode(&self, w: &mut Vec<u8>, ty: &pr::Ty) -> Result<()> {
        let mut ctx = Context::new(ty);

        let meta = encode_body(w, self, ty, &mut ctx)?;
        encode_head(w, self, meta, ty, &mut ctx)?;
        Ok(())
    }

    /// Convert .ld binary encoding into Lutra [Value].
    pub fn decode(buf: &[u8], ty: &pr::Ty) -> Result<Value> {
        let mut ctx = Context::new(ty);

        let head_size = ty.layout.as_ref().unwrap().head_size / 8;

        let mut reader = Reader::new(buf, buf.len() - head_size);
        decode_inner(&mut reader, ty, &mut ctx)
    }
}

enum ValueBodyMeta {
    None,
    Offset(usize),
    Tuple(Vec<ValueBodyMeta>),
}

fn encode_body<'t>(
    w: &mut Vec<u8>,
    value: &Value,
    ty: &'t pr::Ty,
    ctx: &mut Context<'t>,
) -> Result<ValueBodyMeta> {
    let ty = resolve_ident(ty, ctx);

    match value {
        Value::Int(_) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Int)?;
            Ok(ValueBodyMeta::None)
        }
        Value::Float(_) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Float)?;
            Ok(ValueBodyMeta::None)
        }
        Value::Bool(_) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Bool)?;
            Ok(ValueBodyMeta::None)
        }
        Value::Text(v) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Text)?;

            let meta = v.encode_body(w)?;
            Ok(ValueBodyMeta::Offset(meta))
        }
        Value::Tuple(fields) => {
            let ty_fields = expect_ty(ty, |k| k.as_tuple(), "tuple")?;

            let mut metas = Vec::with_capacity(fields.len());
            for (f, f_ty) in fields.iter().zip(ty_fields) {
                metas.push(encode_body(w, f, &f_ty.ty, ctx)?);
            }

            Ok(ValueBodyMeta::Tuple(metas))
        }
        Value::Array(items) => {
            let items_ty = expect_ty(ty, |k| k.as_array(), "array")?;

            let mut metas = Vec::with_capacity(items.len());
            for i in items {
                metas.push(encode_body(w, i, items_ty, ctx)?);
            }

            let items_start = w.len();
            for (i, m) in items.iter().zip(metas.into_iter()) {
                encode_head(w, i, m, items_ty, ctx)?;
            }

            Ok(ValueBodyMeta::Offset(items_start))
        }
        Value::Enum(tag, inner) => {
            let variants = expect_ty(ty, |k| k.as_enum(), "enum")?;

            let (_, variant_format, _, variant_ty) = encode_enum_params(*tag, variants)?;

            let meta = encode_body(w, inner, variant_ty, ctx)?;

            if variant_format.is_inline {
                Ok(meta)
            } else {
                let variant_start = w.len();
                encode_head(w, inner, meta, variant_ty, ctx)?;
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
    ctx: &mut Context<'t>,
) -> Result<()> {
    let ty = resolve_ident(ty, ctx);

    match value {
        Value::Int(v) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Int)?;
            v.encode_head((), w)?;
        }
        Value::Float(v) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Float)?;
            v.encode_head((), w)?;
        }
        Value::Bool(v) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Bool)?;

            v.encode_head((), w)?;
        }
        Value::Text(v) => {
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

            for ((f, m), f_ty) in fields.iter().zip(metas.into_iter()).zip(ty_fields) {
                encode_head(w, f, m, &f_ty.ty, ctx)?;
            }
        }
        Value::Array(items) => {
            expect_ty(ty, |k| k.as_array(), "array")?;

            let ValueBodyMeta::Offset(items_offset) = body_meta else {
                unreachable!()
            };

            let offset = w.len() - items_offset;
            w.write_all(&(offset as u32).to_le_bytes())?;
            w.write_all(&(items.len() as u32).to_le_bytes())?;
        }
        Value::Enum(tag, inner) => {
            let variants = expect_ty(ty, |k| k.as_enum(), "enum")?;

            let (head, variant, tag, variant_ty) = encode_enum_params(*tag, variants)?;

            let tag_bytes = &(tag as u64).to_le_bytes()[0..(head.s / 8)];

            if variant.is_inline {
                w.write_all(tag_bytes)?;
                encode_head(w, inner, body_meta, variant_ty, ctx)?;
            } else {
                let ValueBodyMeta::Offset(inner_start) = body_meta else {
                    unreachable!()
                };
                let offset = w.len() - inner_start;

                w.write_all(tag_bytes)?;
                w.write_all(&(offset as u32).to_le_bytes())?;
            }

            if variant.padding > 0 {
                w.write_all(&vec![0; variant.padding / 8])?;
            }
        }
    }

    Ok(())
}

fn encode_enum_params<'t>(
    tag: usize,
    ty_variants: &'t [(String, pr::Ty)],
) -> Result<(EnumHeadFormat, EnumVariantFormat, usize, &'t pr::Ty)> {
    let head_format = layout::enum_head_format(ty_variants);

    let (_, variant_ty) = ty_variants.get(tag).ok_or(Error::InvalidData)?;

    let variant_format = layout::enum_variant_format(variant_ty);
    Ok((head_format, variant_format, tag, variant_ty))
}

fn decode_inner<'t>(r: &mut Reader<'_>, ty: &'t pr::Ty, ctx: &mut Context<'t>) -> Result<Value> {
    let ty = resolve_ident(ty, ctx);

    Ok(match &ty.kind {
        pr::TyKind::Primitive(pr::PrimitiveSet::Bool) => Value::Bool(bool::decode(r)?),
        pr::TyKind::Primitive(pr::PrimitiveSet::Int) => Value::Int(i64::decode(r)?),
        pr::TyKind::Primitive(pr::PrimitiveSet::Float) => Value::Float(f64::decode(r)?),
        pr::TyKind::Primitive(pr::PrimitiveSet::Text) => Value::Text(String::decode(r)?),

        pr::TyKind::Tuple(fields) => {
            let mut res = Vec::with_capacity(fields.len());
            for field in fields {
                res.push(decode_inner(r, &field.ty, ctx)?);
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
                buf.push(decode_inner(&mut body, item_ty, ctx)?);
            }

            Value::Array(buf)
        }

        pr::TyKind::Enum(variants) => {
            let mut body = r.clone();

            let head = layout::enum_head_format(variants);

            let mut tag_bytes = r.copy_n(head.s / 8);
            tag_bytes.resize(8, 0);
            let tag = u64::from_le_bytes(tag_bytes.try_into().unwrap()) as usize;

            let (_, variant_ty) = variants.get(tag).unwrap();

            let variant_format = layout::enum_variant_format(variant_ty);

            let inner = if variant_format.is_inline {
                decode_inner(r, variant_ty, ctx)?
            } else {
                let offset = r.copy_const::<4>();
                let offset = u32::from_le_bytes(offset) as usize;
                body.rewind(offset);
                decode_inner(&mut body, variant_ty, ctx)?
            };

            r.skip(variant_format.padding);
            Value::Enum(tag, Box::new(inner))
        }

        _ => return Err(Error::InvalidType),
    })
}

struct Context<'t> {
    top_level_ty: &'t pr::Ty,
}

impl<'t> Context<'t> {
    fn new(top_level_ty: &'t pr::Ty) -> Self {
        Context { top_level_ty }
    }
}

fn resolve_ident<'t>(ty: &'t pr::Ty, ctx: &mut Context<'t>) -> &'t pr::Ty {
    if let pr::TyKind::Ident(ident) = &ty.kind {
        if ctx.top_level_ty.name.as_deref() == Some(ident.name()) {
            ctx.top_level_ty
        } else {
            ty
        }
    } else {
        ty
    }
}
