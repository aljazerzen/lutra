use std::io::Write;

use lutra_frontend::pr;

use super::{expect_ty, expect_ty_primitive, Value};
use crate::encode::ReversePointer;
use crate::layout::{self, EnumHeadFormat, EnumVariantFormat};
use crate::{ArrayReader, Decode, Encode, Error, Reader, Result};

impl Value {
    /// Convert a Lutra [Value] to .ld binary encoding.
    pub fn encode(&self, ty: &pr::Ty) -> Result<Vec<u8>> {
        let mut res = Vec::new();

        let mut ctx = Context::new(ty);

        let head_ptr = encode_head(&mut res, self, ty, &mut ctx)?;
        encode_body(&mut res, self, head_ptr, ty, &mut ctx)?;
        Ok(res)
    }

    /// Convert .ld binary encoding into Lutra [Value].
    pub fn decode(buf: &[u8], ty: &pr::Ty) -> Result<Value> {
        let mut ctx = Context::new(ty);

        let mut reader = Reader::new(buf);
        decode_inner(&mut reader, ty, &mut ctx)
    }
}

fn encode_head<'t>(
    w: &mut Vec<u8>,
    value: &Value,
    ty: &'t pr::Ty,
    ctx: &mut Context<'t>,
) -> Result<ValueHeadPtr> {
    let ty = resolve_ident(ty, ctx);

    match value {
        Value::Int(v) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Int)?;
            v.encode_head(w)?;
            Ok(ValueHeadPtr::None)
        }
        Value::Float(v) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Float)?;
            v.encode_head(w)?;
            Ok(ValueHeadPtr::None)
        }
        Value::Bool(v) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Bool)?;

            v.encode_head(w)?;
            Ok(ValueHeadPtr::None)
        }
        Value::Text(v) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Text)?;

            v.encode_head(w).map(ValueHeadPtr::Offset)
        }
        Value::Tuple(fields) => {
            let ty_fields = expect_ty(ty, |k| k.as_tuple(), "tuple")?;

            let mut head_ptrs = Vec::with_capacity(fields.len());
            for (f, f_ty) in fields.iter().zip(ty_fields) {
                head_ptrs.push(encode_head(w, f, &f_ty.ty, ctx)?);
            }

            Ok(ValueHeadPtr::Tuple(head_ptrs))
        }
        Value::Array(items) => {
            expect_ty(ty, |k| k.as_array(), "array")?;

            let offset_ptr = ReversePointer::new(w);
            w.write_all(&(items.len() as u32).to_le_bytes())?;
            Ok(ValueHeadPtr::Offset(offset_ptr))
        }
        Value::Enum(tag, inner) => {
            let variants = expect_ty(ty, |k| k.as_enum(), "enum")?;

            let (head, variant, tag, variant_ty) = enum_params_encode(*tag, variants)?;

            let tag_bytes = &(tag as u64).to_le_bytes()[0..(head.s / 8)];

            let r = if variant.is_inline {
                w.write_all(tag_bytes)?;
                encode_head(w, inner, variant_ty, ctx)?
            } else {
                w.write_all(tag_bytes)?;
                let offset = ReversePointer::new(w);

                ValueHeadPtr::Offset(offset)
            };

            if variant.padding > 0 {
                w.write_all(&vec![0; variant.padding / 8])?;
            }
            Ok(r)
        }
    }
}

enum ValueHeadPtr {
    None,
    Offset(ReversePointer),
    Tuple(Vec<ValueHeadPtr>),
}

fn encode_body<'t>(
    w: &mut Vec<u8>,
    value: &Value,
    head_ptr: ValueHeadPtr,
    ty: &'t pr::Ty,
    ctx: &mut Context<'t>,
) -> Result<()> {
    let ty = resolve_ident(ty, ctx);

    match value {
        Value::Int(_) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Int)?;
        }
        Value::Float(_) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Float)?;
        }
        Value::Bool(_) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Bool)?;
        }
        Value::Text(v) => {
            expect_ty_primitive(ty, pr::PrimitiveSet::Text)?;

            let ValueHeadPtr::Offset(offset_ptr) = head_ptr else {
                unreachable!()
            };

            v.encode_body(offset_ptr, w)?;
        }
        Value::Tuple(fields) => {
            let ty_fields = expect_ty(ty, |k| k.as_tuple(), "tuple")?;

            let ValueHeadPtr::Tuple(tuple_ptrs) = head_ptr else {
                unreachable!()
            };

            for ((f, h), f_ty) in fields.iter().zip(tuple_ptrs.into_iter()).zip(ty_fields) {
                encode_body(w, f, h, &f_ty.ty, ctx)?
            }
        }
        Value::Array(items) => {
            let items_ty = expect_ty(ty, |k| k.as_array(), "array")?;

            let ValueHeadPtr::Offset(offset_ptr) = head_ptr else {
                unreachable!()
            };
            offset_ptr.write(w);

            let mut head_ptrs = Vec::with_capacity(items.len());
            for i in items {
                head_ptrs.push(encode_head(w, i, items_ty, ctx)?);
            }

            for (i, h) in items.iter().zip(head_ptrs.into_iter()) {
                encode_body(w, i, h, items_ty, ctx)?;
            }
        }
        Value::Enum(tag, inner) => {
            let variants = expect_ty(ty, |k| k.as_enum(), "enum")?;

            let (_, variant_format, _, variant_ty) = enum_params_encode(*tag, variants)?;

            if variant_format.is_inline {
                encode_body(w, inner, head_ptr, variant_ty, ctx)?;
            } else {
                let ValueHeadPtr::Offset(offset_ptr) = head_ptr else {
                    unreachable!()
                };
                offset_ptr.write(w);

                let head_ptr = encode_head(w, inner, variant_ty, ctx)?;
                encode_body(w, inner, head_ptr, variant_ty, ctx)?;
            }
        }
    }

    Ok(())
}

fn enum_params_encode(
    tag: usize,
    ty_variants: &[(String, pr::Ty)],
) -> Result<(EnumHeadFormat, EnumVariantFormat, usize, &pr::Ty)> {
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

            let (offset, len) = ArrayReader::read_head(r);
            body.skip(offset);

            let mut buf = Vec::with_capacity(len);
            for _ in 0..len {
                buf.push(decode_inner(&mut body, item_ty, ctx)?);
            }

            Value::Array(buf)
        }

        pr::TyKind::Enum(variants) => {
            let head = layout::enum_head_format(variants);

            let mut tag_bytes = r.copy_n(head.s / 8).to_vec();
            tag_bytes.resize(8, 0);
            let tag = u64::from_le_bytes(tag_bytes.try_into().unwrap()) as usize;

            let (_, variant_ty) = variants.get(tag).unwrap();

            let variant_format = layout::enum_variant_format(variant_ty);

            let inner = if variant_format.is_inline {
                decode_inner(r, variant_ty, ctx)?
            } else {
                let mut body = r.clone();
                let offset = r.copy_const::<4>();
                let offset = u32::from_le_bytes(offset) as usize;
                body.skip(offset);
                decode_inner(&mut body, variant_ty, ctx)?
            };

            r.skip(variant_format.padding / 8);
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
