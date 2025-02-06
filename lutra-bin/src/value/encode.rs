use bytes::{BufMut, BytesMut};

use crate::{ir, reader};

use super::{expect_ty, expect_ty_primitive, Value};
use crate::encode::ReversePointer;
use crate::layout::{self, EnumHeadFormat, EnumVariantFormat, Layout};
use crate::{ArrayReader, Decode, Encode, Error, Reader, Result};

impl Value {
    /// Convert a Lutra [Value] to .ld binary encoding.
    pub fn encode(&self, ty: &ir::Ty) -> Result<Vec<u8>> {
        let mut buf = BytesMut::new();

        let mut ctx = Context::new(ty);

        let head_ptr = encode_head(&mut buf, self, ty, &mut ctx)?;
        encode_body(&mut buf, self, head_ptr, ty, &mut ctx)?;
        Ok(buf.to_vec())
    }

    /// Convert .ld binary encoding into Lutra [Value].
    pub fn decode(buf: &[u8], ty: &ir::Ty) -> Result<Value> {
        let mut ctx = Context::new(ty);

        let mut reader = Reader::new(buf);
        decode_inner(&mut reader, ty, &mut ctx)
    }
}

fn encode_head<'t>(
    buf: &mut BytesMut,
    value: &Value,
    ty: &'t ir::Ty,
    ctx: &mut Context<'t>,
) -> Result<ValueHeadPtr> {
    let ty = resolve_ident(ty, ctx);

    match value {
        Value::Bool(v) => {
            expect_ty_primitive(ty, ir::PrimitiveSet::bool)?;

            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }

        Value::Int8(v) => {
            expect_ty_primitive(ty, ir::PrimitiveSet::int8)?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        Value::Int16(v) => {
            expect_ty_primitive(ty, ir::PrimitiveSet::int16)?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        Value::Int32(v) => {
            expect_ty_primitive(ty, ir::PrimitiveSet::int32)?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        Value::Int64(v) => {
            expect_ty_primitive(ty, ir::PrimitiveSet::int64)?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        Value::Uint8(v) => {
            expect_ty_primitive(ty, ir::PrimitiveSet::uint8)?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        Value::Uint16(v) => {
            expect_ty_primitive(ty, ir::PrimitiveSet::uint16)?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        Value::Uint32(v) => {
            expect_ty_primitive(ty, ir::PrimitiveSet::uint32)?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        Value::Uint64(v) => {
            expect_ty_primitive(ty, ir::PrimitiveSet::uint64)?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        Value::Float32(v) => {
            expect_ty_primitive(ty, ir::PrimitiveSet::float32)?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        Value::Float64(v) => {
            expect_ty_primitive(ty, ir::PrimitiveSet::float64)?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        Value::Text(v) => {
            expect_ty_primitive(ty, ir::PrimitiveSet::text)?;

            Ok(ValueHeadPtr::Offset(v.encode_head(buf)))
        }
        Value::Tuple(fields) => {
            let ty_fields = expect_ty(ty, |k| k.as_tuple(), "tuple")?;

            let mut head_ptrs = Vec::with_capacity(fields.len());
            for (f, f_ty) in fields.iter().zip(ty_fields) {
                head_ptrs.push(encode_head(buf, f, &f_ty.ty, ctx)?);
            }

            Ok(ValueHeadPtr::Tuple(head_ptrs))
        }
        Value::Array(items) => {
            expect_ty(ty, |k| k.as_array(), "array")?;

            let offset_ptr = ReversePointer::new(buf);
            buf.put_u32_le(items.len() as u32);
            Ok(ValueHeadPtr::Offset(offset_ptr))
        }
        Value::Enum(tag, inner) => {
            let variants = expect_ty(ty, |k| k.as_enum(), "enum")?;

            let (head, variant, tag, variant_ty) = enum_params_encode(*tag, variants)?;

            let tag_bytes = &(tag as u64).to_le_bytes()[0..(head.s / 8)];

            let r = if variant.is_inline {
                buf.put_slice(tag_bytes);
                encode_head(buf, inner, variant_ty, ctx)?
            } else {
                buf.put_slice(tag_bytes);
                let offset = ReversePointer::new(buf);

                ValueHeadPtr::Offset(offset)
            };

            if variant.padding > 0 {
                buf.put_bytes(0, variant.padding / 8);
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
    w: &mut BytesMut,
    value: &Value,
    head_ptr: ValueHeadPtr,
    ty: &'t ir::Ty,
    ctx: &mut Context<'t>,
) -> Result<()> {
    let ty = resolve_ident(ty, ctx);

    match value {
        Value::Int8(_)
        | Value::Int16(_)
        | Value::Int32(_)
        | Value::Int64(_)
        | Value::Uint8(_)
        | Value::Uint16(_)
        | Value::Uint32(_)
        | Value::Uint64(_)
        | Value::Float32(_)
        | Value::Float64(_)
        | Value::Bool(_) => {}
        Value::Text(v) => {
            expect_ty_primitive(ty, ir::PrimitiveSet::text)?;

            let ValueHeadPtr::Offset(offset_ptr) = head_ptr else {
                unreachable!()
            };

            v.encode_body(offset_ptr, w);
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
            offset_ptr.write_cur_len(w);

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
                offset_ptr.write_cur_len(w);

                let head_ptr = encode_head(w, inner, variant_ty, ctx)?;
                encode_body(w, inner, head_ptr, variant_ty, ctx)?;
            }
        }
    }

    Ok(())
}

fn enum_params_encode(
    tag: usize,
    ty_variants: &[ir::TyEnumVariant],
) -> Result<(EnumHeadFormat, EnumVariantFormat, usize, &ir::Ty)> {
    let head_format = layout::enum_head_format(ty_variants);

    let variant = ty_variants.get(tag).ok_or(Error::InvalidData)?;

    let variant_format = layout::enum_variant_format(&variant.ty);
    Ok((head_format, variant_format, tag, &variant.ty))
}

fn decode_inner<'t>(r: &mut Reader<'_>, ty: &'t ir::Ty, ctx: &mut Context<'t>) -> Result<Value> {
    let ty = resolve_ident(ty, ctx);

    Ok(match &ty.kind {
        ir::TyKind::Primitive(ir::PrimitiveSet::bool) => Value::Bool(decode::<bool>(r)?),
        ir::TyKind::Primitive(ir::PrimitiveSet::int8) => Value::Int8(decode::<i8>(r)?),
        ir::TyKind::Primitive(ir::PrimitiveSet::int16) => Value::Int16(decode::<i16>(r)?),
        ir::TyKind::Primitive(ir::PrimitiveSet::int32) => Value::Int32(decode::<i32>(r)?),
        ir::TyKind::Primitive(ir::PrimitiveSet::int64) => Value::Int64(decode::<i64>(r)?),
        ir::TyKind::Primitive(ir::PrimitiveSet::uint8) => Value::Uint8(decode::<u8>(r)?),
        ir::TyKind::Primitive(ir::PrimitiveSet::uint16) => Value::Uint16(decode::<u16>(r)?),
        ir::TyKind::Primitive(ir::PrimitiveSet::uint32) => Value::Uint32(decode::<u32>(r)?),
        ir::TyKind::Primitive(ir::PrimitiveSet::uint64) => Value::Uint64(decode::<u64>(r)?),
        ir::TyKind::Primitive(ir::PrimitiveSet::float32) => Value::Float32(decode::<f32>(r)?),
        ir::TyKind::Primitive(ir::PrimitiveSet::float64) => Value::Float64(decode::<f64>(r)?),
        ir::TyKind::Primitive(ir::PrimitiveSet::text) => {
            let res = String::decode(r.as_ref())?;
            r.skip(String::head_size().div_ceil(8));
            Value::Text(res)
        }

        ir::TyKind::Tuple(fields) => {
            let mut res = Vec::with_capacity(fields.len());
            for field in fields {
                res.push(decode_inner(r, &field.ty, ctx)?);
            }
            Value::Tuple(res)
        }
        ir::TyKind::Array(item_ty) => {
            let mut body = r.clone();

            let (offset, len) = ArrayReader::read_head(r.as_ref());
            r.skip(8);

            body.skip(offset);

            let mut buf = Vec::with_capacity(len);
            for _ in 0..len {
                buf.push(decode_inner(&mut body, item_ty, ctx)?);
            }

            Value::Array(buf)
        }

        ir::TyKind::Enum(variants) => {
            let head = layout::enum_head_format(variants);

            let mut tag_bytes = r.read_n(head.s / 8).to_vec();
            tag_bytes.resize(8, 0);
            let tag = u64::from_le_bytes(tag_bytes.try_into().unwrap()) as usize;

            let variant = variants.get(tag).unwrap();

            let variant_format = layout::enum_variant_format(&variant.ty);

            let inner = if variant_format.is_inline {
                decode_inner(r, &variant.ty, ctx)?
            } else {
                let mut body = r.clone();
                let offset = r.read_const::<4>();
                let offset = u32::from_le_bytes(offset) as usize;
                body.skip(offset);
                decode_inner(&mut body, &variant.ty, ctx)?
            };

            r.skip(variant_format.padding / 8);
            Value::Enum(tag, Box::new(inner))
        }

        _ => return Err(Error::InvalidType),
    })
}

fn decode<D: Decode + Sized>(reader: &mut reader::Reader) -> Result<D> {
    let res = D::decode(reader.as_ref());
    reader.skip(D::head_size().div_ceil(8));
    res
}

struct Context<'t> {
    top_level_ty: &'t ir::Ty,
}

impl<'t> Context<'t> {
    fn new(top_level_ty: &'t ir::Ty) -> Self {
        Context { top_level_ty }
    }
}

fn resolve_ident<'t>(ty: &'t ir::Ty, ctx: &mut Context<'t>) -> &'t ir::Ty {
    if let ir::TyKind::Ident(ident) = &ty.kind {
        if ctx.top_level_ty.name.as_ref() == ident.0.last() {
            ctx.top_level_ty
        } else {
            ty
        }
    } else {
        ty
    }
}
