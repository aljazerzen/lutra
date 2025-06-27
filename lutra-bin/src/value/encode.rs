use crate::vec;

#[cfg(not(feature = "std"))]
use alloc::collections::BTreeMap as Map;
#[cfg(feature = "std")]
use std::collections::HashMap as Map;

use bytes::{BufMut, BytesMut};

use super::{Value, expect_ty, expect_ty_primitive};
use crate::encode::ReversePointer;
use crate::ir;
use crate::layout::{self, EnumHeadFormat, EnumVariantFormat};
use crate::{Encode, Error, Result};

impl Value {
    /// Convert a Lutra [Value] to .ld binary encoding.
    pub fn encode(&self, ty: &ir::Ty, ty_defs: &[ir::TyDef]) -> Result<vec::Vec<u8>> {
        let mut buf = BytesMut::new();

        let mut ctx = Context::new(ty_defs);

        let head_ptr = encode_head(&mut buf, self, ty, &mut ctx)?;
        encode_body(&mut buf, self, head_ptr, ty, &mut ctx)?;
        Ok(buf.to_vec())
    }
}

fn encode_head<'t>(
    buf: &mut BytesMut,
    value: &Value,
    ty: &'t ir::Ty,
    ctx: &mut Context<'t>,
) -> Result<ValueHeadPtr> {
    let ty = ctx.get_mat_ty(ty);

    match value {
        Value::Bool(v) => {
            expect_ty_primitive(ty, ir::TyPrimitive::bool)?;

            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }

        Value::Int8(v) => {
            expect_ty_primitive(ty, ir::TyPrimitive::int8)?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        Value::Int16(v) => {
            expect_ty_primitive(ty, ir::TyPrimitive::int16)?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        Value::Int32(v) => {
            expect_ty_primitive(ty, ir::TyPrimitive::int32)?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        Value::Int64(v) => {
            expect_ty_primitive(ty, ir::TyPrimitive::int64)?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        Value::Uint8(v) => {
            expect_ty_primitive(ty, ir::TyPrimitive::uint8)?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        Value::Uint16(v) => {
            expect_ty_primitive(ty, ir::TyPrimitive::uint16)?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        Value::Uint32(v) => {
            expect_ty_primitive(ty, ir::TyPrimitive::uint32)?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        Value::Uint64(v) => {
            expect_ty_primitive(ty, ir::TyPrimitive::uint64)?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        Value::Float32(v) => {
            expect_ty_primitive(ty, ir::TyPrimitive::float32)?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        Value::Float64(v) => {
            expect_ty_primitive(ty, ir::TyPrimitive::float64)?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        Value::Text(v) => {
            expect_ty_primitive(ty, ir::TyPrimitive::text)?;

            Ok(ValueHeadPtr::Offset(v.encode_head(buf)))
        }
        Value::Tuple(fields) => {
            let ty_fields = expect_ty(ty, |k| k.as_tuple(), "tuple")?;

            let mut head_ptrs = vec::Vec::with_capacity(fields.len());
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

            let tag_bytes = &(tag as u64).to_le_bytes()[0..head.tag_bytes as usize];
            buf.put_slice(tag_bytes);

            let r = if head.has_ptr {
                if variant.is_unit {
                    // this is unit variant, no need to encode head
                    ValueHeadPtr::None
                } else {
                    let offset = ReversePointer::new(buf);

                    ValueHeadPtr::Offset(offset)
                }
            } else {
                encode_head(buf, inner, variant_ty, ctx)?
            };

            if variant.padding_bytes > 0 {
                buf.put_bytes(0, variant.padding_bytes as usize);
            }
            Ok(r)
        }
    }
}

enum ValueHeadPtr {
    None,
    Offset(ReversePointer),
    Tuple(vec::Vec<ValueHeadPtr>),
}

fn encode_body<'t>(
    w: &mut BytesMut,
    value: &Value,
    head_ptr: ValueHeadPtr,
    ty: &'t ir::Ty,
    ctx: &mut Context<'t>,
) -> Result<()> {
    let ty = ctx.get_mat_ty(ty);

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
            expect_ty_primitive(ty, ir::TyPrimitive::text)?;

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

            let mut head_ptrs = vec::Vec::with_capacity(items.len());
            for i in items {
                head_ptrs.push(encode_head(w, i, items_ty, ctx)?);
            }

            for (i, h) in items.iter().zip(head_ptrs.into_iter()) {
                encode_body(w, i, h, items_ty, ctx)?;
            }
        }
        Value::Enum(tag, inner) => {
            let variants = expect_ty(ty, |k| k.as_enum(), "enum")?;

            let (head_format, _, _, variant_ty) = enum_params_encode(*tag, variants)?;

            if head_format.has_ptr {
                match head_ptr {
                    ValueHeadPtr::None => {
                        // unit variant, done
                    }
                    ValueHeadPtr::Offset(offset_ptr) => {
                        offset_ptr.write_cur_len(w);

                        let head_ptr = encode_head(w, inner, variant_ty, ctx)?;
                        encode_body(w, inner, head_ptr, variant_ty, ctx)?;
                    }
                    ValueHeadPtr::Tuple(_) => unreachable!(),
                }
            } else {
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

    let variant_format = layout::enum_variant_format(&head_format, &variant.ty);
    Ok((head_format, variant_format, tag, &variant.ty))
}

pub(super) struct Context<'t> {
    ty_defs: Map<&'t ir::Path, &'t ir::Ty>,
}

impl<'t> Context<'t> {
    pub(super) fn new(ty_defs: &'t [ir::TyDef]) -> Self {
        Context {
            ty_defs: ty_defs.iter().map(|def| (&def.name, &def.ty)).collect(),
        }
    }

    pub(super) fn get_mat_ty(&self, ty: &'t ir::Ty) -> &'t ir::Ty {
        if let ir::TyKind::Ident(ident) = &ty.kind {
            self.ty_defs.get(ident).unwrap()
        } else {
            ty
        }
    }
}
