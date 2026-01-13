use crate::vec;

#[cfg(not(feature = "std"))]
use alloc::collections::BTreeMap as Map;
#[cfg(feature = "std")]
use std::collections::HashMap as Map;

use bytes::{BufMut, BytesMut};

use super::{TyClass, Value};
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

    match TyClass::of_ty(ty)? {
        TyClass::Prim8 => {
            let v = value.expect_prim8()?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        TyClass::Prim16 => {
            let v = value.expect_prim16()?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }
        TyClass::Prim32 => {
            let v = value.expect_prim32()?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }

        TyClass::Prim64 => {
            let v = value.expect_prim64()?;
            v.encode_head(buf);
            Ok(ValueHeadPtr::None)
        }

        TyClass::PrimText => {
            let v = value.expect_text()?;
            Ok(ValueHeadPtr::Offset(v.encode_head(buf)))
        }

        TyClass::Tuple(ty_fields) => {
            let fields = value.expect_tuple()?;

            let mut head_ptrs = vec::Vec::with_capacity(fields.len());
            for (f, f_ty) in fields.iter().zip(ty_fields) {
                head_ptrs.push(encode_head(buf, f, &f_ty.ty, ctx)?);
            }

            Ok(ValueHeadPtr::Tuple(head_ptrs))
        }
        TyClass::Array(_ty_items) => {
            let items = value.expect_array()?;

            let offset_ptr = ReversePointer::new(buf);
            buf.put_u32_le(items.len() as u32);
            Ok(ValueHeadPtr::Offset(offset_ptr))
        }
        TyClass::Enum(ty_variants) => {
            let (tag, inner) = value.expect_enum()?;

            let (head, variant, tag, variant_ty) =
                enum_params_encode(tag, ty_variants, &ty.variants_recursive)?;

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

    match TyClass::of_ty(ty)? {
        TyClass::Prim8 | TyClass::Prim16 | TyClass::Prim32 | TyClass::Prim64 => {}
        TyClass::PrimText => {
            let v = value.expect_text()?;

            let ValueHeadPtr::Offset(offset_ptr) = head_ptr else {
                return Err(Error::Bug);
            };

            v.encode_body(offset_ptr, w);
        }
        TyClass::Tuple(ty_fields) => {
            let fields = value.expect_tuple()?;

            let ValueHeadPtr::Tuple(tuple_ptrs) = head_ptr else {
                return Err(Error::Bug);
            };

            for ((f, h), f_ty) in fields.iter().zip(tuple_ptrs.into_iter()).zip(ty_fields) {
                encode_body(w, f, h, &f_ty.ty, ctx)?
            }
        }
        TyClass::Array(items_ty) => {
            let items = value.expect_array()?;

            let ValueHeadPtr::Offset(offset_ptr) = head_ptr else {
                return Err(Error::Bug);
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
        TyClass::Enum(variants) => {
            let (tag, inner) = value.expect_enum()?;

            let (head_format, _, _, variant_ty) =
                enum_params_encode(tag, variants, &ty.variants_recursive)?;

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
                    ValueHeadPtr::Tuple(_) => return Err(Error::Bug),
                }
            } else {
                encode_body(w, inner, head_ptr, variant_ty, ctx)?;
            }
        }
    }

    Ok(())
}

fn enum_params_encode<'a>(
    tag: usize,
    variants: &'a [ir::TyEnumVariant],
    variants_recursive: &[u16],
) -> Result<(EnumHeadFormat, EnumVariantFormat, usize, &'a ir::Ty)> {
    let head_format = layout::enum_head_format(variants, variants_recursive);

    let variant = variants.get(tag).ok_or(Error::InvalidData)?;

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
