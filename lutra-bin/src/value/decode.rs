use crate::value::TyClass;
use crate::{boxed, string, vec};

use bytes::Buf;

use super::Value;
use super::encode::Context;

use crate::ir;
use crate::layout::{self, Layout};
use crate::{ArrayReader, Decode, Result};

impl Value {
    /// Convert .ld binary encoding into Lutra [Value].
    pub fn decode(buf: &[u8], ty: &ir::Ty, ty_defs: &[ir::TyDef]) -> Result<Value> {
        let mut ctx = Context::new(ty_defs);

        let mut buf = buf;
        decode_inner(&mut buf, ty, &mut ctx)
    }
}

fn decode_inner<'t>(
    r: &mut (impl bytes::Buf + Clone),
    ty: &'t ir::Ty,
    ctx: &mut Context<'t>,
) -> Result<Value> {
    let ty = ctx.get_mat_ty(ty);

    Ok(match TyClass::of_ty(ty)? {
        TyClass::Prim8 => Value::Prim8(decode::<u8>(r)?),
        TyClass::Prim16 => Value::Prim16(decode::<u16>(r)?),
        TyClass::Prim32 => Value::Prim32(decode::<u32>(r)?),
        TyClass::Prim64 => Value::Prim64(decode::<u64>(r)?),
        TyClass::PrimText => {
            let res = string::String::decode(r.chunk())?;
            r.advance(string::String::head_size().div_ceil(8));
            Value::Text(res)
        }

        TyClass::Tuple(fields) => {
            let mut res = vec::Vec::with_capacity(fields.len());
            for field in fields {
                res.push(decode_inner(r, &field.ty, ctx)?);
            }
            Value::Tuple(res)
        }
        TyClass::Array(item_ty) => {
            let mut body = r.clone();

            let (offset, len) = ArrayReader::<&[u8]>::read_head(r.chunk());
            r.advance(8);

            body.advance(offset);

            let mut buf = vec::Vec::with_capacity(len);
            for _ in 0..len {
                buf.push(decode_inner(&mut body, item_ty, ctx)?);
            }

            Value::Array(buf)
        }

        TyClass::Enum(variants) => {
            let head = layout::enum_head_format(variants, &ty.variants_recursive);

            let mut tag_bytes = vec![0; 8];
            r.copy_to_slice(&mut tag_bytes[0..head.tag_bytes as usize]);
            tag_bytes.resize(8, 0);
            let tag = u64::from_le_bytes(tag_bytes.try_into().unwrap()) as usize;

            let variant = variants.get(tag).unwrap();

            let variant_format = layout::enum_variant_format(&head, &variant.ty);

            let inner = if head.has_ptr {
                if variant_format.is_unit {
                    // unit variant
                    Value::Tuple(vec![])
                } else {
                    let mut body = r.clone();
                    body.advance(r.get_u32_le() as usize);
                    decode_inner(&mut body, &variant.ty, ctx)?
                }
            } else {
                decode_inner(r, &variant.ty, ctx)?
            };

            r.advance(variant_format.padding_bytes as usize);
            Value::Enum(tag, boxed::Box::new(inner))
        }
    })
}

fn decode<D: Decode + Sized>(reader: &mut impl bytes::Buf) -> Result<D> {
    let res = D::decode(reader.chunk());
    reader.advance(D::head_size().div_ceil(8));
    res
}
