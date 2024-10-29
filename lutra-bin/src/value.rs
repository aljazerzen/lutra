use lutra_parser::parser::pr;
use std::borrow::Cow;
use std::io::Write;

use crate::layout::{self, EnumHeadFormat, EnumVariantFormat, LayoutCache};
use crate::{Decode, Encode, Error, Reader, Result};

#[derive(Debug)]
pub enum Value<'ty> {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Tuple(Vec<(Option<Cow<'ty, str>>, Value<'ty>)>),
    Array(Vec<Value<'ty>>),
    Enum(Cow<'ty, str>, Box<Value<'ty>>),
}

impl<'t> Value<'t> {
    /// Convert a Lutra [Value] to .ld binary encoding.
    pub fn encode(&self, w: &mut Vec<u8>, ty: &'t pr::Ty) -> Result<()> {
        let mut ctx = Context::new(ty);
        // run layout ahead of type to resolve recursive references
        layout::get_head_size(ty, &mut ctx.cache)?;

        let meta = encode_body(w, self, ty, &mut ctx)?;
        encode_head(w, self, meta, ty, &mut ctx)?;
        Ok(())
    }

    pub fn disown_type(self) -> Value<'static> {
        match self {
            Value::Integer(v) => Value::Integer(v),
            Value::Float(v) => Value::Float(v),
            Value::Boolean(v) => Value::Boolean(v),
            Value::String(v) => Value::String(v),
            Value::Tuple(fields) => Value::Tuple(
                fields
                    .into_iter()
                    .map(|(name, ty)| (name.map(cow_to_owned), ty.disown_type()))
                    .collect(),
            ),
            Value::Array(items) => {
                Value::Array(items.into_iter().map(|x| x.disown_type()).collect())
            }
            Value::Enum(variant, inner) => {
                Value::Enum(cow_to_owned(variant), Box::new(inner.disown_type()))
            }
        }
    }
}

fn cow_to_owned(c: Cow<'_, str>) -> Cow<'static, str> {
    match c {
        Cow::Borrowed(b) => Cow::Owned(b.to_string()),
        Cow::Owned(o) => Cow::Owned(o),
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
        Value::Enum(variant, inner) => {
            let variants = expect_ty(ty, |k| k.as_enum(), "enum")?;

            let (_, variant_format, _, variant_ty) = encode_enum_params(variant, variants, ctx)?;

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
        Value::Enum(variant, inner) => {
            let variants = expect_ty(ty, |k| k.as_enum(), "enum")?;

            let (head, variant, tag, variant_ty) = encode_enum_params(variant, variants, ctx)?;

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
    variant: &str,
    ty_variants: &'t [(String, pr::Ty)],
    cache: &mut Context<'t>,
) -> Result<(EnumHeadFormat, EnumVariantFormat, usize, &'t pr::Ty)> {
    let head_format = layout::enum_head_format(ty_variants, &mut cache.cache)?;

    let tag = ty_variants
        .iter()
        .position(|v| v.0 == variant)
        .ok_or(Error::InvalidData)?;
    let (_, variant_ty) = ty_variants.get(tag).ok_or(Error::InvalidData)?;

    let variant_format = layout::enum_variant_format(&head_format, variant_ty, &mut cache.cache)?;
    Ok((head_format, variant_format, tag, variant_ty))
}

impl<'t> Value<'t> {
    /// Convert .ld binary encoding into Lutra [Value].
    pub fn decode<'b>(buf: &'b [u8], ty: &'t pr::Ty) -> Result<Value<'t>> {
        let mut ctx = Context::new(ty);

        let head_size = layout::get_head_size(ty, &mut ctx.cache)? / 8;

        let mut reader = Reader::new(buf, buf.len() - head_size);
        decode_inner(&mut reader, ty, &mut ctx)
    }
}

fn decode_inner<'t>(
    r: &mut Reader<'_>,
    ty: &'t pr::Ty,
    ctx: &mut Context<'t>,
) -> Result<Value<'t>> {
    let ty = resolve_ident(ty, ctx);

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

                res.push((name.map(Cow::from), decode_inner(r, ty, ctx)?));
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

            let head = layout::enum_head_format(variants, &mut ctx.cache)?;

            let mut tag_bytes = r.copy_n(head.s / 8);
            tag_bytes.resize(8, 0);
            let tag = u64::from_le_bytes(tag_bytes.try_into().unwrap()) as usize;

            let (variant_name, variant_ty) = variants.get(tag).unwrap();

            let variant_format = layout::enum_variant_format(&head, variant_ty, &mut ctx.cache)?;

            let inner = if variant_format.is_inline {
                decode_inner(r, variant_ty, ctx)?
            } else {
                let offset = r.copy_const::<4>();
                let offset = u32::from_le_bytes(offset) as usize;
                body.rewind(offset);
                decode_inner(&mut body, variant_ty, ctx)?
            };

            r.skip(variant_format.padding);
            Value::Enum(Cow::from(variant_name), Box::new(inner))
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

fn expect_ty_primitive(ty: &pr::Ty, expected: pr::PrimitiveSet) -> Result<()> {
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

struct Context<'t> {
    cache: LayoutCache,
    top_level_ty: &'t pr::Ty,
}

impl<'t> Context<'t> {
    fn new(top_level_ty: &'t pr::Ty) -> Self {
        Context {
            cache: LayoutCache::default(),
            top_level_ty,
        }
    }
}

fn resolve_ident<'t>(ty: &'t pr::Ty, ctx: &mut Context<'t>) -> &'t pr::Ty {
    if let pr::TyKind::Ident(ident) = &ty.kind {
        if ctx.top_level_ty.name.as_ref() == Some(&ident.name) {
            ctx.top_level_ty
        } else {
            ty
        }
    } else {
        ty
    }
}
