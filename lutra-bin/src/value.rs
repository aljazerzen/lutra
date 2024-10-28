use lutra_parser::parser::pr;
use std::io::Write;

use crate::{encode::Reader, layout, Decode, Encode};

#[derive(Debug)]
pub enum Value<'ty> {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Tuple(Vec<(Option<&'ty str>, Value<'ty>)>),
    Array(Vec<Value<'ty>>),
}

impl Value<'_> {
    /// Convert a Lutra [Value] to .ld binary encoding.
    pub fn encode(&self, w: &mut Vec<u8>) -> std::io::Result<()> {
        let meta = encode_body(w, self)?;
        encode_head(w, self, meta)?;
        Ok(())
    }
}

enum ValueBodyMeta {
    None,
    Offset(usize),
    Tuple(Vec<ValueBodyMeta>),
}

fn encode_body(w: &mut Vec<u8>, value: &Value) -> std::io::Result<ValueBodyMeta> {
    match value {
        Value::Boolean(_) | Value::Integer(_) | Value::Float(_) => Ok(ValueBodyMeta::None),
        Value::String(v) => {
            let meta = v.encode_body(w)?;
            Ok(ValueBodyMeta::Offset(meta))
        }
        Value::Tuple(fields) => {
            let mut metas = Vec::with_capacity(fields.len());
            for (_, f) in fields {
                metas.push(encode_body(w, f)?);
            }

            Ok(ValueBodyMeta::Tuple(metas))
        }
        Value::Array(items) => {
            let mut metas = Vec::with_capacity(items.len());
            for i in items {
                metas.push(encode_body(w, i)?);
            }

            let items_start = w.len();
            for (i, m) in items.iter().zip(metas.into_iter()) {
                encode_head(w, i, m)?;
            }

            Ok(ValueBodyMeta::Offset(items_start))
        }
    }
}

fn encode_head(w: &mut Vec<u8>, value: &Value, body_meta: ValueBodyMeta) -> std::io::Result<()> {
    match value {
        Value::Boolean(v) => {
            v.encode_head((), w)?;
        }
        Value::Integer(v) => {
            v.encode_head((), w)?;
        }
        Value::Float(v) => {
            v.encode_head((), w)?;
        }
        Value::String(v) => {
            let ValueBodyMeta::Offset(bytes_offset) = body_meta else {
                unreachable!()
            };

            v.encode_head(bytes_offset, w)?;
        }
        Value::Tuple(fields) => {
            let ValueBodyMeta::Tuple(metas) = body_meta else {
                unreachable!()
            };

            for ((_, f), m) in fields.iter().zip(metas.into_iter()) {
                encode_head(w, f, m)?;
            }
        }
        Value::Array(items) => {
            let ValueBodyMeta::Offset(items_offset) = body_meta else {
                unreachable!()
            };

            let offset = w.len() - items_offset;
            w.write(&(offset as u32).to_le_bytes())?;
            w.write(&(items.len() as u32).to_le_bytes())?;
        }
    }

    Ok(())
}

impl<'t> Value<'t> {
    /// Convert .ld binary encoding into Lutra [Value].
    pub fn decode<'b>(buf: &'b [u8], ty: &'t pr::Ty) -> std::io::Result<Value<'t>> {
        let head_size = layout::get_head_size(ty) / 8;

        let mut reader = Reader::new(buf, buf.len() - head_size);
        decode_inner(&mut reader, ty)
    }
}

fn decode_inner<'b, 't>(r: &mut Reader<'b>, ty: &'t pr::Ty) -> std::io::Result<Value<'t>> {
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
            body.apply_offset(offset);

            let len = r.copy_const::<4>();
            let len = u32::from_le_bytes(len) as usize;

            let mut buf = Vec::with_capacity(len);
            for _ in 0..len {
                buf.push(decode_inner(&mut body, &item_ty)?);
            }

            Value::Array(buf)
        }
        _ => todo!(),
    })
}
