use prqlc_parser::parser::pr;
use std::io::{Read, Write};

use crate::{Decode, Encode};

#[derive(Debug)]
pub enum Value<'ty> {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Tuple(Vec<(Option<&'ty str>, Value<'ty>)>),
    Array(Vec<Value<'ty>>),
}

/// Convert a Lutra [Value] to .ld binary encoding.
pub fn encode(w: &mut impl Write, value: &Value) -> std::io::Result<()> {
    match value {
        Value::Boolean(v) => {
            v.encode(w)?;
        }
        Value::Integer(v) => {
            v.encode(w)?;
        }
        Value::Float(v) => {
            v.encode(w)?;
        }
        Value::String(v) => {
            v.encode(w)?;
        }
        Value::Tuple(fields) => {
            for (_, f) in fields {
                encode(w, f)?;
            }
        }
        Value::Array(items) => {
            w.write(&(items.len() as u64).to_le_bytes())?;
            for i in items {
                encode(w, i)?;
            }
        }
    }

    Ok(())
}

/// Convert .ld binary encoding into Lutra [Value].
pub fn decode<'t>(r: &mut impl Read, ty: &'t pr::Ty) -> std::io::Result<Value<'t>> {
    Ok(match &ty.kind {
        pr::TyKind::Primitive(pr::PrimitiveSet::Bool) => Value::Boolean(bool::decode(r)?),
        pr::TyKind::Primitive(pr::PrimitiveSet::Int) => Value::Integer(i64::decode(r)?),
        pr::TyKind::Primitive(pr::PrimitiveSet::Float) => Value::Float(f64::decode(r)?),
        pr::TyKind::Primitive(pr::PrimitiveSet::Text) => Value::String(String::decode(r)?),

        pr::TyKind::Tuple(fields) => {
            let mut res = Vec::with_capacity(fields.len());
            for f in fields {
                match f {
                    pr::TyTupleField::Single(name, ty) => {
                        let name = name.as_deref();
                        let ty = ty.as_ref().unwrap();

                        res.push((name, decode(r, ty)?));
                    }
                    pr::TyTupleField::Wildcard(_) => todo!(),
                }
            }
            Value::Tuple(res)
        }
        pr::TyKind::Array(item_ty) => {
            let bytes = read_bytes::<8>(r)?;
            let len = u64::from_le_bytes(bytes) as usize;

            let mut buf = Vec::with_capacity(len);
            for _ in 0..len {
                buf.push(decode(r, &item_ty)?);
            }

            Value::Array(buf)
        }
        _ => todo!(),
    })
}

fn read_bytes<const N: usize>(r: &mut impl Read) -> std::io::Result<[u8; N]> {
    let mut buf = [0_u8; N];
    r.read_exact(&mut buf)?;
    Ok(buf)
}
