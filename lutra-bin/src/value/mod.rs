//! Generic value object
//!
//! [Value] can represent any value in the Lutra type system.
//! It is essentially moving type information from compile time into runtime,
//! which is why it not recommended to use in general.
//!
//! Use this only when you need dynamic types, i.e. data whose type cannot be
//! determined at compile time.

mod decode;
mod encode;
mod fold;
mod print_source;

pub use fold::ValueVisitor;

use crate::{boxed, string, vec};

use crate::ir;
use crate::{Error, Result};

/// Generic Lutra value object
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Prim8(u8),
    Prim16(u16),
    Prim32(u32),
    Prim64(u64),
    Text(string::String),
    Tuple(vec::Vec<Value>),
    Array(vec::Vec<Value>),
    Enum(usize, boxed::Box<Value>),
}

impl Value {
    pub fn unit() -> Value {
        Value::Tuple(vec![])
    }

    pub fn expect_prim8(&self) -> Result<u8> {
        match self {
            Value::Prim8(value) => Ok(*value),
            _ => Err(Error::BadValueType {
                expected: "Prim8",
                found: self.name(),
            }),
        }
    }

    pub fn expect_prim16(&self) -> Result<u16> {
        match self {
            Value::Prim16(value) => Ok(*value),
            _ => Err(Error::BadValueType {
                expected: "Prim16",
                found: self.name(),
            }),
        }
    }

    pub fn expect_prim32(&self) -> Result<u32> {
        match self {
            Value::Prim32(value) => Ok(*value),
            _ => Err(Error::BadValueType {
                expected: "Prim32",
                found: self.name(),
            }),
        }
    }

    pub fn expect_prim64(&self) -> Result<u64> {
        match self {
            Value::Prim64(value) => Ok(*value),
            _ => Err(Error::BadValueType {
                expected: "Prim64",
                found: self.name(),
            }),
        }
    }

    pub fn expect_text(&self) -> Result<&str> {
        match self {
            Value::Text(value) => Ok(value),
            _ => Err(Error::BadValueType {
                expected: "Text",
                found: self.name(),
            }),
        }
    }

    pub fn expect_tuple(&self) -> Result<&[Value]> {
        match self {
            Value::Tuple(value) => Ok(value),
            _ => Err(Error::BadValueType {
                expected: "Tuple",
                found: self.name(),
            }),
        }
    }

    pub fn expect_array(&self) -> Result<&[Value]> {
        match self {
            Value::Array(value) => Ok(value),
            _ => Err(Error::BadValueType {
                expected: "Array",
                found: self.name(),
            }),
        }
    }

    pub fn expect_enum(&self) -> Result<(usize, &Value)> {
        match self {
            Value::Enum(tag, inner) => Ok((*tag, inner)),
            _ => Err(Error::BadValueType {
                expected: "Enum",
                found: self.name(),
            }),
        }
    }

    fn name(&self) -> &'static str {
        match self {
            Value::Prim8(..) => "Prim8",
            Value::Prim16(..) => "Prim16",
            Value::Prim32(..) => "Prim32",
            Value::Prim64(..) => "Prim64",
            Value::Text(..) => "Text",
            Value::Tuple(..) => "Tuple",
            Value::Array(..) => "Array",
            Value::Enum(..) => "Enum",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum TyClass<'t> {
    Prim8,
    Prim16,
    Prim32,
    Prim64,
    PrimText,
    Tuple(&'t [ir::TyTupleField]),
    Array(&'t ir::Ty),
    Enum(&'t [ir::TyEnumVariant]),
}

impl<'t> TyClass<'t> {
    fn of_ty(ty_mat: &'t ir::Ty) -> Result<Self> {
        Ok(match &ty_mat.kind {
            ir::TyKind::Primitive(ir::TyPrimitive::bool)
            | ir::TyKind::Primitive(ir::TyPrimitive::int8)
            | ir::TyKind::Primitive(ir::TyPrimitive::uint8) => TyClass::Prim8,
            ir::TyKind::Primitive(ir::TyPrimitive::int16)
            | ir::TyKind::Primitive(ir::TyPrimitive::uint16) => TyClass::Prim16,
            ir::TyKind::Primitive(ir::TyPrimitive::int32)
            | ir::TyKind::Primitive(ir::TyPrimitive::uint32)
            | ir::TyKind::Primitive(ir::TyPrimitive::float32) => TyClass::Prim32,
            ir::TyKind::Primitive(ir::TyPrimitive::int64)
            | ir::TyKind::Primitive(ir::TyPrimitive::uint64)
            | ir::TyKind::Primitive(ir::TyPrimitive::float64) => TyClass::Prim64,

            ir::TyKind::Primitive(ir::TyPrimitive::text) => TyClass::PrimText,

            ir::TyKind::Tuple(t) => TyClass::Tuple(t),
            ir::TyKind::Array(t) => TyClass::Array(t),
            ir::TyKind::Enum(t) => TyClass::Enum(t),

            ir::TyKind::Function(..) => return Err(Error::InvalidType),
            ir::TyKind::Ident(..) => return Err(Error::Bug),
        })
    }
}
