use crate::string::ToString;
use crate::{boxed, string, vec};

mod encode;
mod fold;
mod print_pretty;
mod print_source;

use crate::ir;
use crate::{Error, Result};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Uint8(u8),
    Uint16(u16),
    Uint32(u32),
    Uint64(u64),
    Float32(f32),
    Float64(f64),
    Text(string::String),
    Tuple(vec::Vec<Value>),
    Array(vec::Vec<Value>),
    Enum(usize, boxed::Box<Value>),
}

fn expect_ty<'t, F, K>(ty: &'t ir::Ty, cast: F, expected: &'static str) -> Result<&'t K>
where
    F: Fn(&ir::TyKind) -> Option<&K>,
{
    cast(&ty.kind).ok_or_else(|| Error::TypeMismatch {
        expected,
        found: ty_kind_name(&ty.kind).to_string(),
    })
}

fn expect_ty_primitive(ty: &ir::Ty, expected: ir::PrimitiveSet) -> Result<()> {
    let ir::TyKind::Primitive(found) = &ty.kind else {
        return Err(Error::TypeMismatch {
            expected: primitive_set_name(&expected),
            found: ty_kind_name(&ty.kind).to_string(),
        });
    };

    if *found != expected {
        return Err(Error::TypeMismatch {
            expected: primitive_set_name(&expected),
            found: primitive_set_name(found).to_string(),
        });
    }
    Ok(())
}

fn primitive_set_name(expected: &ir::PrimitiveSet) -> &'static str {
    match expected {
        ir::PrimitiveSet::bool => "bool",
        ir::PrimitiveSet::int8 => "int8",
        ir::PrimitiveSet::int16 => "int16",
        ir::PrimitiveSet::int32 => "int32",
        ir::PrimitiveSet::int64 => "int64",
        ir::PrimitiveSet::uint8 => "uint8",
        ir::PrimitiveSet::uint16 => "uint16",
        ir::PrimitiveSet::uint32 => "uint32",
        ir::PrimitiveSet::uint64 => "uint64",
        ir::PrimitiveSet::float32 => "float32",
        ir::PrimitiveSet::float64 => "float64",
        ir::PrimitiveSet::text => "text",
    }
}

fn ty_kind_name(expected: &ir::TyKind) -> &'static str {
    match expected {
        ir::TyKind::Primitive(_) => "primitive",
        ir::TyKind::Tuple(_) => "tuple",
        ir::TyKind::Array(_) => "array",
        ir::TyKind::Enum(_) => "enum",
        ir::TyKind::Function(_) => "function",
        ir::TyKind::Ident(_) => "ident",
    }
}
