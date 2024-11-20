mod encode;
mod fold;
mod print_pretty;
mod print_source;

use crate::ir;
use crate::{Error, Result};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Text(String),
    Tuple(Vec<Value>),
    Array(Vec<Value>),
    Enum(usize, Box<Value>),
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
        ir::PrimitiveSet::int => "int",
        ir::PrimitiveSet::float => "float",
        ir::PrimitiveSet::bool => "bool",
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
