mod encode;
mod fold;
mod print_source;
mod print_pretty;

use crate::{Error, Result};
use lutra_parser::parser::pr;

#[derive(Clone, Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Text(String),
    Tuple(Vec<Value>),
    Array(Vec<Value>),
    Enum(usize, Box<Value>),
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
