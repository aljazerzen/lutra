use enum_as_inner::EnumAsInner;
use serde::{Deserialize, Serialize};
use strum::AsRefStr;

use crate::parser::pr::ident::Ident;
use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Ty {
    pub kind: TyKind,

    pub span: Option<Span>,

    /// Name inferred from the type declaration.
    pub name: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, EnumAsInner, AsRefStr)]
pub enum TyKind {
    /// Identifier that still needs to be resolved.
    Ident(Ident),

    /// Type of a built-in primitive type
    Primitive(PrimitiveSet),

    /// Type of tuples (product)
    Tuple(Vec<TyTupleField>),

    /// Type of arrays
    Array(Box<Ty>),

    /// Type of functions with defined params and return types.
    Enum(Vec<(String, Ty)>),

    /// Type of functions with defined params and return types.
    Function(Option<TyFunc>),
}

impl TyKind {
    pub fn into_ty(self: TyKind, span: Span) -> Ty {
        Ty {
            kind: self,
            span: Some(span),
            name: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TyTupleField {
    /// Named tuple element.
    pub name: Option<String>,
    
    pub ty: Ty,
}

/// Built-in sets.
#[derive(
    Debug,
    Clone,
    Serialize,
    Deserialize,
    PartialEq,
    Eq,
    strum::EnumString,
    strum::Display,
)]
pub enum PrimitiveSet {
    #[strum(to_string = "int")]
    Int,
    #[strum(to_string = "float")]
    Float,
    #[strum(to_string = "bool")]
    Bool,
    #[strum(to_string = "text")]
    Text,
    #[strum(to_string = "date")]
    Date,
    #[strum(to_string = "time")]
    Time,
    #[strum(to_string = "timestamp")]
    Timestamp,
}

// Type of a function
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TyFunc {
    pub name_hint: Option<Ident>,
    pub params: Vec<Option<Ty>>,
    pub return_ty: Option<Box<Ty>>,
}

impl Ty {
    pub fn new<K: Into<TyKind>>(kind: K) -> Ty {
        Ty {
            kind: kind.into(),
            span: None,
            name: None,
        }
    }

    pub fn relation(tuple_fields: Vec<TyTupleField>) -> Self {
        let tuple = Ty::new(TyKind::Tuple(tuple_fields));
        Ty::new(TyKind::Array(Box::new(tuple)))
    }

    pub fn as_relation(&self) -> Option<&Vec<TyTupleField>> {
        self.kind.as_array()?.kind.as_tuple()
    }

    pub fn as_relation_mut(&mut self) -> Option<&mut Vec<TyTupleField>> {
        self.kind.as_array_mut()?.kind.as_tuple_mut()
    }

    pub fn into_relation(self) -> Option<Vec<TyTupleField>> {
        self.kind.into_array().ok()?.kind.into_tuple().ok()
    }

    pub fn is_relation(&self) -> bool {
        match &self.kind {
            TyKind::Array(elem) => {
                matches!(elem.kind, TyKind::Tuple(_))
            }
            _ => false,
        }
    }
}

impl From<PrimitiveSet> for TyKind {
    fn from(value: PrimitiveSet) -> Self {
        TyKind::Primitive(value)
    }
}

impl From<TyFunc> for TyKind {
    fn from(value: TyFunc) -> Self {
        TyKind::Function(Some(value))
    }
}
