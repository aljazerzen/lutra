use enum_as_inner::EnumAsInner;

use crate::pr::path::Path;
use crate::span::Span;

use super::Ref;

#[derive(Debug, Clone)]
pub struct Ty {
    pub kind: TyKind,

    pub span: Option<Span>,

    /// Name inferred from the type's definition.
    pub name: Option<String>,

    /// When this expr is the root of a new scope, this holds the id of
    /// that scope. This will always be set for [TyKind::Function], but
    /// might be set for other nodes too.
    pub scope_id: Option<usize>,

    /// When this type expr is an ident, this holds information
    /// what is being referenced by the ident.
    pub target: Option<Ref>,
}

/// Memory layout of a type.
#[derive(Debug, Clone)]
pub struct TyLayout {
    /// Number of bits required to store the type's the head
    /// (the part whose size is known at compile time).
    pub head_size: u32,

    /// Position of the body pointer within the head.
    /// It is measured bytes from the start of the head.
    /// Pointer itself is relative to own position (and the start of the head).
    pub body_ptrs: Vec<u32>,
}

#[derive(Debug, Clone, PartialEq, Hash, EnumAsInner, strum::AsRefStr)]
pub enum TyKind {
    /// Identifier that still needs to be resolved.
    Ident(Path),

    /// Type of a built-in primitive type
    Primitive(TyPrimitive),

    /// Type of tuples (product)
    Tuple(Vec<TyTupleField>),

    /// Type of arrays
    Array(Box<Ty>),

    /// Type of functions with defined params and return types.
    Enum(Vec<TyEnumVariant>),

    /// Type of functions with defined params and return types.
    Func(TyFunc),

    /// Tuple that is constructed by iterating over fields of a tuple
    TupleComprehension(TyTupleComprehension),
    // /// Tuples that have fields of `base` tuple, but don't have fields of `except` tuple.
    // /// Implies that `base` has all fields of `except`.
    // Exclude { base: Box<Ty>, except: Box<Ty> },
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct TyTupleField {
    pub name: Option<String>,

    pub unpack: bool,

    pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct TyEnumVariant {
    pub name: String,
    pub ty: Ty,
}

/// Built-in sets.
#[derive(Debug, Clone, PartialEq, Eq, Hash, strum::Display)]
#[allow(non_camel_case_types)]
pub enum TyPrimitive {
    int8,
    int16,
    int32,
    int64,
    uint8,
    uint16,
    uint32,
    uint64,
    float32,
    float64,
    bool,
    text,
}

// Type of a function
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct TyFunc {
    pub params: Vec<(Option<Ty>, bool)>,
    pub body: Option<Box<Ty>>,
    pub ty_params: Vec<TyParam>,
}

#[derive(Debug, Clone)]
pub struct TyParam {
    /// Assigned name of this generic type param
    pub name: String,

    pub domain: TyParamDomain,

    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub enum TyParamDomain {
    /// This param can be any type
    Open,

    /// This param must be one of the following
    OneOf(Vec<TyPrimitive>),

    /// This param must be a tuple with following fields
    TupleHasFields(Vec<TyDomainTupleField>),

    /// This param must be a tuple with exactly N fields
    TupleLen { n: usize },

    /// This param must be an enum with following variants
    EnumVariants(Vec<TyDomainEnumVariant>),
}

#[derive(Clone)]
pub struct TyDomainTupleField {
    pub location: super::Lookup,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct TyDomainEnumVariant {
    pub name: String,
    pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct TyTupleComprehension {
    pub tuple: Box<Ty>,

    pub variable_name: String,
    pub variable_ty: String,

    pub body_name: Option<String>,
    pub body_ty: Box<Ty>,
}

impl Ty {
    pub fn new(kind: impl Into<TyKind>) -> Ty {
        Ty {
            kind: kind.into(),
            span: None,
            name: None,
            target: None,
            scope_id: None,
        }
    }

    pub fn new_with_span(kind: impl Into<TyKind>, span: Span) -> Ty {
        Ty {
            kind: kind.into(),
            span: Some(span),
            name: None,
            target: None,
            scope_id: None,
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

impl From<TyPrimitive> for TyKind {
    fn from(value: TyPrimitive) -> Self {
        TyKind::Primitive(value)
    }
}

impl From<TyFunc> for TyKind {
    fn from(value: TyFunc) -> Self {
        TyKind::Func(value)
    }
}

impl From<Path> for TyKind {
    fn from(value: Path) -> Self {
        TyKind::Ident(value)
    }
}

impl PartialEq for Ty {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for Ty {}

impl std::hash::Hash for Ty {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
    }
}

impl PartialEq for TyParam {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
        // && self.domain == other.domain
    }
}

impl Eq for TyParam {}

impl std::hash::Hash for TyParam {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        // self.domain.hash(state);
    }
}

impl TyTupleField {
    pub(crate) fn matches_name(&self, name: &str) -> bool {
        self.name.as_ref().is_some_and(|n| n == name)
    }
}

impl std::fmt::Debug for TyDomainTupleField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TyDomainTupleField")
            .field("location", &self.location)
            .field("ty", &crate::printer::print_ty(&self.ty))
            .finish()
    }
}
