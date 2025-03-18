use enum_as_inner::EnumAsInner;
use strum::AsRefStr;

use crate::pr::path::Path;
use crate::span::Span;

#[derive(Debug, Clone)]
pub struct Ty {
    pub kind: TyKind,

    pub span: Option<Span>,

    /// Name inferred from the type's declaration.
    pub name: Option<String>,

    pub layout: Option<TyLayout>,

    /// For enums, indexes of variants that contain recursive
    /// (transitive) references to self. This is used to determine
    /// if a variant needs a Box in Rust.
    pub variants_recursive: Vec<u16>,
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

#[derive(Debug, Clone, PartialEq, Hash, EnumAsInner, AsRefStr)]
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
    Function(TyFunc),
    // /// Tuples that have fields of `base` tuple, but don't have fields of `except` tuple.
    // /// Implies that `base` has all fields of `except`.
    // Exclude { base: Box<Ty>, except: Box<Ty> },
}

impl TyKind {
    // Keep in sync with [lutra_bin::layout::get_layout_simple]
    pub fn get_layout_simple(&self) -> Option<TyLayout> {
        let head_size = match self {
            TyKind::Primitive(prim) => match prim {
                TyPrimitive::bool => 8,
                TyPrimitive::int8 => 8,
                TyPrimitive::int16 => 16,
                TyPrimitive::int32 => 32,
                TyPrimitive::int64 => 64,
                TyPrimitive::uint8 => 8,
                TyPrimitive::uint16 => 16,
                TyPrimitive::uint32 => 32,
                TyPrimitive::uint64 => 64,
                TyPrimitive::float32 => 32,
                TyPrimitive::float64 => 64,
                TyPrimitive::text => 64,
            },
            TyKind::Array(_) => 64,

            TyKind::Tuple(fields) => {
                let mut size = 0;
                for f in fields {
                    if let Some(layout) = &f.ty.layout {
                        size += layout.head_size;
                    } else {
                        return None;
                    }
                }
                size
            }
            TyKind::Enum(variants) => {
                let variants: Vec<_> = variants
                    .iter()
                    .cloned()
                    .map(lutra_bin::ir::TyEnumVariant::from)
                    .collect();

                let head = lutra_bin::layout::enum_head_format(&variants);
                (head.tag_bytes + head.inner_bytes) * 8
            }
            _ => return None,
        };
        let body_ptrs: Vec<u32> = match self {
            TyKind::Primitive(TyPrimitive::text) => vec![0],
            TyKind::Array(_) => vec![0],
            TyKind::Enum(_) => {
                vec![]
                //let variants: Vec<_> = variants
                //    .iter()
                //    .cloned()
                //    .map(lutra_bin::ir::TyEnumVariant::from)
                //    .collect();
                //let head = lutra_bin::layout::enum_head_format(&variants);
                //if head.inner_bytes < 4 {
                //    vec![]
                //} else {
                //    // TODO: this is not true for all variants
                //    // There might be one variant that is inline
                //    // and one that is not.
                //    // TyLayout will have to be expaned to accomodate this.
                //    vec![head.tag_bytes]
                //}
            }

            TyKind::Tuple(fields) => {
                let mut r = Vec::new();
                let mut field_start = 0;
                for f in fields {
                    let layout = f.ty.layout.as_ref().unwrap();
                    r.extend(layout.body_ptrs.iter().map(|p| p + field_start));
                    field_start += layout.head_size.div_ceil(8);
                }
                r
            }
            TyKind::Primitive(_) => vec![],

            _ => return None,
        };
        Some(TyLayout {
            head_size,
            body_ptrs,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct TyTupleField {
    /// Named tuple element.
    pub name: Option<String>,

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
    pub params: Vec<Option<Ty>>,
    pub body: Option<Box<Ty>>,
    pub ty_params: Vec<TyParam>,
}

#[derive(Debug, Clone)]
pub struct TyParam {
    /// Assigned name of this generic type argument.
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
    // TODO: generalize and replace this with TyTupleField
    TupleFields(Vec<TyDomainTupleField>),
}

#[derive(Debug, Clone)]
pub struct TyDomainTupleField {
    pub name: Option<String>,
    pub ty: TyPrimitive,
}

impl Ty {
    pub fn new(kind: impl Into<TyKind>) -> Ty {
        Ty {
            kind: kind.into(),
            span: None,
            name: None,
            layout: None,
            variants_recursive: Vec::new(),
        }
    }

    pub fn new_with_span(kind: impl Into<TyKind>, span: Span) -> Ty {
        Ty {
            kind: kind.into(),
            span: Some(span),
            name: None,
            layout: None,
            variants_recursive: Vec::new(),
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
        TyKind::Function(value)
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
