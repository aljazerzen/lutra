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

    /// For enums, indexes of variants that contain recursive
    /// (transitive) references to self. This is used to determine
    /// if a variant needs a Box in Rust.
    pub variants_recursive: Vec<u16>,
}

#[derive(Debug, Clone, PartialEq, Hash, EnumAsInner, AsRefStr)]
pub enum TyKind {
    /// Identifier that still needs to be resolved.
    Ident(Path),

    /// Type of a built-in primitive type
    Primitive(PrimitiveSet),

    /// Type of tuples (product)
    Tuple(Vec<TyTupleField>),

    /// Type of arrays
    Array(Box<Ty>),

    /// Type of functions with defined params and return types.
    Enum(Vec<TyEnumVariant>),

    /// Type of functions with defined params and return types.
    Function(Option<TyFunc>),
    // /// Tuples that have fields of `base` tuple, but don't have fields of `except` tuple.
    // /// Implies that `base` has all fields of `except`.
    // Exclude { base: Box<Ty>, except: Box<Ty> },
}

impl TyKind {
    pub fn into_ty(self: TyKind, span: Span) -> Ty {
        Ty {
            kind: self,
            span: Some(span),
            name: None,
            layout: None,
        }
    }

    pub fn get_layout_simple(&self) -> Option<TyLayout> {
        let head_size = match self {
            TyKind::Primitive(prim) => match prim {
                PrimitiveSet::bool => 8,
                PrimitiveSet::int8 => 8,
                PrimitiveSet::int16 => 16,
                PrimitiveSet::int32 => 32,
                PrimitiveSet::int64 => 64,
                PrimitiveSet::uint8 => 8,
                PrimitiveSet::uint16 => 16,
                PrimitiveSet::uint32 => 32,
                PrimitiveSet::uint64 => 64,
                PrimitiveSet::float32 => 32,
                PrimitiveSet::float64 => 64,
                PrimitiveSet::text => 64,
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
                let tag_size = enum_tag_size(variants.len());

                tag_size + 32
            }
            _ => return None,
        };
        let body_ptrs: Vec<u32> = match self {
            TyKind::Primitive(PrimitiveSet::text) => vec![0],
            TyKind::Array(_) => vec![0],
            TyKind::Enum(_) => vec![1], // TODO: this is wrong (in some cases)

            TyKind::Tuple(fields) => {
                let mut r = Vec::new();
                for f in fields {
                    let ty = f.ty.layout.as_ref().unwrap();
                    r.extend(&ty.body_ptrs);
                }
                r
            }
            TyKind::Primitive(_) => vec![],

            _ => return None,
        };
        Some(TyLayout {
            head_size,
            body_ptrs,
            variants_recursive: vec![],
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
pub enum PrimitiveSet {
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
}

impl Ty {
    pub fn new<K: Into<TyKind>>(kind: K) -> Ty {
        Ty {
            kind: kind.into(),
            span: None,
            name: None,
            layout: None,
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

fn enum_tag_size(variants_len: usize) -> u32 {
    // TODO: when bool-sub-byte packing is implemented, remove function in favor of enum_tag_size_used
    enum_tag_size_used(variants_len).div_ceil(8) * 8
}

fn enum_tag_size_used(variants_len: usize) -> u32 {
    f64::log2(variants_len as f64).ceil() as u32
}

#[test]
fn test_enum_tag_size() {
    assert_eq!(0, enum_tag_size_used(0));
    assert_eq!(0, enum_tag_size_used(1));
    assert_eq!(1, enum_tag_size_used(2));
    assert_eq!(2, enum_tag_size_used(3));
    assert_eq!(2, enum_tag_size_used(4));
    assert_eq!(3, enum_tag_size_used(5));
    assert_eq!(3, enum_tag_size_used(6));
    assert_eq!(3, enum_tag_size_used(7));
    assert_eq!(3, enum_tag_size_used(8));
    assert_eq!(4, enum_tag_size_used(9));
    assert_eq!(4, enum_tag_size_used(10));
    assert_eq!(4, enum_tag_size_used(11));
    assert_eq!(4, enum_tag_size_used(12));
    assert_eq!(4, enum_tag_size_used(13));
    assert_eq!(4, enum_tag_size_used(14));
    assert_eq!(4, enum_tag_size_used(15));
    assert_eq!(4, enum_tag_size_used(16));
    assert_eq!(5, enum_tag_size_used(17));
    assert_eq!(5, enum_tag_size_used(18));
    assert_eq!(5, enum_tag_size_used(19));
    assert_eq!(5, enum_tag_size_used(20));
    assert_eq!(5, enum_tag_size_used(21));
    assert_eq!(5, enum_tag_size_used(22));
}
