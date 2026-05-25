mod literal;
mod printer;

pub use crate::generated::ir::*;
#[cfg(feature = "std")]
pub use printer::{print, print_no_color, print_ty};

use crate::{boxed, string, vec};

impl Program {
    pub fn get_output_ty(&self) -> &Ty {
        let main_ty = self.main.ty.kind.as_function().unwrap();
        &main_ty.body
    }

    pub fn get_input_ty(&self) -> &Ty {
        let main_ty = self.main.ty.kind.as_function().unwrap();
        assert_eq!(main_ty.params.len(), 1);
        &main_ty.params[0]
    }
}

impl Expr {
    pub fn new(kind: impl Into<ExprKind>, ty: Ty) -> Expr {
        Expr {
            kind: kind.into(),
            ty,
        }
    }
    pub fn new_lit_bool(value: bool) -> Self {
        Expr {
            kind: ExprKind::Literal(Literal::Prim8(value as u8)),
            ty: Ty::bool(),
        }
    }
}
impl From<ParameterPtr> for ExprKind {
    fn from(ptr: ParameterPtr) -> Self {
        ExprKind::Pointer(Pointer::Parameter(ptr))
    }
}
impl From<ExternalPtr> for ExprKind {
    fn from(ptr: ExternalPtr) -> Self {
        ExprKind::Pointer(Pointer::External(ptr))
    }
}
impl From<TupleLookup> for ExprKind {
    fn from(v: TupleLookup) -> Self {
        ExprKind::TupleLookup(boxed::Box::new(v))
    }
}
impl From<Binding> for ExprKind {
    fn from(v: Binding) -> Self {
        ExprKind::Binding(boxed::Box::new(v))
    }
}
impl From<Call> for ExprKind {
    fn from(v: Call) -> Self {
        ExprKind::Call(boxed::Box::new(v))
    }
}
impl From<Function> for ExprKind {
    fn from(v: Function) -> Self {
        ExprKind::Function(boxed::Box::new(v))
    }
}
impl From<EnumEq> for ExprKind {
    fn from(eq: EnumEq) -> Self {
        ExprKind::EnumEq(boxed::Box::new(eq))
    }
}

impl PartialEq for Ty {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for Ty {}

impl Ty {
    pub fn new(kind: impl Into<TyKind>) -> Self {
        Ty {
            kind: kind.into(),
            layout: None,
            name: None,
            variants_recursive: vec![],
        }
    }
    pub fn new_ident(path: &[&str]) -> Self {
        use string::ToString;
        Ty::new(Path(path.iter().map(|n| n.to_string()).collect()))
    }
    pub fn bool() -> Self {
        Ty::new_ident(&["std", "Bool"])
    }
    pub fn text() -> Self {
        Ty::new_ident(&["std", "Text"])
    }
    pub fn new_unit() -> Self {
        Ty {
            kind: TyKind::Tuple(vec![]),
            layout: Some(TyLayout {
                head_size: 0,
                body_ptrs: vec![],
            }),
            name: None,
            variants_recursive: vec![],
        }
    }
    pub fn is_unit(&self) -> bool {
        self.kind.is_unit()
    }
}

impl TyKind {
    pub fn is_unit(&self) -> bool {
        self.as_tuple().is_some_and(|f| f.is_empty())
    }

    pub fn as_option(&self) -> Option<&Ty> {
        self.as_enum()
            .filter(|v| v.len() == 2 && v[0].ty.is_unit() && !v[1].ty.is_unit())
            .map(|v| &v[1].ty)
    }
}

impl From<TyPrimitive> for TyKind {
    fn from(value: TyPrimitive) -> Self {
        TyKind::Primitive(value)
    }
}
impl From<vec::Vec<TyTupleField>> for TyKind {
    fn from(value: vec::Vec<TyTupleField>) -> Self {
        TyKind::Tuple(value)
    }
}
impl From<TyFunction> for TyKind {
    fn from(value: TyFunction) -> Self {
        TyKind::Function(boxed::Box::new(value))
    }
}
impl From<Path> for TyKind {
    fn from(value: Path) -> Self {
        TyKind::Ident(value)
    }
}

impl Module {
    pub fn insert(&mut self, path: &[string::String], decl: Decl) {
        if path.is_empty() {
            panic!();
        }

        if path.len() == 1 {
            self.decls.retain(|d| d.name != path[0]);
            self.decls.push(ModuledeclsItems {
                name: path[0].clone(),
                decl,
            });
        } else {
            let exists = self.decls.iter().any(|d| d.name == path[0]);
            if !exists {
                self.decls.push(ModuledeclsItems {
                    name: path[0].clone(),
                    decl: Decl::Module(boxed::Box::new(Module {
                        decls: vec::Vec::new(),
                    })),
                });
            }

            let sub_module = self.decls.iter_mut().find(|d| d.name == path[0]);
            let Decl::Module(sub_module) = &mut sub_module.unwrap().decl else {
                panic!()
            };
            sub_module.insert(&path[1..], decl)
        }
    }

    pub fn iter_defs_re(&self) -> impl Iterator<Item = (Path, &Decl)> {
        self.decls.iter().flat_map(|item| match &item.decl {
            Decl::Module(sub_module) => sub_module
                .iter_defs_re()
                .map(|(mut p, d)| {
                    p.0.insert(0, item.name.clone());
                    (p, d)
                })
                .collect::<vec::Vec<_>>(),
            _ => {
                vec![(Path(vec![item.name.clone()]), &item.decl)]
            }
        })
    }

    pub fn iter_types_re(&self) -> impl Iterator<Item = (Path, &Ty)> {
        self.iter_defs_re().filter_map(|(p, d)| {
            if let Decl::Type(ty) = d {
                Some((p, ty))
            } else {
                None
            }
        })
    }
}

impl Path {
    pub fn is(&self, name: &[&str]) -> bool {
        self.0.len() == name.len() && core::iter::zip(&self.0, name).all(|(a, b)| a == b)
    }
}

impl Literal {
    pub fn new_int16(value: i16) -> Self {
        Self::Prim16(value.to_le() as u16)
    }
    pub fn new_int32(value: i32) -> Self {
        Self::Prim32(value.to_le() as u32)
    }
    pub fn new_int64(value: i64) -> Self {
        Self::Prim64(value.to_le() as u64)
    }
}

/// A helper enum for dealing with std types.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TyStd {
    Bool,
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float32,
    Float64,
    Text,
    Date,
    Time,
    Timestamp,
    Decimal,
}

impl TyStd {
    pub fn try_new(ident: &Path) -> Option<Self> {
        Self::try_from_steps(&ident.0)
    }

    pub fn try_from_steps(steps: &[string::String]) -> Option<Self> {
        if steps.len() != 2 && steps.first().unwrap() != "std" {
            return None;
        }
        Some(match steps[1].as_str() {
            "Bool" => Self::Bool,
            "Int8" => Self::Int8,
            "Int16" => Self::Int16,
            "Int32" => Self::Int32,
            "Int64" => Self::Int64,
            "Uint8" => Self::UInt8,
            "Uint16" => Self::UInt16,
            "Uint32" => Self::UInt32,
            "Uint64" => Self::UInt64,
            "Float32" => Self::Float32,
            "Float64" => Self::Float64,
            "Text" => Self::Text,
            "Date" => Self::Date,
            "Time" => Self::Time,
            "Timestamp" => Self::Timestamp,
            "Decimal" => Self::Decimal,
            _ => return None,
        })
    }

    pub fn is_int(self) -> bool {
        use TyStd::*;
        matches!(
            self,
            Int8 | Int16 | Int32 | Int64 | UInt8 | UInt16 | UInt32 | UInt64
        )
    }

    pub fn is_signed_int(self) -> bool {
        matches!(
            self,
            TyStd::Int8 | TyStd::Int16 | TyStd::Int32 | TyStd::Int64
        )
    }

    pub fn is_float(self) -> bool {
        matches!(self, TyStd::Float32 | TyStd::Float64)
    }

    pub fn is_number(self) -> bool {
        self.is_int() || self.is_float() || self == Self::Decimal
    }

    pub fn bits(self) -> u32 {
        match self {
            TyStd::Int8 | TyStd::UInt8 => 8,
            TyStd::Int16 | TyStd::UInt16 => 16,
            TyStd::Int32 | TyStd::UInt32 | TyStd::Float32 => 32,
            TyStd::Int64 | TyStd::UInt64 | TyStd::Float64 => 64,
            _ => 0,
        }
    }
}
