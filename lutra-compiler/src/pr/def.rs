use enum_as_inner::EnumAsInner;
use indexmap::IndexMap;
use itertools::Itertools;

use crate::codespan::Span;
use crate::pr::path::Path;
use crate::pr::{Expr, Ty};

/// Definition.
#[derive(Debug, Clone, PartialEq)]
pub struct Def {
    pub kind: DefKind,
    pub span: Option<Span>,

    pub annotations: Vec<Annotation>,

    pub doc_comment: Option<DocComment>,
}

#[derive(Debug, EnumAsInner, PartialEq, Clone)]
pub enum DefKind {
    Module(ModuleDef),
    Expr(ExprDef),
    Ty(TyDef),
    Import(ImportDef),

    /// A definition that has not yet been resolved.
    /// Created during the first pass of the AST, must not be present in
    /// a fully resolved module structure.
    Unresolved(Option<Box<DefKind>>),
}

#[derive(PartialEq, Clone, Default)]
pub struct ModuleDef {
    pub defs: IndexMap<String, Def>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExprDef {
    pub value: Option<Box<Expr>>,

    pub constant: bool,
    pub ty: Option<Ty>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TyDef {
    pub ty: Ty,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ImportDef {
    pub kind: ImportKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ImportKind {
    /// Represents `std::sql::from as read_table`
    Single(Path, Option<String>),

    /// Represents `std::sql::{...}`
    Many(Path, Vec<ImportDef>),
}

impl ImportDef {
    pub fn new_simple(path: Path, span: Span) -> Self {
        Self {
            kind: ImportKind::Single(path, None),
            span,
        }
    }

    pub fn as_simple(&self) -> Option<&Path> {
        let ImportKind::Single(path, _) = &self.kind else {
            return None;
        };
        Some(path)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DocComment {
    pub content: String,
    pub span: Span,
}

impl Def {
    pub fn new<K: Into<DefKind>>(kind: K) -> Def {
        Def {
            kind: kind.into(),
            span: None,
            annotations: Vec::new(),
            doc_comment: None,
        }
    }
}

impl From<ModuleDef> for DefKind {
    fn from(value: ModuleDef) -> Self {
        DefKind::Module(value)
    }
}
impl From<Ty> for DefKind {
    fn from(ty: Ty) -> Self {
        DefKind::Ty(TyDef { ty })
    }
}
impl From<Expr> for DefKind {
    fn from(expr: Expr) -> Self {
        DefKind::Expr(ExprDef {
            value: Some(Box::new(expr)),
            ty: None,
            constant: false,
        })
    }
}

impl std::fmt::Debug for ModuleDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut ds = f.debug_struct("ModuleDef");

        if self.defs.len() < 15 {
            ds.field("defs", &DebugNames(&self.defs));
        } else {
            ds.field("defs", &format!("... {} entries ...", self.defs.len()));
        }
        ds.finish()
    }
}

struct DebugNames<'a>(&'a IndexMap<String, Def>);

impl<'a> std::fmt::Debug for DebugNames<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut dm = f.debug_map();
        for (n, def) in self.0.iter().sorted_by_key(|x| x.0) {
            dm.entry(n, def);
        }
        dm.finish()
    }
}
