use std::collections::HashMap;

use enum_as_inner::EnumAsInner;
use semver::VersionReq;

use crate::pr::path::Path;
use crate::pr::{Expr, Ty};
use crate::span::Span;

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct QueryDef {
    pub version: Option<VersionReq>,
    pub other: HashMap<String, String>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum VarDefKind {
    Let,
    Into,
    Main,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Option<Span>,

    pub annotations: Vec<Annotation>,

    pub doc_comment: Option<String>,
}

#[derive(Debug, EnumAsInner, PartialEq, Clone)]
pub enum StmtKind {
    VarDef(VarDef),
    TypeDef(TypeDef),
    ModuleDef(ModuleDef),
    ImportDef(ImportDef),
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarDef {
    pub kind: VarDefKind,
    pub name: String,
    pub value: Option<Box<Expr>>,

    pub ty: Option<Ty>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeDef {
    pub name: String,
    pub value: Option<Ty>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ModuleDef {
    pub name: String,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ImportDef {
    pub alias: Option<String>,
    pub name: Path,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
    pub expr: Box<Expr>,
}

impl Stmt {
    pub fn new(kind: StmtKind) -> Stmt {
        Stmt {
            kind,
            span: None,
            annotations: Vec::new(),
            doc_comment: None,
        }
    }
}
