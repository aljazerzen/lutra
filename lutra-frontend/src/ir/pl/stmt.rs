use enum_as_inner::EnumAsInner;

use crate::pr::{Path, Ty};
use crate::Span;

use super::{Expr, FuncCall};

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub id: Option<usize>,
    pub kind: StmtKind,
    pub span: Option<Span>,

    pub annotations: Vec<Annotation>,
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

impl Annotation {
    /// Utility to match function calls by name and unpack its arguments.
    pub fn as_func_call(&self, name: &str) -> Option<&FuncCall> {
        let call = self.expr.kind.as_func_call()?;

        let func_name = call.name.kind.as_ident()?;
        if func_name.len() != 1 || func_name.name() != name {
            return None;
        }
        Some(call)
    }
}
