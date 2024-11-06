use std::fmt::Debug;

use enum_as_inner::EnumAsInner;
use indexmap::IndexMap;
use itertools::Itertools;
use lutra_parser::pr::Span;

use crate::pr;

/// Context of the pipeline.
#[derive(Default, Clone)]
pub struct RootModule {
    /// A tree of all accessible statements
    pub module: Module,

    // TODO: make a more efficient "a ordered vec of unordered groups" data structure
    pub ordering: Vec<Vec<pr::Path>>,
}

#[derive(Default, PartialEq, Clone)]
pub struct Module {
    /// Names declared in this module. This is the important thing.
    pub names: IndexMap<String, Decl>,
}

/// A struct containing information about a single declaration
/// within a PRQL module.
#[derive(Debug, PartialEq, Clone)]
pub struct Decl {
    pub span: Option<Span>,

    pub kind: DeclKind,

    pub annotations: Vec<pr::Annotation>,
}

/// Declaration kind.
#[derive(Debug, PartialEq, Clone, EnumAsInner)]
pub enum DeclKind {
    /// A nested namespace
    Module(Module),

    Expr(Box<pr::Expr>),

    Ty(pr::Ty),

    /// Equivalent to the declaration pointed to by the fully qualified ident
    Import(pr::Path),

    /// A declaration that has not yet been resolved.
    /// Created during the first pass of the AST, must not be present in
    /// a fully resolved module structure.
    Unresolved(Option<pr::StmtKind>),
}

impl std::fmt::Debug for RootModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.module.fmt(f)
    }
}

impl std::fmt::Debug for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut ds = f.debug_struct("Module");

        if self.names.len() < 15 {
            ds.field("names", &DebugNames(&self.names));
        } else {
            ds.field("names", &format!("... {} entries ...", self.names.len()));
        }
        ds.finish()
    }
}

struct DebugNames<'a>(&'a IndexMap<String, Decl>);

impl<'a> std::fmt::Debug for DebugNames<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut dm = f.debug_map();
        for (n, decl) in self.0.iter().sorted_by_key(|x| x.0) {
            dm.entry(n, decl);
        }
        dm.finish()
    }
}

impl Decl {
    pub fn new(kind: impl Into<DeclKind>) -> Self {
        Decl {
            kind: kind.into(),
            span: None,
            annotations: Vec::new(),
        }
    }
}

impl From<Module> for DeclKind {
    fn from(value: Module) -> Self {
        DeclKind::Module(value)
    }
}
