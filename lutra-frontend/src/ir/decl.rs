use std::collections::HashMap;
use std::fmt::Debug;

use enum_as_inner::EnumAsInner;
use indexmap::IndexMap;
use itertools::Itertools;

use crate::ir::pl;
use crate::pr::{self, Span, Ty};

/// Context of the pipeline.
#[derive(Default, Clone)]
pub struct RootModule {
    /// A tree of all accessible statements
    pub module: Module,

    pub ordering: Vec<pl::Path>,

    pub span_map: HashMap<usize, Span>,
}

#[derive(Default, PartialEq, Clone)]
pub struct Module {
    /// Names declared in this module. This is the important thing.
    pub names: IndexMap<String, Decl>,

    /// List of relative paths to include in search path when doing lookup in
    /// this module.
    ///
    /// Assuming we want to lookup `average`, which is in `std`. The root module
    /// does not contain the `average`. So instead:
    /// - look for `average` in root module and find nothing,
    /// - follow redirects in root module,
    /// - because of redirect `std`, so we look for `average` in `std`,
    /// - there is `average` is `std`,
    /// - result of the lookup is FQ ident `std.average`.
    pub redirects: Vec<pl::Path>,

    /// A declaration that has been shadowed (overwritten) by this module.
    pub shadowed: Option<Box<Decl>>,
}

/// A struct containing information about a single declaration
/// within a PRQL module.
#[derive(Debug, PartialEq, Default, Clone)]
pub struct Decl {
    // TODO: make this plain usize, it is populated at creation anyway
    pub declared_at: Option<usize>,

    pub kind: DeclKind,

    /// Some declarations (like relation columns) have an order to them.
    /// 0 means that the order is irrelevant.
    pub order: usize,

    pub annotations: Vec<pl::Annotation>,
}

/// Declaration kind.
#[derive(Debug, PartialEq, Clone, EnumAsInner)]
pub enum DeclKind {
    /// A nested namespace
    Module(Module),

    /// A function parameter (usually the implicit `this` param)
    // TODO: make this type non-optional
    Variable(Option<Ty>),

    TupleField,

    Expr(Box<pl::Expr>),

    Ty(Ty),

    /// Equivalent to the declaration pointed to by the fully qualified ident
    Import(pr::Path),

    /// A declaration that has not yet been resolved.
    /// Created during the first pass of the AST, must not be present in
    /// a fully resolved module structure.
    Unresolved(Option<pl::StmtKind>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct GenericParam {
    pub domain: Vec<Ty>,
    pub bounds: Vec<Ty>,
}

impl std::fmt::Debug for RootModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.module.fmt(f)
    }
}

impl std::fmt::Debug for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut ds = f.debug_struct("Module");

        if !self.redirects.is_empty() {
            let redirects = self.redirects.iter().map(|x| x.to_string()).collect_vec();
            ds.field("redirects", &redirects);
        }

        if self.names.len() < 15 {
            ds.field("names", &DebugNames(&self.names));
        } else {
            ds.field("names", &format!("... {} entries ...", self.names.len()));
        }
        if self.shadowed.is_some() {
            ds.field("shadowed", &"(hidden)");
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

impl Default for DeclKind {
    fn default() -> Self {
        DeclKind::Module(Module::default())
    }
}

// TODO: convert to Decl::new
impl From<DeclKind> for Decl {
    fn from(kind: DeclKind) -> Self {
        Decl {
            kind,
            declared_at: None,
            order: 0,
            annotations: Vec::new(),
        }
    }
}
