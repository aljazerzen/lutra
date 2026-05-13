use enum_as_inner::EnumAsInner;
use indexmap::IndexMap;
use itertools::Itertools;

use super::{Call, CallArg, Expr, ExprKind, Literal, Path, Ty};
use crate::Span;

/// Definition.
#[derive(Debug, Clone, PartialEq)]
pub struct Def {
    pub kind: DefKind,

    pub annotations: Vec<Annotation>,

    /// Code span of the whole definition (including doc comments and annotations)
    pub span: Option<Span>,

    /// Code span of definition name
    pub span_name: Option<Span>,
}

#[derive(Debug, EnumAsInner, PartialEq, Clone)]
pub enum DefKind {
    Module(ModuleDef),
    Expr(ExprDef),
    Ty(TyDef),
    Import(ImportDef),
}

impl DefKind {
    pub fn dummy() -> Self {
        DefKind::Module(ModuleDef::default())
    }
}

#[derive(PartialEq, Clone, Default)]
pub struct ModuleDef {
    pub imports: Vec<Def>,

    pub defs: IndexMap<String, Def>,

    // Self-annotations, defined within the module
    pub annotations: Vec<Annotation>,

    /// Span covering the content of this module, i.e. the region inside the
    /// braces for inline modules, or the whole file body for file-based
    /// submodules. This excludes inner doc comments or annotations.
    pub span_content: Option<Span>,
}

impl ModuleDef {
    /// Get definition by fully qualified ident.
    pub fn get(&self, fq_ident: &Path) -> Option<&Def> {
        let sub_module = self.get_submodule(fq_ident.parent()?)?;
        sub_module.defs.get(fq_ident.last())
    }

    /// Get definition by fully qualified ident and return remaining steps into the def.
    pub fn try_get<'a, 's>(&'a self, steps: &'s [String]) -> Option<(&'a Def, &'s [String])> {
        let mut curr_mod = self;
        for (index, step) in steps.iter().enumerate() {
            let def = curr_mod.defs.get(step)?;
            if let DefKind::Module(sub_module) = &def.kind {
                curr_mod = sub_module;
            } else {
                return Some((def, &steps[(index + 1)..]));
            }
        }
        None
    }

    /// Get an exclusive reference to definition by fully qualified ident.
    pub fn get_mut(&mut self, ident: &Path) -> Option<&mut Def> {
        let module = self.get_module_mut(ident.parent()?)?;

        module.defs.get_mut(ident.last())
    }

    pub fn get_submodule<'a>(&'a self, path: &[String]) -> Option<&'a ModuleDef> {
        let mut curr_mod = self;
        for step in path {
            let def = curr_mod.defs.get(step)?;
            curr_mod = def.kind.as_module()?;
        }
        Some(curr_mod)
    }

    pub fn get_module_mut(&mut self, path: &[String]) -> Option<&mut ModuleDef> {
        let mut curr_mod = self;
        for step in path {
            let def = curr_mod.defs.get_mut(step)?;
            curr_mod = def.kind.as_module_mut()?;
        }
        Some(curr_mod)
    }

    pub fn iter_defs(&self) -> impl Iterator<Item = (&String, &Def)> {
        self.defs.iter()
    }

    pub fn iter_defs_re(&self) -> impl Iterator<Item = (Path, &Def)> {
        let non_modules = (self.defs.iter())
            .filter(|(_, d)| !d.kind.is_module())
            .map(|(name, d)| (Path::from_name(name), d));

        let sub_defs = (self.defs.iter())
            .filter(|(_, d)| d.kind.is_module())
            .flat_map(|(name, d)| {
                let sub_module = d.kind.as_module().unwrap();
                sub_module
                    .iter_defs_re()
                    .map(|(p, d)| (Path::from_name(name).append(p), d))
                    .collect_vec()
            });

        non_modules.chain(sub_defs)
    }

    /// Collect all non-module def paths under this module into a set.
    pub fn collect_unresolved(&self) -> std::collections::HashSet<Path> {
        let p = Path::empty();
        self.collect_unresolved_re(&p)
    }

    fn collect_unresolved_re(&self, prefix: &Path) -> std::collections::HashSet<Path> {
        let mut paths = std::collections::HashSet::new();
        for (name, def) in &self.defs {
            let mut path = prefix.clone();
            path.push(name.clone());
            if let DefKind::Module(sub) = &def.kind {
                paths.extend(sub.collect_unresolved_re(&path));
            } else {
                paths.insert(path);
            }
        }
        paths
    }

    pub fn get_doc_at(&self, fq: &Path) -> Option<&str> {
        if fq.is_empty() {
            get_doc(&self.annotations)
        } else {
            self.get(fq)?.get_doc()
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExprDef {
    pub value: Box<Expr>,

    pub constant: bool,
    pub ty: Option<Ty>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TyDef {
    pub ty: Ty,
    pub is_framed: bool,
    pub framed_label: Option<String>,
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

    /// Represents `std::sql::*`
    Star(Path),
}

impl ImportDef {
    pub fn new_simple(path: Path, span: Span) -> Self {
        Self {
            kind: ImportKind::Single(path, None),
            span,
        }
    }
}

impl ImportKind {
    pub fn as_simple(&self) -> Option<&Path> {
        let ImportKind::Single(path, _) = self else {
            return None;
        };
        Some(path)
    }

    pub fn path(&self) -> &Path {
        match self {
            ImportKind::Single(path, _) => path,
            ImportKind::Many(path, _) => path,
            ImportKind::Star(path) => path,
        }
    }

    pub fn path_mut(&mut self) -> &mut Path {
        match self {
            ImportKind::Single(p, _) => p,
            ImportKind::Many(p, _) => p,
            ImportKind::Star(p) => p,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
    pub expr: Box<Expr>,
}

impl Annotation {
    pub fn new(name: &str, args: Vec<CallArg>, span: Span) -> Self {
        let subject = Box::new(Expr::new_with_span(Path::from_name(name), span));
        let expr = Box::new(Expr::new_with_span(Call { subject, args }, span));
        Annotation { expr }
    }

    /// Construct a `@doc("...")` annotation.
    pub fn new_doc(content: String, span: Span) -> Self {
        let content = Expr::new_with_span(Literal::Text(content), span);
        Self::new("doc", vec![CallArg::simple(content)], span)
    }

    pub fn as_doc(&self) -> Option<&str> {
        let ExprKind::Call(call) = &self.expr.kind else {
            return None;
        };
        let ExprKind::Ident(path) = &call.subject.kind else {
            return None;
        };
        if path.len() == 1
            && path.first() == "doc"
            && let Some(arg) = call.args.first()
            && let ExprKind::Literal(super::Literal::Text(text)) = &arg.expr.kind
        {
            return Some(text.as_str());
        }
        None
    }
}

/// Extract the doc string from a `@doc("...")` annotation, if present.
pub fn get_doc(annotations: &[Annotation]) -> Option<&str> {
    annotations.iter().find_map(Annotation::as_doc)
}

impl Def {
    pub fn new<K: Into<DefKind>>(kind: K) -> Def {
        Def {
            kind: kind.into(),
            annotations: Vec::new(),

            span: None,
            span_name: None,
        }
    }

    pub fn get_doc(&self) -> Option<&str> {
        get_doc(&self.annotations)
            .or_else(|| self.kind.as_module().and_then(|m| get_doc(&m.annotations)))
    }
}

impl From<ModuleDef> for DefKind {
    fn from(value: ModuleDef) -> Self {
        DefKind::Module(value)
    }
}
impl From<ImportDef> for DefKind {
    fn from(value: ImportDef) -> Self {
        DefKind::Import(value)
    }
}
impl From<Expr> for DefKind {
    fn from(expr: Expr) -> Self {
        DefKind::Expr(ExprDef {
            value: Box::new(expr),
            ty: None,
            constant: false,
        })
    }
}

impl std::fmt::Debug for ModuleDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut ds = f.debug_struct("ModuleDef");

        if !self.annotations.is_empty() {
            ds.field("annotations", &self.annotations);
        }

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
