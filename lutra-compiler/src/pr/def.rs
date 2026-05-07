use enum_as_inner::EnumAsInner;
use indexmap::IndexMap;
use itertools::Itertools;

use super::{Expr, Path, Ty};
use crate::Span;

/// Definition.
#[derive(Debug, Clone, PartialEq)]
pub struct Def {
    pub kind: DefKind,

    pub annotations: Vec<Annotation>,

    pub doc_comment: Option<DocComment>,

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

    /// A definition that has not yet been resolved.
    /// Created during the first pass of the AST, must not be present in
    /// a fully resolved module structure.
    Unresolved(Option<Box<DefKind>>),
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

#[derive(Debug, Clone, PartialEq)]
pub struct DocComment {
    pub content: String,
    pub span: Span,
}

impl ModuleDef {
    /// Get definition by fully qualified ident.
    pub fn get(&self, fq_ident: &Path) -> Option<&Def> {
        if fq_ident.is_empty() {
            return None;
        }
        let sub_module = self.get_submodule(fq_ident.parent())?;
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
        let module = self.get_module_mut(ident.parent())?;

        module.defs.get_mut(ident.last())
    }

    pub fn get_submodule(&self, path: &[String]) -> Option<&ModuleDef> {
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

    pub(crate) fn take_unresolved(&mut self, ident: &Path) -> (DefKind, Option<Span>) {
        let def = self.get_mut(ident).unwrap();
        let unresolved = def.kind.as_unresolved_mut().unwrap();
        (*unresolved.take().unwrap(), def.span)
    }

    pub(crate) fn insert_unresolved(&mut self, ident: &Path, def_kind: DefKind) {
        let def = self.get_mut(ident).unwrap();
        *def.kind.as_unresolved_mut().unwrap() = Some(Box::new(def_kind));
    }

    pub(crate) fn into_unresolved(self) -> ModuleDef {
        // init new module and recurse
        let mut new_mod = ModuleDef {
            imports: self.imports,
            annotations: self.annotations,
            span_content: self.span_content,
            ..ModuleDef::default()
        };

        for (name, def) in self.defs {
            let kind = match def.kind {
                DefKind::Module(m) => DefKind::Module(m.into_unresolved()),
                kind => DefKind::Unresolved(Some(Box::new(kind))),
            };
            new_mod.defs.insert(name, Def { kind, ..def });
        }
        new_mod
    }
}

impl Def {
    pub fn new<K: Into<DefKind>>(kind: K) -> Def {
        Def {
            kind: kind.into(),
            annotations: Vec::new(),
            doc_comment: None,

            span: None,
            span_name: None,
        }
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
