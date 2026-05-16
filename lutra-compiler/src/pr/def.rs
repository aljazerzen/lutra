use enum_as_inner::EnumAsInner;
use indexmap::IndexMap;
use itertools::Itertools;

use super::{Call, CallArg, Expr, ExprKind, Literal, Path, Ref, Ty, TyFuncParam};
use crate::Span;

/// Definition.
#[derive(Debug, Clone, PartialEq)]
pub struct Def {
    pub kind: DefKind,

    pub annotations: Vec<Anno>,

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
    Anno(AnnoDef),
}

/// Annotation type definition. Introduced with `anno name(const params...)`.
#[derive(Debug, PartialEq, Clone)]
pub struct AnnoDef {
    pub params: Vec<TyFuncParam>,
}

#[derive(PartialEq, Clone, Default)]
pub struct ModuleDef {
    pub imports: Vec<Def>,

    pub defs: IndexMap<String, Def>,

    // Self-annotations, defined within the module
    pub annotations: Vec<Anno>,

    /// Span covering the content of this module, i.e. the region inside the
    /// braces for inline modules, or the whole file body for file-based
    /// submodules. This excludes inner doc comments or annotations.
    pub span_content: Option<Span>,
}

impl ModuleDef {
    /// Get definition by fully qualified ident.
    pub fn get(&self, fq_ident: &Path) -> Option<&Def> {
        let sub_module = self.get_module(fq_ident.parent()?)?;
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

    pub fn get_module<'a>(&'a self, path: &[String]) -> Option<&'a ModuleDef> {
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

    pub fn get_doc_at<'a>(&'a self, fq: &Path) -> Option<&'a str> {
        self.get_anno_at(fq, Anno::as_std_doc)
    }

    pub fn get_anno_at<'a, R: 'a>(
        &'a self,
        fq: &Path,
        matcher: impl Fn(&'a Anno) -> Option<R>,
    ) -> Option<R> {
        if fq.is_empty() {
            self.annotations.iter().find_map(matcher)
        } else {
            self.get(fq)?.annotations.iter().find_map(matcher)
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
pub struct Anno {
    pub expr: Box<Expr>,
}

impl Anno {
    pub fn new(name: Path, args: Vec<CallArg>, span: Span) -> Self {
        let subject = Box::new(Expr::new_with_span(name, span));
        let expr = Box::new(Expr::new_with_span(Call { subject, args }, span));
        Anno { expr }
    }

    pub fn get_args_mut(&mut self) -> Option<&mut Vec<CallArg>> {
        self.expr.kind.as_call_mut().map(|c| &mut c.args)
    }

    pub fn as_named<'a>(&'a self, name: &[&str]) -> Option<&'a [CallArg]> {
        if is_named_fq(&self.expr, name) {
            return Some(&[]);
        }
        if let ExprKind::Call(call) = &self.expr.kind
            && is_named_fq(&call.subject, name)
        {
            return Some(&call.args);
        }
        None
    }

    // --- std annotations ---

    /// Construct a `@std::doc("...")` annotation.
    pub fn new_std_doc(content: String, span: Span) -> Self {
        let content = Expr::new_with_span(Literal::Text(content), span);
        let args = vec![CallArg::simple(content)];
        Anno::new(Path::new(["std", "doc"].to_vec()), args, span)
    }

    /// Matches as `@std::doc("...")` annotation.
    pub fn as_std_doc(&self) -> Option<&str> {
        let args = (self.as_named(&["std", "doc"])).or_else(|| self.as_named(&["doc"]))?;
        // fallback is needed for docs on defs in std
        Some(args.first()?.expr.kind.as_literal()?.as_text()?)
    }

    /// Matches as `@std::hidden` annotation.
    pub fn as_hidden(&self) -> Option<()> {
        let _ = self
            .as_named(&["std", "hidden"])
            .or_else(|| self.as_named(&["hidden"]))?;
        // fallback is needed for docs on defs in std
        Some(())
    }

    /// Matches as `@std::package(name = "...")` annotation.
    pub fn as_std_package(&self) -> Option<&str> {
        let args = self
            .as_named(&["std", "package"])
            .or_else(|| self.as_named(&["package"]))?;
        // fallback is needed for the @package on std itself
        Some(args.first()?.expr.kind.as_literal()?.as_text()?)
    }

    /// Matches as `@std::schema` annotation.
    pub fn as_std_schema(&self) -> Option<()> {
        let _ = self.as_named(&["std", "schema"])?;
        Some(())
    }

    /// Matches as `@std::rust_derive([...])` annotation.
    pub fn as_std_rust_derive(&self) -> Option<Vec<&str>> {
        let args = self.as_named(&["std", "rust_derive"])?;
        let items = args.first()?.expr.kind.as_array()?;
        items
            .iter()
            .map(|i| Some(i.kind.as_literal()?.as_text()?.as_str()))
            .collect()
    }
}

fn is_named_fq(expr: &Expr, fq: &[&str]) -> bool {
    if let Some(Ref::Global(target)) = &expr.target {
        return target.as_steps() == fq;
    }
    expr.kind.as_ident().is_some_and(|i| i.as_steps() == fq)
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

    pub fn dummy() -> Self {
        Def::new(DefKind::dummy())
    }

    pub fn get_doc(&self) -> Option<&str> {
        self.get_anno(Anno::as_std_doc)
    }

    pub fn get_anno<'a, R: 'a>(&'a self, matcher: impl Fn(&'a Anno) -> Option<R>) -> Option<R> {
        self.annotations.iter().find_map(matcher)
    }
}

impl DefKind {
    pub fn dummy() -> Self {
        DefKind::Module(ModuleDef::default())
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
