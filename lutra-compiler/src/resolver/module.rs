use enum_as_inner::EnumAsInner;
use indexmap::IndexMap;
use itertools::Itertools;

use crate::Span;
use crate::diagnostic::Diagnostic;
use crate::pr;

pub fn init_root(root_module_def: pr::ModuleDef) -> Result<pr::ModuleDef, Vec<Diagnostic>> {
    let mut root = pr::ModuleDef {
        defs: IndexMap::new(),
    };

    let diagnostics = root.populate_module(root_module_def.defs);
    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }

    Ok(root)
}

#[derive(Debug, EnumAsInner)]
pub enum ExprOrTy<'a> {
    Expr(&'a pr::Expr),
    Ty(&'a pr::Ty),
}

impl pr::ModuleDef {
    /// Get definition by fully qualified ident.
    pub fn get(&self, fq_ident: &pr::Path) -> Option<ExprOrTy> {
        let module_steps = &fq_ident.full_path()[0..(fq_ident.len() - 1)];
        let sub_module = self.get_submodule(module_steps)?;

        let def = sub_module.defs.get(fq_ident.last())?;
        match &def.kind {
            pr::DefKind::Expr(expr) => Some(ExprOrTy::Expr(expr.value.as_ref().unwrap())),
            pr::DefKind::Ty(ty) => Some(ExprOrTy::Ty(&ty.ty)),
            pr::DefKind::Unresolved(o) => {
                panic!("unresolved: {o:?}")
            }
            pr::DefKind::Module(_) | pr::DefKind::Import(_) => None,
        }
    }

    /// Get definition by fully qualified ident and return remaining steps into the def.
    pub fn try_get<'a, 's>(&'a self, steps: &'s [String]) -> Option<(&'a pr::Def, &'s [String])> {
        let mut curr_mod = self;
        for (index, step) in steps.iter().enumerate() {
            let def = curr_mod.defs.get(step)?;
            if let pr::DefKind::Module(sub_module) = &def.kind {
                curr_mod = sub_module;
            } else {
                return Some((def, &steps[(index + 1)..]));
            }
        }
        None
    }

    /// Get an exclusive reference to definition by fully qualified ident.
    pub fn get_mut(&mut self, ident: &pr::Path) -> Option<&mut pr::Def> {
        let module_steps = &ident.full_path()[0..(ident.len() - 1)];
        let module = self.get_submodule_mut(module_steps)?;

        module.defs.get_mut(ident.last())
    }

    pub fn get_submodule(&self, path: &[String]) -> Option<&pr::ModuleDef> {
        let mut curr_mod = self;
        for step in path {
            let def = curr_mod.defs.get(step)?;
            curr_mod = def.kind.as_module()?;
        }
        Some(curr_mod)
    }

    pub fn get_submodule_mut(&mut self, path: &[String]) -> Option<&mut pr::ModuleDef> {
        let mut curr_mod = self;
        for step in path {
            let def = curr_mod.defs.get_mut(step)?;
            curr_mod = def.kind.as_module_mut()?;
        }
        Some(curr_mod)
    }

    pub fn iter_defs(&self) -> impl Iterator<Item = (&String, &pr::Def)> {
        self.defs.iter()
    }

    pub fn iter_defs_re(&self) -> impl Iterator<Item = (pr::Path, &pr::Def)> {
        let non_modules = (self.defs.iter())
            .filter(|(_, d)| !d.kind.is_module())
            .map(|(name, d)| (pr::Path::from_name(name), d));

        let sub_defs = (self.defs.iter())
            .filter(|(_, d)| d.kind.is_module())
            .flat_map(|(name, d)| {
                let sub_module = d.kind.as_module().unwrap();
                sub_module
                    .iter_defs_re()
                    .map(|(p, d)| (p.prepend(vec![name.clone()]), d))
                    .collect_vec()
            });

        non_modules.chain(sub_defs)
    }

    pub(super) fn take_unresolved(&mut self, ident: &pr::Path) -> (pr::DefKind, Option<Span>) {
        let def = self.get_mut(ident).unwrap();
        let unresolved = def.kind.as_unresolved_mut().unwrap();
        (*unresolved.take().unwrap(), def.span)
    }

    pub(super) fn insert_unresolved(&mut self, ident: &pr::Path, def_kind: pr::DefKind) {
        let def = self.get_mut(ident).unwrap();
        *def.kind.as_unresolved_mut().unwrap() = Some(Box::new(def_kind));
    }

    pub(super) fn populate_module(&mut self, defs: IndexMap<String, pr::Def>) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        for (name, def) in defs {
            let kind = match def.kind {
                pr::DefKind::Module(module_def) => {
                    // init new module and recurse
                    let mut new_mod = pr::ModuleDef::default();
                    diagnostics.extend(new_mod.populate_module(module_def.defs));

                    pr::DefKind::Module(new_mod)
                }
                kind => {
                    // insert "DeclKind::Unresolved"
                    pr::DefKind::Unresolved(Some(Box::new(kind)))
                }
            };
            let def = pr::Def { kind, ..def };

            let existing = self.defs.insert(name, def);
            if existing.is_some() {
                panic!()
            }
        }
        diagnostics
    }
}
