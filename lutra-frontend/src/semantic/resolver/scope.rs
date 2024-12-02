use indexmap::IndexMap;

use crate::_lexer::Diagnostic;
use crate::decl;
use crate::pr::{self, Path};

use super::Resolver;

#[derive(Debug)]
pub struct Scope {
    // todo: use something else then Decl here
    pub types: IndexMap<String, decl::Decl>,

    // todo: use something else then Decl here
    pub values: IndexMap<String, decl::Decl>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            types: IndexMap::new(),
            values: IndexMap::new(),
        }
    }

    pub fn new_of_func(func: &pr::Func) -> Self {
        let values = func
            .params
            .iter()
            .map(|p| {
                let dummy = pr::Expr::new(pr::Path::new::<String, _>(vec![]));
                let dummy = decl::Decl::new(decl::DeclKind::Expr(Box::new(dummy)));

                (p.name.clone(), dummy)
            })
            .collect();

        let types = func
            .generic_type_params
            .iter()
            .map(|gtp| {
                let dummy = pr::Ty::new(pr::TyKind::Tuple(vec![]));
                let dummy = decl::Decl::new(decl::DeclKind::Ty(dummy));

                (gtp.name.clone(), dummy)
            })
            .collect();

        Self { types, values }
    }

    pub fn populate_from_func(&mut self, func: &pr::Func) -> crate::Result<()> {
        for param in &func.params {
            let ty = param
                .ty
                .clone()
                .ok_or_else(|| Diagnostic::new_custom("missing type annotations"))?;
            let decl = decl::Decl::new(pr::Expr {
                kind: pr::Path::from_name(param.name.clone()).into(),
                ty: Some(ty),
                span: None,
                alias: None,
                id: None,
            });
            self.values.insert(param.name.clone(), decl);
        }
        Ok(())
    }

    pub fn get(&self, name: &str) -> Option<&decl::Decl> {
        if let Some(decl) = self.types.get(name) {
            return Some(decl);
        }

        self.values.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut decl::Decl> {
        if let Some(decl) = self.types.get_mut(name) {
            return Some(decl);
        }

        self.values.get_mut(name)
    }
}

impl Resolver<'_> {
    /// Get declaration from within the current scope.
    ///
    /// Does not mutate the current scope or module structure.
    pub(super) fn get_ident(&self, ident: &Path) -> Option<&decl::Decl> {
        for scope in self.scopes.iter().rev() {
            if let Some(decl) = scope.get(ident.first()) {
                return Some(decl);
            }
        }

        self.root_mod.module.get(ident)
    }

    /// Get mutable reference to a declaration from within the current scope.
    ///
    /// Does not mutate the current scope or module structure.
    #[allow(dead_code)]
    pub(super) fn get_ident_mut(&mut self, ident: &Path) -> Option<&mut decl::Decl> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(decl) = scope.get_mut(ident.first()) {
                return Some(decl);
            }
        }

        self.root_mod.module.get_mut(ident)
    }
}
