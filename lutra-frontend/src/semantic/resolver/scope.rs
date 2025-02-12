use indexmap::IndexMap;

use crate::_lexer::Diagnostic;
use crate::decl;
use crate::pr::{self, Path};

use super::Resolver;

#[derive(Debug)]
pub struct Scope {
    // TODO: use something other than Decl here
    names: IndexMap<String, decl::Decl>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            names: IndexMap::new(),
        }
    }

    pub fn new_of_func(func: &pr::Func) -> Self {
        let names = itertools::chain(
            func.params.iter().map(|p| {
                let dummy = pr::Expr::new(pr::Path::new::<String, _>(vec![]));
                let dummy = decl::Decl::new(decl::DeclKind::Expr(Box::new(dummy)));

                (p.name.clone(), dummy)
            }),
            func.generic_type_params.iter().map(|gtp| {
                let dummy = pr::Ty::new(pr::TyKind::Tuple(vec![]));
                let dummy = decl::Decl::new(decl::DeclKind::Ty(dummy));

                (gtp.name.clone(), dummy)
            }),
        )
        .collect();

        Self { names }
    }

    pub fn insert_params(&mut self, func: &pr::Func) -> crate::Result<()> {
        for (index, param) in func.params.iter().enumerate() {
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
            self.names.insert(index.to_string(), decl);
        }
        Ok(())
    }

    pub fn get(&self, name: &str) -> Option<&decl::Decl> {
        self.names.get(name)
    }

    pub fn get_index(&self, name: &str) -> Option<usize> {
        self.names.get_index_of(name)
    }

    #[allow(dead_code)]
    pub fn get_mut(&mut self, name: &str) -> Option<&mut decl::Decl> {
        self.names.get_mut(name)
    }
}

impl Resolver<'_> {
    /// Get declaration from within the current scope.
    ///
    /// Does not mutate the current scope or module structure.
    pub(super) fn get_ident(&self, ident: &Path) -> Option<&decl::Decl> {
        if ident.starts_with_part("func") {
            let mut parts = ident.iter().peekable();
            parts.next(); // func

            // walk up the scope stack for each `up` in the ident
            let mut scope = self.scopes.iter().rev();
            while parts.peek().map_or(false, |x| *x == "up") {
                parts.next();
                scope.next();
            }
            let scope = scope.next().unwrap();
            scope.get(parts.next().unwrap())
        } else {
            self.root_mod.module.get(ident)
        }
    }
}
