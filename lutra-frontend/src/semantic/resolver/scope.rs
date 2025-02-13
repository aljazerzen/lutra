use indexmap::IndexMap;

use crate::decl;
use crate::diagnostic::Diagnostic;
use crate::pr::{self, Path};
use crate::Span;

use super::Resolver;

#[derive(Debug)]
pub struct Scope {
    names: IndexMap<String, Scoped>,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Scoped {
    pub kind: ScopedKind,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, enum_as_inner::EnumAsInner)]
pub enum ScopedKind {
    Param { ty: pr::Ty },
    Generic,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            names: IndexMap::new(),
        }
    }

    pub fn new_of_func(func: &pr::Func) -> crate::Result<Self> {
        let mut scope = Self::new();
        scope.insert_generics(func)?;
        scope.insert_params(func)?;
        Ok(scope)
    }

    pub fn insert_generics(&mut self, func: &pr::Func) -> crate::Result<()> {
        for gtp in func.generic_type_params.iter() {
            let scoped = Scoped {
                kind: ScopedKind::Generic,
                span: gtp.span,
            };
            self.names.insert(gtp.name.clone(), scoped);
        }
        Ok(())
    }

    pub fn insert_params(&mut self, func: &pr::Func) -> crate::Result<()> {
        for param in func.params.iter() {
            let ty = param
                .ty
                .clone()
                .ok_or_else(|| Diagnostic::new_custom("missing type annotations"))?;
            let scoped = Scoped {
                kind: ScopedKind::Param { ty },
                span: Some(param.span),
            };
            self.names.insert(param.name.clone(), scoped);
        }
        Ok(())
    }

    #[allow(dead_code)]
    pub fn get(&self, name: &str) -> Option<&Scoped> {
        self.names.get(name)
    }
    pub fn get_index(&self, name: &str) -> Option<usize> {
        self.names.get_index_of(name)
    }
}

#[derive(Debug)]
pub enum Named<'a> {
    Decl(&'a decl::Decl),
    Scoped(&'a Scoped),
}

impl Resolver<'_> {
    /// Get declaration from within the current scope.
    ///
    /// Does not mutate the current scope or module structure.
    pub(super) fn get_ident(&self, ident: &Path) -> Option<Named<'_>> {
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
            let index = parts.next().unwrap().parse::<usize>().unwrap();
            let scoped = scope.names.get_index(index).map(|x| x.1);
            scoped.map(Named::Scoped)
        } else {
            self.root_mod.module.get(ident).map(Named::Decl)
        }
    }
}
