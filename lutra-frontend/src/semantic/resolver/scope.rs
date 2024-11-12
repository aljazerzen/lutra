use indexmap::IndexMap;

use crate::decl::Decl;
use crate::pr::Path;
use crate::semantic::NS_LOCAL;

use super::Resolver;

#[derive(Debug)]
pub(super) struct Scope {
    pub types: IndexMap<String, Decl>,

    pub values: IndexMap<String, Decl>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            types: IndexMap::new(),
            values: IndexMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Decl> {
        if let Some(decl) = self.types.get(name) {
            return Some(decl);
        }

        self.values.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Decl> {
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
    pub(super) fn get_ident(&self, ident: &Path) -> Option<&Decl> {
        if ident.starts_with_part(NS_LOCAL) {
            assert!(ident.len() == 2);
            self.scopes.last()?.get(ident.name())
        } else {
            self.root_mod.module.get(ident)
        }
    }

    /// Get mutable reference to a declaration from within the current scope.
    ///
    /// Does not mutate the current scope or module structure.
    #[allow(dead_code)]
    pub(super) fn get_ident_mut(&mut self, ident: &Path) -> Option<&mut Decl> {
        if ident.starts_with_part(NS_LOCAL) {
            assert!(ident.len() == 2);
            self.scopes.last_mut()?.get_mut(ident.name())
        } else {
            self.root_mod.module.get_mut(ident)
        }
    }
}
