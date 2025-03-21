use indexmap::IndexMap;

use crate::pr;

#[derive(Debug)]
pub struct Scope {
    id: usize,
    names: IndexMap<String, ScopedKind>,
}

#[derive(Debug, Clone, enum_as_inner::EnumAsInner)]
pub enum ScopedKind {
    Param,
    Generic,
}

impl Scope {
    pub fn new_of_func(id: usize, func: &pr::Func) -> crate::Result<Self> {
        let mut scope = Self {
            id,
            names: IndexMap::new(),
        };
        scope.insert_generics(&func.ty_params)?;
        scope.insert_params(func)?;
        Ok(scope)
    }

    pub fn new_of_ty_func(id: usize, func: &pr::TyFunc) -> crate::Result<Self> {
        let mut scope = Self {
            id,
            names: IndexMap::new(),
        };
        scope.insert_generics(&func.ty_params)?;
        Ok(scope)
    }

    pub fn insert_generics(&mut self, type_params: &[pr::TyParam]) -> crate::Result<()> {
        for param in type_params {
            let scoped = ScopedKind::Generic;
            self.names.insert(param.name.clone(), scoped);
        }
        Ok(())
    }

    pub fn insert_params(&mut self, func: &pr::Func) -> crate::Result<()> {
        for param in func.params.iter() {
            self.names.insert(param.name.clone(), ScopedKind::Param);
        }
        Ok(())
    }

    pub fn get(&self, name: &str) -> Option<(usize, usize, &ScopedKind)> {
        let (position, _, scoped) = self.names.get_full(name)?;
        Some((self.id, position, scoped))
    }
}
