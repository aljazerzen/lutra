use indexmap::IndexMap;

use crate::pr;

#[derive(Debug)]
pub struct Scope {
    id: usize,
    names: IndexMap<String, ()>,
}

impl Scope {
    pub fn new_empty(id: usize) -> Self {
        Self {
            id,
            names: IndexMap::new(),
        }
    }

    pub fn new_of_func(id: usize, func: &pr::Func) -> crate::Result<Self> {
        let mut scope = Self::new_empty(id);
        scope.insert_generics(&func.ty_params)?;
        scope.insert_params(func)?;
        Ok(scope)
    }

    pub fn new_of_ty_func(id: usize, func: &pr::TyFunc) -> crate::Result<Self> {
        let mut scope = Self::new_empty(id);
        scope.insert_generics(&func.ty_params)?;
        Ok(scope)
    }

    pub fn insert_generics(&mut self, type_params: &[pr::TyParam]) -> crate::Result<()> {
        for param in type_params {
            self.names.insert(param.name.clone(), ());
        }
        Ok(())
    }

    pub fn insert_params(&mut self, func: &pr::Func) -> crate::Result<()> {
        for param in func.params.iter() {
            self.names.insert(param.name.clone(), ());
        }
        Ok(())
    }

    pub fn insert_local(&mut self, name: String) {
        self.names.insert(name.to_string(), ());
    }

    pub fn insert_ty_local(&mut self, name: &str) {
        self.names.insert(name.to_string(), ());
    }

    pub fn get(&self, name: &str) -> Option<(usize, usize, &())> {
        let (position, _, scoped) = self.names.get_full(name)?;
        Some((self.id, position, scoped))
    }
}
