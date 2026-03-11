use indexmap::IndexMap;

use crate::Span;
use crate::pr;

#[derive(Debug)]
pub struct Scope {
    id: usize,
    names: IndexMap<String, Option<Span>>,
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
            self.names.insert(param.name.clone(), param.span);
        }
        Ok(())
    }

    pub fn insert_params(&mut self, func: &pr::Func) -> crate::Result<()> {
        for param in func.params.iter() {
            self.names.insert(param.name.clone(), Some(param.span));
        }
        Ok(())
    }

    pub fn insert_local(&mut self, name: String, span: Option<Span>) {
        self.names.insert(name, span);
    }

    pub fn insert_ty_local(&mut self, name: &str) {
        self.names.insert(name.to_string(), None);
    }

    pub fn get(&self, name: &str) -> Option<(usize, usize, Option<Span>)> {
        let (position, _, span) = self.names.get_full(name)?;
        Some((self.id, position, *span))
    }
}
