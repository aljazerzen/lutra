use indexmap::IndexMap;
use std::borrow::Cow;
use std::cell::OnceCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::decl;
use crate::diagnostic::Diagnostic;
use crate::pr::{self, Path};

use super::Resolver;

#[derive(Debug)]
pub struct Scope {
    names: IndexMap<String, ScopedKind>,

    type_args: IndexMap<TypeArgId, Rc<OnceCell<pr::Ty>>>,
}

#[derive(Debug, Clone, enum_as_inner::EnumAsInner)]
pub enum ScopedKind {
    Param { ty: pr::Ty },
    Type { ty: pr::Ty },
    TypeParam,
    TypeArg(TypeArgId),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TypeArgId {
    pub expr_id: usize,
    pub name: String,
}

#[derive(Debug, strum::AsRefStr)]
pub enum Named<'a> {
    Decl(&'a decl::Decl),
    Scoped(ScopedKind),
}

#[derive(Clone)]
pub enum TyRef<'a> {
    Ty(Cow<'a, pr::Ty>),
    Param,
    Arg(TypeArgId),
}

impl Scope {
    pub fn new() -> Self {
        Self {
            type_args: IndexMap::new(),
            names: IndexMap::new(),
        }
    }

    pub fn insert_generics_params(&mut self, type_params: &[pr::GenericTypeParam]) {
        for gtp in type_params {
            let scoped = ScopedKind::TypeParam;
            self.names.insert(gtp.name.clone(), scoped);
        }
    }

    pub fn insert_params(&mut self, func: &pr::Func) -> crate::Result<()> {
        for (position, param) in func.params.iter().enumerate() {
            let ty = param
                .ty
                .clone()
                .ok_or_else(|| Diagnostic::new_custom("missing type annotations"))?;

            let scoped = ScopedKind::Param { ty };
            self.names.insert(position.to_string(), scoped);
        }
        Ok(())
    }

    pub fn insert_generic_arg(&mut self, type_arg_id: TypeArgId) {
        let empty = Rc::new(OnceCell::new());

        self.type_args.insert(type_arg_id, empty);
    }

    pub fn infer_type_arg(&mut self, id: &TypeArgId, ty: pr::Ty) {
        log::debug!("inferring {id:?} is {ty:?}");
        let type_arg = self.type_args.get(id).unwrap();
        type_arg.set(ty).unwrap();
    }

    pub fn infer_type_args_equal(&mut self, a: TypeArgId, b: TypeArgId) {
        log::debug!("inferring equality between {a:?} and {b:?}");
        let a_cell = self.type_args.get(&a).unwrap();

        let existing = self.type_args.insert(b, Rc::clone(a_cell));
        assert!(existing.is_some());
    }

    pub fn finalize_type_args(self) -> crate::Result<HashMap<Path, pr::Ty>> {
        let mut mapping = HashMap::new();
        for (id, ty) in self.type_args.into_iter() {
            let Some(ty) = ty.get() else {
                return Err(Diagnostic::new_custom(format!(
                    "cannot infer type: {}.{}",
                    id.expr_id, id.name
                )));
            };

            mapping.insert(
                Path::new(vec![
                    "scope".to_string(),
                    "type_args".to_string(),
                    id.expr_id.to_string(),
                    id.name,
                ]),
                ty.clone(),
            );
        }
        if !mapping.is_empty() {
            log::debug!("finalized scope: {mapping:#?}");
        }
        Ok(mapping)
    }
}

impl Resolver<'_> {
    /// Get declaration from within the current scope.
    ///
    /// Does not mutate the current scope or module structure.
    pub(super) fn get_ident(&self, ident: &Path) -> Option<Named<'_>> {
        log::debug!("get_ident: {ident}");

        if ident.starts_with_part("scope") {
            let mut parts = ident.iter().peekable();
            parts.next(); // scope

            // walk up the scope stack for each `up` in the ident
            let mut scope = self.scopes.iter().rev();
            while parts.peek().map_or(false, |x| *x == "up") {
                parts.next();
                scope.next();
            }
            let scope = scope.next().unwrap();

            let part = parts.next().unwrap();
            if part == "type_args" {
                // special case: type_args
                let id = TypeArgId {
                    expr_id: parts.next().unwrap().parse::<usize>().unwrap(),
                    name: parts.next().unwrap().clone(),
                };

                let type_arg = scope.type_args.get(&id)?;
                if let Some(ty) = type_arg.get() {
                    Some(Named::Scoped(ScopedKind::Type { ty: ty.clone() }))
                } else {
                    Some(Named::Scoped(ScopedKind::TypeArg(id)))
                }
            } else {
                scope.names.get(part).cloned().map(Named::Scoped)
            }
        } else {
            self.root_mod.module.get(ident).map(Named::Decl)
        }
    }
}
