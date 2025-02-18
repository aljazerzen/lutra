use indexmap::IndexMap;
use std::borrow::Cow;
use std::cell::OnceCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::decl;
use crate::diagnostic::Diagnostic;
use crate::pr::{self, Path, TyParamDomain};

use super::Resolver;

#[derive(Debug)]
pub struct Scope {
    names: IndexMap<String, ScopedKind>,

    type_args: IndexMap<TyArgId, TyArg>,
}

#[derive(Debug, Clone, enum_as_inner::EnumAsInner)]
pub enum ScopedKind {
    Param { ty: pr::Ty },
    Type { ty: pr::Ty },
    TypeParam { domain: TyParamDomain },
    TypeArg { id: TyArgId },
}

#[derive(Debug, Clone)]
pub struct TyArg {
    pub domain: TyParamDomain,
    pub inferred: Rc<OnceCell<pr::Ty>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TyArgId {
    pub expr_id: usize,
    pub name: String,
}

#[derive(Debug, strum::AsRefStr)]
pub enum Named<'a> {
    Decl(&'a decl::Decl),
    Scoped(ScopedKind),
}

#[derive(Debug, Clone)]
pub enum TyRef<'a> {
    Ty(Cow<'a, pr::Ty>),
    Param(String),
    Arg(TyArgId),
}

impl Scope {
    pub fn new() -> Self {
        Self {
            type_args: IndexMap::new(),
            names: IndexMap::new(),
        }
    }

    pub fn insert_generics_params(&mut self, type_params: &[pr::TyParam]) {
        for gtp in type_params {
            let scoped = ScopedKind::TypeParam {
                domain: gtp.domain.clone(),
            };
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

    pub fn insert_generic_arg(&mut self, type_arg_id: TyArgId, domain: TyParamDomain) {
        let empty = Rc::new(OnceCell::new());

        let type_arg = TyArg {
            domain,
            inferred: empty,
        };

        self.type_args.insert(type_arg_id, type_arg);
    }

    pub fn get_ty_arg<'a>(&'a self, id: &TyArgId) -> &'a TyArg {
        self.type_args.get(id).unwrap()
    }

    pub fn infer_type_arg(&mut self, id: &TyArgId, ty: pr::Ty) {
        log::debug!("inferring {id:?} is {ty:?}");
        let type_arg = self.type_args.get(id).unwrap();
        type_arg.inferred.set(ty).unwrap();
    }

    pub fn infer_type_args_equal(&mut self, a: TyArgId, b: TyArgId) {
        log::debug!("inferring equality between {a:?} and {b:?}");
        let a_arg = self.type_args.get(&a).unwrap();
        let a_cell = Rc::clone(&a_arg.inferred);

        let b_arg = self.type_args.get_mut(&b).unwrap();
        assert!(b_arg.inferred.get().is_none());
        b_arg.inferred = a_cell;
    }

    pub fn finalize_type_args(self) -> crate::Result<HashMap<Path, pr::Ty>> {
        let mut mapping = HashMap::new();
        for (id, arg) in self.type_args.into_iter() {
            let Some(ty) = arg.inferred.get() else {
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
    pub(super) fn get_ident<'a>(&'a self, ident: &Path) -> Option<Named<'a>> {
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
                let id = TyArgId {
                    expr_id: parts.next().unwrap().parse::<usize>().unwrap(),
                    name: parts.next().unwrap().clone(),
                };

                let type_arg = scope.type_args.get(&id)?;
                if let Some(ty) = type_arg.inferred.get() {
                    Some(Named::Scoped(ScopedKind::Type { ty: ty.clone() }))
                } else {
                    Some(Named::Scoped(ScopedKind::TypeArg { id }))
                }
            } else {
                scope.names.get(part).cloned().map(Named::Scoped)
            }
        } else {
            self.root_mod.module.get(ident).map(Named::Decl)
        }
    }

    pub fn get_ty_param(&self, param_id: &str) -> &TyParamDomain {
        let scope = self.scopes.last().unwrap();
        let scoped = scope.names.get(param_id).unwrap();
        let domain = scoped.as_type_param().unwrap();

        domain
    }
}
