use indexmap::IndexMap;
use std::borrow::Cow;
use std::cell::OnceCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::pr::{self, Path, TyParamDomain};
use crate::{decl, printer, utils, Result};

use super::TypeResolver;

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
        tracing::debug!("inferring {id:?} is {ty:?}");
        let type_arg = self.type_args.get(id).unwrap();
        type_arg.inferred.set(ty).unwrap();
    }

    pub fn infer_type_args_equal(&mut self, a: TyArgId, b: TyArgId) {
        tracing::debug!("inferring equality between {a:?} and {b:?}");
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
            tracing::debug!("finalized scope: {mapping:#?}");
        }
        Ok(mapping)
    }
}

impl TypeResolver<'_> {
    /// Get declaration from within the current scope.
    ///
    /// Does not mutate the current scope or module structure.
    pub(super) fn get_ident<'a>(&'a self, ident: &Path) -> Option<Named<'a>> {
        tracing::trace!("get_ident: {ident}");

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

    /// Resolves if type is an ident. Does not recurse.
    pub fn resolve_ty_ident(&self, ty: pr::Ty) -> Result<TyRef<'_>> {
        let pr::TyKind::Ident(ident) = ty.kind else {
            return Ok(TyRef::Ty(Cow::Owned(ty)));
        };
        let named = self.get_ident(&ident).ok_or_else(|| {
            tracing::debug!("scope: {:?}", self.scopes.last().unwrap());
            Diagnostic::new_assert("cannot find type ident")
                .push_hint(format!("ident={ident}"))
                .with_span(ty.span)
        })?;
        match named {
            Named::Decl(decl) => match &decl.kind {
                decl::DeclKind::Ty(t) => Ok(TyRef::Ty(Cow::Borrowed(t))),
                decl::DeclKind::Unresolved(_) => Err(Diagnostic::new_assert(format!(
                    "Unresolved ident at {:?}: (during eval of {})",
                    ty.span, self.debug_current_decl
                ))),
                _ => Err(Diagnostic::new_assert("expected a type")
                    .push_hint(format!("got {:?}", &decl.kind))
                    .with_span(ty.span)),
            },
            Named::Scoped(scoped) => match scoped {
                ScopedKind::Param { ty, .. } => {
                    Err(Diagnostic::new_assert("expected a type, found a value")
                        .push_hint(format!("got param of type `{}`", printer::print_ty(&ty)))
                        .with_span(ty.span))
                }
                ScopedKind::Type { ty } => self.resolve_ty_ident(ty),
                ScopedKind::TypeParam { .. } => Ok(TyRef::Param(ident.name().to_string())),
                ScopedKind::TypeArg { id } => Ok(TyRef::Arg(id)),
            },
        }
    }

    /// Add type's params into scope as type arguments.
    pub fn introduce_ty_into_scope(&mut self, ty: pr::Ty) -> (pr::Ty, Vec<pr::Ty>) {
        let pr::TyKind::Function(mut ty_func) = ty.kind else {
            return (ty, Vec::new());
        };

        // TODO: recurse? There might be type params deeper in the type.

        if ty_func.ty_params.is_empty() {
            return (
                pr::Ty {
                    kind: pr::TyKind::Function(ty_func),
                    ..ty
                },
                Vec::new(),
            );
        }

        let expr_id = self.id.gen();
        tracing::debug!(
            "introducing generics with expr_id={expr_id}, ty_func={}",
            crate::printer::print_ty(&pr::Ty::new(ty_func.clone()))
        );

        let mut mapping = HashMap::new();
        let mut ty_args = Vec::with_capacity(ty_func.ty_params.len());

        let scope = self.scopes.last_mut().unwrap();
        for gtp in ty_func.ty_params.drain(..) {
            let ty_arg = pr::Ty::new(Path::new(vec![
                "scope".to_string(),
                "type_args".to_string(),
                expr_id.to_string(),
                gtp.name.clone(),
            ]));
            mapping.insert(Path::new(vec!["scope", gtp.name.as_str()]), ty_arg.clone());
            ty_args.push(ty_arg);

            let type_arg_id = TyArgId {
                expr_id,
                name: gtp.name,
            };
            scope.insert_generic_arg(type_arg_id, gtp.domain);
        }

        let ty = pr::Ty {
            kind: pr::TyKind::Function(ty_func),
            ..ty
        };
        (utils::TypeReplacer::on_ty(ty, mapping), ty_args)
    }
}
