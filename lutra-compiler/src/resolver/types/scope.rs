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
    id: usize,
    names: Vec<ScopedKind>,
}

#[derive(Debug, Clone, enum_as_inner::EnumAsInner)]
pub enum ScopedKind {
    Param { ty: pr::Ty },
    TypeParam { name: String, domain: TyParamDomain },
    TypeArg(TyArg),
}

#[derive(Debug, Clone)]
pub struct TyArg {
    pub param_name: String,
    pub domain: TyParamDomain,
    pub inferred: Rc<OnceCell<pr::Ty>>,
}

pub type TyArgId = usize;

#[derive(Debug, strum::AsRefStr)]
pub enum Named<'a> {
    Decl(&'a decl::Decl),
    Scoped(&'a ScopedKind),
}

#[derive(Debug, Clone)]
pub enum TyRef<'a> {
    Ty(Cow<'a, pr::Ty>),
    Param(usize, Cow<'a, str>),
    Arg(usize, usize, Cow<'a, str>),
}

impl<'a> TyRef<'a> {
    #[allow(dead_code)]
    fn into_owned(self) -> TyRef<'static> {
        match self {
            TyRef::Ty(v) => TyRef::Ty(Cow::Owned(v.as_ref().clone())),
            TyRef::Param(v, n) => TyRef::Param(v, Cow::Owned(n.as_ref().to_string())),
            TyRef::Arg(s, o, n) => TyRef::Arg(s, o, Cow::Owned(n.as_ref().to_string())),
        }
    }
}

impl Scope {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            names: Vec::new(),
        }
    }

    pub fn insert_generics_params(&mut self, type_params: &[pr::TyParam]) {
        for gtp in type_params {
            let scoped = ScopedKind::TypeParam {
                name: gtp.name.clone(),
                domain: gtp.domain.clone(),
            };
            self.names.push(scoped);
        }
    }

    pub fn insert_params(&mut self, func: &pr::Func) -> crate::Result<()> {
        for param in &func.params {
            let ty = param
                .ty
                .clone()
                .ok_or_else(|| Diagnostic::new_custom("missing type annotations"))?;

            let scoped = ScopedKind::Param { ty };
            self.names.push(scoped);
        }
        Ok(())
    }

    pub fn insert_generic_arg(&mut self, param_name: String, domain: TyParamDomain) {
        let empty = Rc::new(OnceCell::new());

        let type_arg = TyArg {
            param_name,
            domain,
            inferred: empty,
        };

        self.names.push(ScopedKind::TypeArg(type_arg));
    }

    pub fn get_ty_arg<'a>(&'a self, id: &TyArgId) -> &'a TyArg {
        let ScopedKind::TypeArg(arg) = self.names.get(*id).unwrap() else {
            panic!()
        };
        arg
    }

    fn get_ty_arg_mut<'a>(&'a mut self, id: &TyArgId) -> &'a mut TyArg {
        let ScopedKind::TypeArg(arg) = self.names.get_mut(*id).unwrap() else {
            panic!()
        };
        arg
    }

    pub fn infer_type_arg(&mut self, id: &TyArgId, ty: pr::Ty) {
        tracing::debug!("inferring {id:?} is {ty:?}");
        let type_arg = self.get_ty_arg(id);
        type_arg.inferred.set(ty).unwrap();
    }

    pub fn infer_type_args_equal(&mut self, a: TyArgId, b: TyArgId) {
        tracing::debug!("inferring equality between {a:?} and {b:?}");
        let a_arg = self.get_ty_arg(&a);
        let a_cell = Rc::clone(&a_arg.inferred);

        let b_arg = self.get_ty_arg_mut(&b);
        assert!(b_arg.inferred.get().is_none());
        b_arg.inferred = a_cell;
    }

    pub fn finalize_type_args(self) -> crate::Result<HashMap<pr::Ref, pr::Ty>> {
        let mut mapping = HashMap::new();

        for (offset, scoped) in self.names.iter().enumerate() {
            let ScopedKind::TypeArg(arg) = scoped else {
                continue;
            };

            let Some(ty) = arg.inferred.get() else {
                return Err(Diagnostic::new_custom(format!(
                    "cannot infer type: {}",
                    arg.param_name,
                )));
            };

            mapping.insert(
                pr::Ref::Local {
                    scope: self.id,
                    offset,
                },
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
    pub(super) fn get_ident<'a>(&'a self, target: &pr::Ref) -> Option<Named<'a>> {
        tracing::trace!("get_ident: {target:?}");

        match target {
            pr::Ref::FullyQualified(path) => self.root_mod.module.get(path).map(Named::Decl),

            pr::Ref::Local { scope, offset } => {
                let scope = self.scopes.iter().find(|s| s.id == *scope).unwrap();
                scope.names.get(*offset).map(Named::Scoped)
            }
        }
    }

    pub fn get_ty_param(&self, param_id: usize) -> &TyParamDomain {
        let scope = self.scopes.last().unwrap();
        let scoped = scope.names.get(param_id).unwrap();
        let (_, domain) = scoped.as_type_param().unwrap();

        domain
    }

    /// Resolves if type is an ident. Does not recurse.
    pub fn get_ty_mat<'t>(&'t self, ty: &'t pr::Ty) -> Result<TyRef<'t>> {
        let pr::TyKind::Ident(ident) = &ty.kind else {
            return Ok(TyRef::Ty(Cow::Borrowed(ty)));
        };

        let target = ty.target.as_ref().unwrap();
        let named = self.get_ident(target).ok_or_else(|| {
            tracing::debug!("scope: {:#?}", self.scopes.last().unwrap());
            Diagnostic::new_assert("cannot find type ident")
                .push_hint(format!("ident={ident}"))
                .push_hint(format!("target={target:?}"))
                .with_span(ty.span)
        })?;

        match named {
            Named::Decl(decl) => match &decl.kind {
                decl::DeclKind::Ty(ty) => {
                    // reference to a type: all ok

                    // but refed ty might be an ident too: recurse!
                    self.get_ty_mat(ty)
                }
                decl::DeclKind::Unresolved(_) => Err(Diagnostic::new_assert(format!(
                    "Unresolved ident at {:?}: (during eval of {})",
                    ty.span, self.debug_current_decl
                ))),
                _ => Err(Diagnostic::new_assert("expected a type")
                    .push_hint(format!("got {:?}", &decl.kind))
                    .with_span(ty.span)),
            },
            Named::Scoped(scoped) => {
                let pr::Ref::Local { scope, offset } = target else {
                    panic!()
                };
                match scoped {
                    ScopedKind::Param { ty, .. } => {
                        Err(Diagnostic::new_assert("expected a type, found a value")
                            .push_hint(format!("got param of type `{}`", printer::print_ty(ty)))
                            .with_span(ty.span))
                    }
                    ScopedKind::TypeParam { name, .. } => {
                        Ok(TyRef::Param(*offset, Cow::Borrowed(name)))
                    }
                    ScopedKind::TypeArg(arg) => {
                        if let Some(ty) = arg.inferred.get() {
                            // type arg already inferred a type: use that

                            // but first, recurse!
                            self.get_ty_mat(ty)
                        } else {
                            // return type arg
                            Ok(TyRef::Arg(*scope, *offset, Cow::Borrowed(&arg.param_name)))
                        }
                    }
                }
            }
        }
    }

    /// Add type's params into scope as type arguments.
    pub fn introduce_ty_into_scope(&mut self, ty: pr::Ty) -> (pr::Ty, Vec<pr::Ty>) {
        let pr::TyKind::Func(mut ty_func) = ty.kind else {
            return (ty, Vec::new());
        };

        // TODO: recurse? There might be type params deeper in the type.

        if ty_func.ty_params.is_empty() {
            return (
                pr::Ty {
                    kind: pr::TyKind::Func(ty_func),
                    ..ty
                },
                Vec::new(),
            );
        }

        tracing::debug!(
            "introducing generics for ty_func={}",
            crate::printer::print_ty(&pr::Ty::new(ty_func.clone()))
        );

        let mut mapping = HashMap::new();
        let mut ty_args = Vec::with_capacity(ty_func.ty_params.len());

        let scope = self.scopes.last_mut().unwrap();
        for (gtp_position, gtp) in ty_func.ty_params.drain(..).enumerate() {
            let gtp_ref = pr::Ref::Local {
                scope: ty.scope_id.unwrap(), // original scope of the type
                offset: gtp_position,
            };

            let mut ty_arg_ident = pr::Ty::new(
                // path does matter here, it is just for error messages
                Path::new(vec![gtp.name.clone()]),
            );
            let offset = scope.names.len();
            ty_arg_ident.target = Some(pr::Ref::Local {
                scope: scope.id, // current scope
                offset,
            });

            mapping.insert(gtp_ref, ty_arg_ident.clone());
            ty_args.push(ty_arg_ident);

            scope.insert_generic_arg(gtp.name, gtp.domain);
        }

        let ty = pr::Ty {
            kind: pr::TyKind::Func(ty_func),
            ..ty
        };
        (utils::TypeReplacer::on_ty(ty, mapping), ty_args)
    }

    pub fn introduce_ty_arg(&mut self, domain: pr::TyParamDomain) -> pr::Ty {
        let scope = self.scopes.last_mut().unwrap();

        let name = "unnamed";

        let mut ty_arg = pr::Ty::new(Path::new(vec![name.to_string()]));
        ty_arg.target = Some(pr::Ref::Local {
            scope: scope.id, // current scope
            offset: scope.names.len(),
        });

        scope.insert_generic_arg(name.to_string(), domain);
        ty_arg
    }
}
