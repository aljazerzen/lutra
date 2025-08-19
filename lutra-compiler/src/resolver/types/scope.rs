use std::cell::RefCell;
use std::collections::HashMap;

use itertools::Itertools;

use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::pr;
use crate::resolver::module::ExprOrTy;
use crate::{Result, Span};
use crate::{printer, utils};

use super::TypeResolver;

#[derive(Debug)]
pub struct Scope {
    pub(super) id: usize,

    pub(super) kind: ScopeKind,
    pub(super) names: append_only_vec::AppendOnlyVec<ScopedKind>,

    pub(super) ty_var_constraints: RefCell<Vec<TyVarConstraint>>,
}

#[derive(Debug)]
pub enum ScopeKind {
    /// Scope that does not allow references to parent scopes.
    /// When closed, all type must be inferred.
    /// Used for variable stmts and functions with generic params.
    Isolated,

    /// Scope that allows references to parent scopes.
    /// Used for function bodies, match case blocks.
    Nested,
}

#[derive(Debug, Clone, enum_as_inner::EnumAsInner)]
pub enum ScopedKind {
    Param {
        ty: pr::Ty,
    },
    Local {
        ty: pr::Ty,
    },
    LocalTy {
        ty: pr::Ty,
    },

    /// Type parameter to a function.
    ///
    /// For example, `T` is a type param:
    /// ```lt
    /// func twice <T> (x: T): {T, T} -> {x: T, x}
    /// ```
    TyParam {
        name: String,
        domain: pr::TyParamDomain,
    },

    /// Type variable - something that we don't yet know the type of,
    /// but we will (probably) be able to infer it later on.
    ///
    /// For example, here:
    /// ```lt
    /// let x: int32 = 5
    /// ```
    /// ... `5` is initially assigned a type variable. Later on, when validating
    /// the type annotation, it infer that it must be `int32`.
    TyVar(TyVar),
}

pub type TyVarId = usize;

#[derive(Debug, Clone)]
pub struct TyVar {
    pub name_hint: Option<String>,

    pub span: Option<Span>,
}

#[derive(Debug)]
pub enum TyVarConstraint {
    IsTy(TyVarId, pr::Ty),
    Equals(TyVarId, TyVarId),
    InDomain(TyVarId, pr::TyParamDomain),
}

#[derive(Debug, strum::AsRefStr)]
pub enum Named<'a> {
    Expr(&'a pr::Expr),
    Ty(&'a pr::Ty, bool),
    Scoped(&'a ScopedKind),
    EnumVariant(&'a pr::Ty, usize),
}

/// A reference to a type-like objects
#[derive(Debug, Clone)]
pub enum TyRef<'a> {
    /// Concrete type (cannot be an ident)
    Ty(&'a pr::Ty),

    /// Reference to type param: a placeholder which must support a few different concrete types.
    /// Contains offset in scope.
    Param(usize),

    /// Reference to type variable: a placeholder which will inferred to be some non-variable type
    /// when scope is finalized. Contains scope id and offset in scope.
    #[allow(dead_code)] // TODO: actually use scope_id when fetching the scope
    Var(usize, usize),
}

// impl<'a> TyRef<'a> {
//     #[allow(dead_code)]
//     fn into_owned(self) -> TyRef<'static> {
//         match self {
//             TyRef::Ty(v) => TyRef::Ty(Cow::Owned(v.as_ref().clone())),
//             TyRef::Param(v) => TyRef::Param(v),
//             TyRef::Var(s, o) => TyRef::Var(s, o),
//         }
//     }
// }

impl Scope {
    pub fn new(id: usize, kind: ScopeKind) -> Self {
        Self {
            id,
            kind,
            names: Default::default(),
            ty_var_constraints: RefCell::new(Vec::new()),
        }
    }

    /// Returns true for scopes that should store type variables.
    /// Location of where ty vars are store determines when they will be finalized.
    pub fn for_ty_vars(&self) -> bool {
        matches!(self.kind, ScopeKind::Isolated)
    }

    pub fn insert_type_params(&mut self, type_params: &[pr::TyParam]) {
        for gtp in type_params {
            let scoped = ScopedKind::TyParam {
                name: gtp.name.clone(),
                domain: gtp.domain.clone(),
            };
            self.names.push(scoped);
        }
    }

    pub fn insert_params(&mut self, func: &pr::Func) -> Result<(), Vec<Diagnostic>> {
        let mut d = Vec::new();

        for param in &func.params {
            let Some(ty) = param.ty.clone() else {
                d.push(
                    Diagnostic::new_custom("missing type annotations").with_span(Some(param.span)),
                );
                continue;
            };

            let scoped = ScopedKind::Param { ty };
            self.names.push(scoped);
        }

        if d.is_empty() { Ok(()) } else { Err(d) }
    }

    #[must_use]
    pub fn insert_type_var(
        &self,
        name_hint: Option<String>,
        span: Span,
        domain: pr::TyParamDomain,
    ) -> usize {
        let type_arg = TyVar {
            name_hint,
            span: Some(span),
        };
        let var_id = self.names.push(ScopedKind::TyVar(type_arg));

        if !matches!(domain, pr::TyParamDomain::Open) {
            self.ty_var_constraints
                .borrow_mut()
                .push(TyVarConstraint::InDomain(var_id, domain));
        }
        var_id
    }

    pub fn insert_local(&mut self, ty: pr::Ty) -> usize {
        let local = ScopedKind::Local { ty };
        self.names.push(local)
    }

    pub fn insert_local_ty(&mut self, ty: pr::Ty) -> usize {
        let local = ScopedKind::LocalTy { ty };
        self.names.push(local)
    }

    pub fn infer_type_var(&self, id: TyVarId, ty: pr::Ty) {
        tracing::debug!("inferring {id:?} is {}", crate::printer::print_ty(&ty));

        let mut constraints = self.ty_var_constraints.borrow_mut();
        constraints.push(TyVarConstraint::IsTy(id, ty));
    }

    pub fn infer_type_var_in_domain(&self, id: TyVarId, domain: pr::TyParamDomain) {
        tracing::debug!("inferring {id:?} in domain {domain:?}");

        let mut constraints = self.ty_var_constraints.borrow_mut();
        constraints.push(TyVarConstraint::InDomain(id, domain));
    }

    pub fn infer_type_vars_equal(&self, a: TyVarId, b: TyVarId) {
        tracing::debug!("inferring equality between {a:?} and {b:?}");

        let mut constraints = self.ty_var_constraints.borrow_mut();
        constraints.push(TyVarConstraint::Equals(a, b));
    }
}

impl TypeResolver<'_> {
    pub fn get_ty_var_scope(&self) -> &Scope {
        let mut stack = self.scopes.iter().rev();
        stack.find(|s| s.for_ty_vars()).unwrap()
    }
    pub fn get_ty_var_scope_mut(&mut self) -> &mut Scope {
        let mut stack = self.scopes.iter_mut().rev();
        stack.find(|s| s.for_ty_vars()).unwrap()
    }

    /// Get definition from within the current scope.
    ///
    /// Does not mutate the current scope or module structure.
    pub(super) fn get_ident<'a>(&'a self, target: &pr::Ref) -> Result<Named<'a>> {
        tracing::trace!("get_ident: {target:?}");

        match target {
            pr::Ref::FullyQualified {
                to_def: path,
                within,
            } => {
                let def = self
                    .root_mod
                    .get(path)
                    .unwrap_or_else(|| panic!("cannot find {path}"));
                match def {
                    ExprOrTy::Expr(expr) => {
                        if !within.is_empty() {
                            unreachable!()
                        }
                        Ok(Named::Expr(expr))
                    }

                    ExprOrTy::Ty(ty) => lookup_into_ty(ty, within.full_path(), true)
                        .ok_or_else(|| Diagnostic::new_custom("unknown name")),
                }
            }

            pr::Ref::Local { scope, offset } => {
                let scope = self
                    .scopes
                    .iter()
                    .find(|s| s.id == *scope)
                    .ok_or_else(|| panic!("cannot find scope by id"))
                    .unwrap();
                Ok(Named::Scoped(&scope.names[*offset]))
            }
        }
    }

    pub fn get_ty_param(&self, param_id: usize) -> (&String, &pr::TyParamDomain) {
        let scope = self.scopes.last().unwrap();
        let scoped = &scope.names[param_id];
        scoped.as_ty_param().unwrap()
    }

    pub fn get_ty_var(&self, id: TyVarId) -> &TyVar {
        let scoped = &self.get_ty_var_scope().names[id];
        scoped.as_ty_var().unwrap()
    }

    /// Resolves type identifiers. Does not resolve identifiers in contained types.
    pub fn get_ty_mat<'t>(&'t self, ty: &'t pr::Ty) -> Result<TyRef<'t>> {
        let pr::TyKind::Ident(ident) = &ty.kind else {
            return Ok(TyRef::Ty(ty));
        };

        let target = ty.target.as_ref().unwrap();
        let named = self.get_ident(target).with_span(ty.span)?;
        // Diagnostic::new_assert("cannot find type ident")
        //     .push_hint(format!("ident={ident}"))
        //     .push_hint(format!("target={target:?}"))
        //     .with_span(ty.span)

        match named {
            Named::Ty(ty, _is_top_level) => {
                // reference to a type: all ok

                // but refed ty might be an ident too: recurse!
                self.get_ty_mat(ty)
            }
            Named::Expr(_) => Err(Diagnostic::new_assert("expected a type").with_span(ty.span)),
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
                    ScopedKind::Local { ty, .. } => {
                        Err(Diagnostic::new_assert("expected a type, found a value")
                            .push_hint(format!("got local var of type `{}`", printer::print_ty(ty)))
                            .with_span(ty.span))
                    }
                    ScopedKind::LocalTy { ty } => self.get_ty_mat(ty),
                    ScopedKind::TyParam { .. } => Ok(TyRef::Param(*offset)),
                    ScopedKind::TyVar(_) => {
                        // return type var
                        Ok(TyRef::Var(*scope, *offset))
                    }
                }
            }
            Named::EnumVariant(_, _) => {
                Err(Diagnostic::new_assert("expected a type, found a value")
                    .push_hint(format!("{ident} is an enum variant"))
                    .with_span(ty.span))
            }
        }
    }

    /// Add type's params into scope as type variables.
    pub fn introduce_ty_into_scope(&mut self, ty: pr::Ty, span: Span) -> (pr::Ty, Vec<pr::Ty>) {
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

        let mut mapping = HashMap::new();
        let mut ty_args = Vec::with_capacity(ty_func.ty_params.len());

        let scope = self.get_ty_var_scope_mut();
        tracing::debug!(
            "introducing generics for ty_func={} into scope {}",
            crate::printer::print_ty(&pr::Ty::new(ty_func.clone())),
            scope.id,
        );
        for (gtp_position, gtp) in ty_func.ty_params.drain(..).enumerate() {
            let gtp_ref = pr::Ref::Local {
                scope: ty.scope_id.unwrap(), // original scope of the type
                offset: gtp_position,
            };

            let mut ty_arg_ident = pr::Ty::new(
                // path does matter here, it is just for error messages
                pr::Path::new(vec![gtp.name.clone()]),
            );
            let offset = scope.insert_type_var(Some(gtp.name), span, gtp.domain);
            ty_arg_ident.target = Some(pr::Ref::Local {
                scope: scope.id, // current scope
                offset,
            });

            mapping.insert(gtp_ref, ty_arg_ident.clone());
            ty_args.push(ty_arg_ident);
        }

        let ty = pr::Ty {
            kind: pr::TyKind::Func(ty_func),
            ..ty
        };
        (utils::TypeReplacer::on_ty(ty, mapping), ty_args)
    }

    pub fn introduce_ty_var(&self, domain: pr::TyParamDomain, span: Span) -> pr::Ty {
        let scope = self.get_ty_var_scope();
        let var_id = scope.insert_type_var(None, span, domain);

        pr::Ty {
            span: Some(span),
            target: Some(pr::Ref::Local {
                scope: scope.id,
                offset: var_id,
            }),
            ..pr::Ty::new(pr::TyKind::Ident(pr::Path::from_name("_")))
        }
    }
}

fn lookup_into_ty<'t>(ty: &'t pr::Ty, steps: &[String], top_level: bool) -> Option<Named<'t>> {
    if steps.is_empty() {
        return Some(Named::Ty(ty, top_level));
    }
    match &ty.kind {
        pr::TyKind::Enum(variants) => {
            // path into enum

            let (position, variant) = variants.iter().find_position(|v| v.name == steps[0])?;

            if steps.len() == 1 {
                Some(Named::EnumVariant(ty, position))
            } else {
                lookup_into_ty(&variant.ty, &steps[1..], false)
            }
        }
        pr::TyKind::Tuple(fields) => {
            // path into tuple

            let field = fields
                .iter()
                .find(|f| f.name.as_ref().is_some_and(|n| n == &steps[0]))?;

            lookup_into_ty(&field.ty, &steps[1..], false)
        }
        _ => None,
    }
}
