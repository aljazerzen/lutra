use crate::diagnostic::WithErrorInfo;
use crate::resolver::types::{TypeResolver, scope, tuple};
use crate::utils::fold::{self, PrFold};
use crate::{Result, printer};
use crate::{pr, utils};

impl fold::PrFold for super::TypeResolver<'_> {
    #[tracing::instrument(name = "e", skip(self, node))]
    fn fold_expr(&mut self, node: pr::Expr) -> Result<pr::Expr> {
        tracing::debug!("{}", node.kind.as_ref());
        let span = node.span;

        let r = match node.kind {
            pr::ExprKind::Ident(ident) => {
                tracing::debug!("resolving ident {ident:?}...");

                let target = node.target.as_ref().unwrap();
                let named = self.get_ref(target).with_span(span)?;

                tracing::debug!("... resolved to {}", named.as_ref());

                let ty = match named {
                    scope::Named::Expr(expr) => expr.ty.clone().unwrap(),
                    scope::Named::Module => {
                        return Err(scope::err_name_kind("a value", "a module").with_span(span));
                    }
                    scope::Named::Ty {
                        is_framed: false, ..
                    } => {
                        return Err(scope::err_name_kind("a value", "a type").with_span(span));
                    }
                    scope::Named::Scoped(scoped) => match scoped {
                        scope::ScopedKind::Param { ty } => ty.clone(),
                        scope::ScopedKind::Local { ty } => ty.clone(),
                        scope::ScopedKind::LocalTy { .. }
                        | scope::ScopedKind::TyParam { .. }
                        | scope::ScopedKind::TyVar { .. } => {
                            return Err(scope::err_name_kind("a value", "a type")
                                .push_hint(format!("scoped = {scoped:?}"))
                                .with_span(span));
                        }
                    },
                    scope::Named::Ty {
                        is_framed: true,
                        ty,
                    } => {
                        // framed types can be called like a function to act as a constructor

                        let ty_framed = pr::Ty {
                            target: Some(target.clone()),
                            ..pr::Ty::new(ident.clone())
                        };
                        let ty_inner = ty.clone();

                        return Ok(pr::Expr {
                            kind: pr::ExprKind::Ident(ident),
                            ty: Some(pr::Ty::new(pr::TyFunc {
                                params: vec![pr::TyFuncParam::simple(Some(ty_inner))],
                                body: Some(Box::new(ty_framed)),
                                ty_params: vec![],
                            })),
                            ..node
                        });
                    }
                };
                let (ty, ty_args) = self.introduce_ty_into_scope(ty, span.unwrap());
                pr::Expr {
                    kind: pr::ExprKind::Ident(ident),
                    ty: Some(ty),
                    ty_args,
                    ..node
                }
            }

            pr::ExprKind::Lookup { base, lookup } => {
                let base = Box::new(self.fold_expr(*base)?);
                let base_ty = base.ty.as_ref().unwrap();

                // special case: lookups into framed types
                let base_ty_target = base_ty.target.as_ref().map(|r| self.get_ref(r).unwrap());
                if let Some(scope::Named::Ty {
                    ty,
                    is_framed: true,
                }) = base_ty_target
                {
                    if !matches!(lookup, pr::Lookup::Position(0)) {
                        return Err(tuple::error_no_field(base_ty, &lookup)
                            .with_span(span)
                            .push_hint(format!(
                                "{} is a framed type. Inner value can be accessed with `.0`",
                                printer::print_ty(base_ty)
                            )));
                    }
                    let mut r = *base;
                    r.ty = Some(ty.clone());
                    r.span = span;
                    return Ok(r);
                }

                // general case: resolve lookup
                let target_ty = self
                    .resolve_tuple_lookup(base_ty, &lookup, span.unwrap())
                    .with_span(span)?;

                let kind = pr::ExprKind::Lookup { base, lookup };
                pr::Expr {
                    ty: Some(target_ty),
                    kind,
                    ..node
                }
            }

            pr::ExprKind::Variant(mut variant) => {
                let inner_ty = if let Some(inner) = variant.inner {
                    // inner specified -> this is a enum variant

                    // resolve inner
                    let inner = self.fold_expr(*inner)?;
                    let inner_ty = inner.ty.clone().unwrap();
                    variant.inner = Some(Box::new(inner));

                    inner_ty
                } else {
                    pr::Ty::new(pr::TyKind::Tuple(vec![]))
                };

                // new ty var for enum
                let enum_domain = pr::TyDomain::EnumVariants(vec![pr::TyDomainEnumVariant {
                    name: variant.name.clone(),
                    ty: inner_ty,
                }]);
                let ty = self.introduce_ty_var(enum_domain, span.unwrap());

                pr::Expr {
                    ty: Some(ty),
                    ..pr::Expr::new(variant)
                }
            }

            pr::ExprKind::Call(pr::Call { subject, args }) => {
                // fold function name
                let subject = Box::new(self.fold_expr(*subject)?);

                let subject_target = subject.target.as_ref().map(|ref_| {
                    // SAFETY: this was resolved already in fold_expr, ExprKind::Ident
                    self.get_ref(ref_).unwrap()
                });
                let is_framing = matches!(subject_target, Some(scope::Named::Ty { .. }));

                let resolved = self.resolve_func_call(subject, args, span)?;

                if is_framing {
                    // special case: framed type constructor
                    // just unwrap the func call and use the first arg
                    let call = resolved.kind.into_call().unwrap();
                    let arg = call.args.into_iter().next();

                    let mut inner = arg.map(|a| a.expr).unwrap_or_else(|| {
                        // no arg: this has emitted an error before, just try to recover
                        pr::Expr::new(pr::ExprKind::Tuple(vec![]))
                    });
                    inner.ty = resolved.ty;
                    inner
                } else {
                    // general case
                    resolved
                }
            }

            pr::ExprKind::Func(func) => {
                let func = self
                    .resolve_func(node.scope_id.unwrap(), func)
                    .with_span_fallback(span)?;
                pr::Expr {
                    kind: pr::ExprKind::Func(func),
                    ..node
                }
            }

            pr::ExprKind::TypeAnnotation(ann) => {
                let ty = self.fold_type(*ann.ty)?;
                let mut expr = self.fold_expr(*ann.expr)?;

                self.validate_expr_type(&mut expr, &ty, &|| None)?;

                // return inner expr, with ty set to the explicit type
                // (this removes ExprKind::TypeAnnotation)
                // Using the explicit type matters when inferred type is a ty var
                // and user wants to help the compiler.
                let mut ty = ty;
                expr.span = span;
                ty.span = span;
                return Ok(pr::Expr {
                    ty: Some(ty),
                    ..expr
                });
            }

            pr::ExprKind::Match(_) => self.resolve_match(node)?,

            pr::ExprKind::If(_) => self.resolve_if(node)?,

            pr::ExprKind::Tuple(_) => self.resolve_tuple_constructor(node)?,

            pr::ExprKind::VarBinding(binding) => {
                let bound = Box::new(self.fold_expr(*binding.bound)?);
                let bound_ty = bound.ty.as_ref().unwrap();

                // populate scope
                let mut scope = scope::Scope::new(node.scope_id.unwrap(), scope::ScopeKind::Nested);
                scope.insert_local(bound_ty.clone());
                self.scopes.push(scope);

                // fold main
                let main = self.fold_expr(*binding.main)?;

                let mapping = self.finalize_type_vars()?;
                let main = utils::TypeReplacer::on_expr(main, mapping);
                self.scopes.pop().unwrap();

                pr::Expr {
                    ty: main.ty.clone(),
                    kind: pr::ExprKind::VarBinding(pr::VarBinding {
                        name: binding.name,
                        bound,
                        main: Box::new(main),
                    }),
                    ..node
                }
            }

            item => pr::Expr {
                kind: fold::fold_expr_kind(self, item)?,
                ..node
            },
        };

        let mut r = r;
        r.span = r.span.or(span);
        if r.ty.is_none() {
            r.ty = Some(self.infer_type(&r)?);
        }
        if let Some(scope_id) = r.scope_id {
            // make ty infer scope_id of expr
            r.ty.as_mut().unwrap().scope_id = Some(scope_id);
        }
        Ok(r)
    }

    fn fold_type(&mut self, ty: pr::Ty) -> Result<pr::Ty> {
        let ty = match ty.kind {
            // introduce new ty vars for missing type annotations
            // (this is needed to find non-inferable params)
            pr::TyKind::Func(mut ty_func) => {
                for p in &mut ty_func.params {
                    if p.ty.is_none() {
                        p.ty = Some(self.introduce_ty_var(pr::TyDomain::Open, ty.span.unwrap()));
                    }
                }
                if ty_func.body.is_none() {
                    ty_func.body = Some(Box::new(
                        self.introduce_ty_var(pr::TyDomain::Open, ty.span.unwrap()),
                    ));
                }

                pr::Ty {
                    kind: pr::TyKind::Func(ty_func),
                    ..ty
                }
            }

            // normal fold
            _ => fold::fold_type(self, ty)?,
        };
        Ok(ty)
    }

    fn fold_pattern(&mut self, _pattern: pr::Pattern) -> Result<pr::Pattern> {
        unreachable!()
    }
}

impl TypeResolver<'_> {
    fn resolve_if(&mut self, node: pr::Expr) -> Result<pr::Expr> {
        let pr::ExprKind::If(if_else) = node.kind else {
            unreachable!()
        };

        let bool = pr::Ty::new(pr::TyPrimitive::bool);
        let mut condition = Box::new(self.fold_expr_or_recover(*if_else.condition, &bool));
        self.validate_expr_type(&mut condition, &bool, &|| Some("if".into()))
            .unwrap_or_else(self.push_diagnostic());

        let then = Box::new(self.fold_expr(*if_else.then)?);
        let ty = then.ty.clone().unwrap();

        let mut els = Box::new(self.fold_expr(*if_else.els)?);
        self.validate_expr_type(&mut els, &ty, &|| None)
            .unwrap_or_else(self.push_diagnostic());

        Ok(pr::Expr {
            kind: pr::ExprKind::If(pr::If {
                condition,
                then,
                els,
            }),
            ty: Some(ty),
            ..node
        })
    }
}
