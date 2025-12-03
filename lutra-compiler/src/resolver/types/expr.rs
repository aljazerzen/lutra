use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::resolver::names;
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
                        is_nominal: false, ..
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
                    scope::Named::EnumVariant(ty, ty_fq, tag) => {
                        let ty = if let Some(ty_fq) = ty_fq {
                            // when possible, use ident as the type
                            pr::Ty {
                                target: Some(pr::Ref::Global(pr::AbsoluteRef::new(ty_fq))),
                                span,
                                ..pr::Ty::new(pr::TyKind::Ident(ident))
                            }
                        } else {
                            // fallback to concrete enum type
                            ty.clone()
                        };

                        let r = pr::Expr {
                            span,
                            ty: Some(ty),
                            ..pr::Expr::new(pr::ExprKind::EnumVariant(pr::EnumVariant {
                                tag,
                                inner: None,
                            }))
                        };
                        return Ok(r);
                    }
                    scope::Named::Ty {
                        is_nominal: true,
                        ty,
                    } => {
                        // nominal types can be called like a function to act as a constructor

                        let ty_nominal = pr::Ty {
                            target: Some(target.clone()),
                            ..pr::Ty::new(ident.clone())
                        };
                        let ty_framed = ty.clone();

                        return Ok(pr::Expr {
                            kind: pr::ExprKind::Ident(ident),
                            ty: Some(pr::Ty::new(pr::TyFunc {
                                params: vec![(Some(ty_framed), false)],
                                body: Some(Box::new(ty_nominal)),
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

            pr::ExprKind::TupleLookup { base, lookup } => {
                let base = Box::new(self.fold_expr(*base)?);
                let base_ty = base.ty.as_ref().unwrap();

                // special case: lookups into nominal types
                let base_ty_target = base_ty.target.as_ref().map(|r| self.get_ref(r).unwrap());
                if let Some(scope::Named::Ty {
                    ty,
                    is_nominal: true,
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

                let kind = pr::ExprKind::TupleLookup { base, lookup };
                pr::Expr {
                    ty: Some(target_ty),
                    kind,
                    ..node
                }

                // tuple::BaseKind::Array => {
                //     let std_index = pr::Path::new(vec![NS_STD, "index"]);
                //     let mut std_index_expr = pr::Expr::new(std_index.clone());
                //     std_index_expr.span = span;
                //     std_index_expr.target = Some(pr::Ref::FullyQualified {
                //         to_def: std_index,
                //         within: pr::Path::empty(),
                //     });

                //     let position = field.into_position().unwrap();
                //     let mut position = pr::Expr::new(pr::Literal::Integer(position));
                //     position.span = span;

                //     let func = Box::new(self.fold_expr(std_index_expr)?);

                //     self.resolve_func_call(func, vec![base, position], span)?
                // }
            }

            pr::ExprKind::FuncCall(pr::FuncCall { func, args }) => {
                // fold function name
                let func = Box::new(self.fold_expr(*func)?);

                let func_target = func.target.as_ref().map(|ref_| {
                    // SAFETY: this was resolved already in fold_expr, ExprKind::Ident
                    self.get_ref(ref_).unwrap()
                });
                let is_nominal_constructor = matches!(func_target, Some(scope::Named::Ty { .. }));

                let resolved = if let pr::ExprKind::EnumVariant(mut variant) = func.kind {
                    // special case: enum variant construction
                    let ty = func.ty.as_ref().unwrap();

                    if args.len() != 1 {
                        return Err(Diagnostic::new_custom(format!(
                            "{}expected exactly one argument, but got {}",
                            ty.name.clone().unwrap_or_default(),
                            args.len(),
                        ))
                        .with_span(span));
                    }

                    let inner = args.into_iter().next().unwrap();
                    let mut inner = self.fold_expr(inner)?;

                    // validate type of inner
                    let ty_enum = self.get_ty_mat(ty).unwrap();
                    let scope::TyRef::Ty(ty_enum) = ty_enum else {
                        panic!()
                    };
                    let ty_variants = ty_enum.kind.as_enum().unwrap();
                    let ty_variant = ty_variants.get(variant.tag).unwrap().clone();
                    self.validate_expr_type(&mut inner, &ty_variant.ty, &|| {
                        Some(ty_variant.name.clone())
                    })?;

                    variant.inner = Some(Box::new(inner));

                    pr::Expr {
                        kind: pr::ExprKind::EnumVariant(variant),
                        ..*func
                    }
                } else {
                    // general case
                    self.resolve_func_call(func, args, span)?
                };

                if is_nominal_constructor {
                    // special case: nominal type constructor
                    // just unwrap the func call and use the first arg
                    let call = resolved.kind.into_func_call().unwrap();
                    let mut inner = call.args.into_iter().next().unwrap();
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
                for (p, _) in &mut ty_func.params {
                    if p.is_none() {
                        *p = Some(self.introduce_ty_var(pr::TyParamDomain::Open, ty.span.unwrap()))
                    }
                }
                if ty_func.body.is_none() {
                    ty_func.body = Some(Box::new(
                        self.introduce_ty_var(pr::TyParamDomain::Open, ty.span.unwrap()),
                    ));
                }

                pr::Ty {
                    kind: pr::TyKind::Func(ty_func),
                    ..ty
                }
            }

            // inline idents into types
            pr::TyKind::Ident(_) => {
                if let Some(pr::Ref::Global(pr::AbsoluteRef { to_def, within })) = &ty.target
                    && !within.is_empty()
                {
                    // Things like `my_tuple::field` are "references into types".
                    // They are needed for constructing enums and are useful in general.
                    // But they are inconvenient to work with in IR, because in addition
                    // to the code that finds the def, we need code to look into the def
                    // as well/
                    // So instead, we inline these references during resolving.
                    // This is possible, because they are restricted to be non-recursive.
                    tracing::debug!("inlining a 'path into type' for: {to_def:?}.{within:?}");

                    let def = self.root_mod.get(to_def).unwrap();
                    let def = def.kind.as_ty().unwrap();
                    let referenced = names::lookup_in_ty(&def.ty, within.as_steps()).unwrap();

                    return self.fold_type(referenced.clone());
                }
                ty
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
