use itertools::Itertools;

use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::pr;
use crate::pr::Ty;
use crate::resolver::types::scope::{Named, ScopedKind};
use crate::resolver::types::tuple::BaseKind;
use crate::utils::fold;
use crate::Result;

use super::scope::Scope;

impl fold::PrFold for super::TypeResolver<'_> {
    fn fold_stmts(&mut self, _: Vec<pr::Stmt>) -> Result<Vec<pr::Stmt>> {
        unreachable!()
    }

    #[tracing::instrument(name = "e", skip(self, node))]
    fn fold_expr(&mut self, node: pr::Expr) -> Result<pr::Expr> {
        tracing::debug!("{}", node.kind.as_ref());
        let span = node.span;

        let r = match node.kind {
            pr::ExprKind::Ident(ident) => {
                tracing::debug!("resolving ident {ident:?}...");

                let target = node.target.as_ref().unwrap();
                let named = self.get_ident(target).with_span(span)?;

                tracing::debug!("... resolved to {}", named.as_ref());

                let ty = match named {
                    Named::Expr(expr) => {
                        // if the type contains generics, we need to instantiate those
                        // generics into current function scope
                        // let ty = self.instantiate_type(ty, id);
                        expr.ty.clone().unwrap()
                    }

                    Named::Ty(_) => {
                        return Err(Diagnostic::new_custom("expected a value, but found a type")
                            .with_span(span))
                    }
                    Named::Scoped(scoped) => match scoped {
                        ScopedKind::Param { ty } => ty.clone(),
                        ScopedKind::TypeParam { .. } | ScopedKind::TypeArg { .. } => {
                            return Err(Diagnostic::new_custom(
                                "expected a value, but found a type",
                            )
                            .with_span(span))
                        }
                    },
                    Named::EnumVariant(ty, tag) => {
                        let mut r = pr::Expr::new(pr::ExprKind::EnumVariant(pr::EnumVariant {
                            tag,
                            inner: None,
                        }));
                        r.span = span;
                        r.ty = Some(ty.clone());
                        return Ok(r);
                    }
                };
                let (ty, ty_args) = self.introduce_ty_into_scope(ty);
                pr::Expr {
                    kind: pr::ExprKind::Ident(ident),
                    ty: Some(ty),
                    ty_args,
                    ..node
                }
            }

            pr::ExprKind::Indirection { base, field } => {
                let base = self.fold_expr(*base)?;
                let base_ty = base.ty.as_ref().unwrap();

                let indirection = self.resolve_indirection(base_ty, &field).with_span(span)?;
                match indirection.base {
                    BaseKind::Tuple => {
                        let kind = pr::ExprKind::Indirection {
                            base: Box::new(base),
                            field: pr::IndirectionKind::Position(indirection.position as i64),
                        };
                        pr::Expr {
                            ty: Some(indirection.target_ty),
                            kind,
                            ..node
                        }
                    }
                    BaseKind::Array => {
                        let std_index = pr::Path::new(vec!["std", "index"]);
                        let mut std_index_expr = pr::Expr::new(std_index.clone());
                        std_index_expr.target = Some(pr::Ref::FullyQualified(std_index));

                        let position =
                            pr::Expr::new(pr::Literal::Integer(indirection.position as i64));

                        let func = Box::new(self.fold_expr(std_index_expr)?);

                        self.resolve_func_call(func, vec![base, position], span)?
                    }
                }
            }

            pr::ExprKind::FuncCall(pr::FuncCall {
                func: name, args, ..
            }) if (name.kind.as_ident()).map_or(false, |i| i.to_string() == "std.not")
                && matches!(args[0].kind, pr::ExprKind::Tuple(_)) =>
            {
                let arg = args.into_iter().exactly_one().unwrap();
                self.resolve_column_exclusion(arg)?
            }

            pr::ExprKind::FuncCall(pr::FuncCall { func, args }) => {
                // fold function name
                let func = Box::new(self.fold_expr(*func)?);

                if let pr::ExprKind::EnumVariant(mut variant) = func.kind {
                    // special case: enum variant construction
                    let inner = args.into_iter().exactly_one().unwrap();
                    let inner = self.fold_expr(inner)?;
                    variant.inner = Some(Box::new(inner));

                    pr::Expr {
                        kind: pr::ExprKind::EnumVariant(variant),
                        ..*func
                    }
                } else {
                    // general case
                    self.resolve_func_call(func, args, span)?
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

                self.validate_expr_type(&mut expr, Some(&ty), &|| None)?;

                // return inner expr directly (without type annotation)
                return Ok(expr);
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

    fn fold_type(&mut self, ty: Ty) -> Result<Ty> {
        let ty = match ty.kind {
            // open a new scope for functions
            pr::TyKind::Func(ty_func) if self.scopes.is_empty() => {
                let mut scope = Scope::new(0);
                scope.insert_generics_params(&ty_func.ty_params);
                self.scopes.push(scope);
                let ty_func = fold::fold_ty_func(self, ty_func)?;
                self.scopes.pop();

                Ty {
                    kind: pr::TyKind::Func(ty_func),
                    ..ty
                }
            }

            // normal fold
            _ => fold::fold_type(self, ty)?,
        };
        Ok(ty)
    }
}
