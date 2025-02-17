use itertools::Itertools;

use crate::decl::DeclKind;
use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::pr;
use crate::pr::Ty;
use crate::semantic::resolver::scope::{Named, ScopedKind};
use crate::semantic::NS_STD;
use crate::utils::fold::{self, PrFold};
use crate::Result;

use super::scope;
use super::tuple::Step;

impl fold::PrFold for super::Resolver<'_> {
    fn fold_stmts(&mut self, _: Vec<pr::Stmt>) -> Result<Vec<pr::Stmt>> {
        unreachable!()
    }

    fn fold_type(&mut self, ty: Ty) -> Result<Ty> {
        self.fold_type_actual(ty)
    }

    fn fold_expr(&mut self, node: pr::Expr) -> Result<pr::Expr> {
        let span = node.span;

        log::trace!("folding expr {node:?}");

        let r = match node.kind {
            pr::ExprKind::Ident(ident) => {
                log::debug!("resolving ident {ident:?}...");

                let named = self.get_ident(&ident).unwrap();

                let log_debug = !ident.starts_with_part(NS_STD);
                if log_debug {
                    log::debug!("... resolved to {}", named.as_ref());
                }

                let ty = match named {
                    Named::Decl(decl) => match &decl.kind {
                        DeclKind::Expr(expr) => {
                            // if the type contains generics, we need to instantiate those
                            // generics into current function scope
                            // let ty = self.instantiate_type(ty, id);
                            expr.ty.clone().unwrap()
                        }

                        DeclKind::Ty(_) => {
                            return Err(Diagnostic::new_custom(
                                "expected a value, but found a type",
                            )
                            .with_span(span))
                        }

                        DeclKind::Unresolved(_) => {
                            return Err(Diagnostic::new_assert(format!(
                                "bad resolution order: unresolved {ident} while resolving {}",
                                self.debug_current_decl
                            )))
                        }

                        DeclKind::Module(_) | DeclKind::Import(_) => {
                            // handled during name resolution
                            unreachable!()
                        }
                    },
                    Named::Scoped(scoped) => match scoped {
                        ScopedKind::Param { ty } => ty.clone(),
                        ScopedKind::Type { .. }
                        | ScopedKind::TypeParam { .. }
                        | ScopedKind::TypeArg { .. } => {
                            return Err(Diagnostic::new_custom(
                                "expected a value, but found a type",
                            )
                            .with_span(span))
                        }
                    },
                };
                let ty = self.introduce_ty_into_scope(ty);
                pr::Expr {
                    kind: pr::ExprKind::Ident(ident),
                    ty: Some(ty),
                    ..node
                }
            }

            pr::ExprKind::Indirection { base, field } => {
                let base = self.fold_expr(*base)?;

                let base_ty = base.ty.as_ref().unwrap();
                let base_ty = self.resolve_ty_ident(base_ty)?;
                let base_ty = match base_ty {
                    scope::TyRef::Ty(b) => b,
                    scope::TyRef::Param(_) => todo!("tuple indirection into generic type Param"),
                    scope::TyRef::Arg(id) => {
                        todo!("tuple indirection into generic type Arg: {id:?}")
                    }
                };

                let step = self.resolve_indirection(&base_ty, &field).with_span(span)?;
                let position = step.position;
                let target_ty = Some(step.target_ty);

                if base_ty.kind.is_tuple() {
                    // tuples
                    let kind = pr::ExprKind::Indirection {
                        base: Box::new(base),
                        field: pr::IndirectionKind::Position(position as i64),
                    };
                    pr::Expr {
                        ty: target_ty,
                        kind,
                        ..node
                    }
                } else {
                    // arrays
                    let std_index = pr::Expr::new(pr::Path::new(vec!["std", "index"]));
                    let position = pr::Expr::new(pr::Literal::Integer(position as i64));

                    let func = Box::new(self.fold_expr(std_index)?);

                    self.resolve_func_call(func, vec![base, position], span)?
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

            pr::ExprKind::FuncCall(pr::FuncCall { func: name, args }) => {
                // fold function name
                let func = Box::new(self.fold_expr(*name)?);

                self.resolve_func_call(func, args, span)?
            }

            pr::ExprKind::Func(func) => {
                let func = self.resolve_func(func).with_span_fallback(span)?;
                pr::Expr {
                    kind: pr::ExprKind::Func(func),
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
        Ok(r)
    }
}

impl super::Resolver<'_> {
    pub fn resolve_column_exclusion(&mut self, expr: pr::Expr) -> Result<pr::Expr> {
        let expr = self.fold_expr(expr)?;
        let _except = self.coerce_into_tuple(expr)?;

        todo!()
        // self.fold_expr(pr::Expr::new(pr::ExprKind::All {
        //     within: Box::new(pr::Expr::new(pr::Path::from_path(vec![NS_THIS]))),
        //     except: Box::new(except),
        // }))
    }

    /// Resolve tuple indirections.
    /// For example, `base.indirection` where `base` has a tuple type.
    ///
    /// Returns the position of the tuple field within the base tuple.
    pub fn resolve_indirection<'a>(
        &'a self,
        base: &'a Ty,
        indirection: &pr::IndirectionKind,
    ) -> Result<Step> {
        match &base.kind {
            pr::TyKind::Ident(_) => panic!(),

            pr::TyKind::Tuple(fields) => match indirection {
                pr::IndirectionKind::Name(name) => {
                    self.lookup_name_in_tuple(fields, name).and_then(|res| {
                        res.ok_or_else(|| Diagnostic::new_custom(format!("Unknown name {name}")))
                    })
                }
                pr::IndirectionKind::Position(pos) => {
                    let step = super::tuple::lookup_position_in_tuple(base, *pos as usize)?
                        .ok_or_else(|| Diagnostic::new_custom("Out of bounds"))?;

                    Ok(step)
                }
                pr::IndirectionKind::Star => todo!(),
            },

            pr::TyKind::Array(items_ty) => match indirection {
                pr::IndirectionKind::Name(_) => {
                    Err(Diagnostic::new_custom("cannot lookup array items by name")
                        .with_span(base.span))
                }
                pr::IndirectionKind::Position(pos) => Ok(Step {
                    position: *pos as usize,
                    target_ty: *items_ty.clone(),
                }),
                pr::IndirectionKind::Star => todo!(),
            },

            pr::TyKind::Primitive(_) | pr::TyKind::Enum(_) | pr::TyKind::Function(_) => {
                Err(Diagnostic::new_custom(format!(
                    "expected a struct or array, found {}",
                    base.kind.as_ref()
                ))
                .with_span(base.span))
            }
        }
    }
}
