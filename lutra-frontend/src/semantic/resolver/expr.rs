use itertools::Itertools;

use crate::decl::DeclKind;
use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::pr::Ty;
use crate::semantic::resolver::scope::{Named, ScopedKind};
use crate::semantic::resolver::tuple::BaseKind;
use crate::utils::fold::{self, PrFold};
use crate::Result;
use crate::{pr, printer};

use super::tuple::Indirection;
use super::{scope, tuple};

impl fold::PrFold for super::Resolver<'_> {
    fn fold_stmts(&mut self, _: Vec<pr::Stmt>) -> Result<Vec<pr::Stmt>> {
        unreachable!()
    }

    fn fold_type(&mut self, ty: Ty) -> Result<Ty> {
        self.fold_type_actual(ty)
    }

    #[tracing::instrument(name = "e", skip(self, node))]
    fn fold_expr(&mut self, node: pr::Expr) -> Result<pr::Expr> {
        tracing::debug!("{}", node.kind.as_ref());
        let span = node.span;

        let r = match node.kind {
            pr::ExprKind::Ident(ident) => {
                tracing::debug!("resolving ident {ident:?}...");

                let named = self.get_ident(&ident).unwrap();

                tracing::debug!("... resolved to {}", named.as_ref());

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
                        let std_index = pr::Expr::new(pr::Path::new(vec!["std", "index"]));
                        let position =
                            pr::Expr::new(pr::Literal::Integer(indirection.position as i64));

                        let func = Box::new(self.fold_expr(std_index)?);

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

    /// Resolve indirections (lookups).
    /// For example, `base.indirection` where `base` either has a tuple or array type.
    ///
    /// Returns a positional indirection into the base.
    pub fn resolve_indirection(
        &self,
        base: &Ty,
        indirection: &pr::IndirectionKind,
    ) -> Result<Indirection> {
        let base_ref = self.resolve_ty_ident(base.clone())?;

        let base = match base_ref {
            scope::TyRef::Ty(b) => b,
            scope::TyRef::Param(id) => {
                let param = self.get_ty_param(&id);
                return match param {
                    pr::TyParamDomain::Open | pr::TyParamDomain::OneOf(_) => {
                        Err(Diagnostic::new_custom(format!(
                            "expected a tuple or an array, found {}",
                            base.kind.as_ref()
                        )))
                    }
                    pr::TyParamDomain::TupleFields(fields) => {
                        tuple::lookup_in_domain(fields, indirection).ok_or_else(|| {
                            Diagnostic::new_custom(format!(
                                "Field {} does not exist in type {}",
                                tuple::print_indirection_kind(indirection),
                                printer::print_ty(base)
                            ))
                        })
                    }
                };
            }
            scope::TyRef::Arg(id) => {
                todo!("tuple indirection into generic type Arg: {id:?}")
            }
        };

        match &base.kind {
            pr::TyKind::Ident(_) => unreachable!(),

            pr::TyKind::Tuple(fields) => {
                tuple::lookup_in_tuple(fields, indirection).ok_or_else(|| {
                    Diagnostic::new_custom(format!(
                        "Field {} does not exist in type {}",
                        tuple::print_indirection_kind(indirection),
                        printer::print_ty(&base)
                    ))
                })
            }

            pr::TyKind::Array(items_ty) => match indirection {
                pr::IndirectionKind::Name(_) => {
                    Err(Diagnostic::new_custom("cannot lookup array items by name"))
                }
                pr::IndirectionKind::Position(pos) => Ok(Indirection {
                    base: BaseKind::Array,
                    position: *pos as usize,
                    target_ty: *items_ty.clone(),
                }),
                pr::IndirectionKind::Star => todo!(),
            },

            pr::TyKind::Primitive(_) | pr::TyKind::Enum(_) | pr::TyKind::Function(_) => {
                Err(Diagnostic::new_custom(format!(
                    "expected a tuple or an array, found {}",
                    base.kind.as_ref()
                )))
            }
        }
    }
}
