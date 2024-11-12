use itertools::Itertools;

use crate::decl::DeclKind;
use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::pr;
use crate::pr::Ty;
use crate::semantic::{NS_STD, NS_THIS};
use crate::utils::fold::{self, PrFold};
use crate::{Result, Span};

use super::tuple::StepOwned;

impl fold::PrFold for super::Resolver<'_> {
    fn fold_stmts(&mut self, _: Vec<pr::Stmt>) -> Result<Vec<pr::Stmt>> {
        unreachable!()
    }

    fn fold_type(&mut self, ty: Ty) -> Result<Ty> {
        self.fold_type_actual(ty)
    }

    fn fold_var_def(&mut self, var_def: pr::VarDef) -> Result<pr::VarDef> {
        Ok(pr::VarDef {
            name: var_def.name,
            kind: pr::VarDefKind::Let,
            value: match var_def.value {
                Some(value) => Some(Box::new(self.fold_expr(*value)?)),
                None => None,
            },
            ty: var_def.ty.map(|x| self.fold_type(x)).transpose()?,
        })
    }

    fn fold_expr(&mut self, node: pr::Expr) -> Result<pr::Expr> {
        if node.id.is_some() && !matches!(node.kind, pr::ExprKind::Func(_)) {
            return Ok(node);
        }

        let id = self.id.gen();
        let alias = Box::new(node.alias.clone());
        let span = Box::new(node.span);

        log::trace!("folding expr {node:?}");

        let r = match node.kind {
            pr::ExprKind::Ident(ident) => {
                log::debug!("resolving ident {ident:?}...");

                let indirections = vec![];

                let mut expr = {
                    let decl = self.get_ident(&ident).unwrap();

                    let log_debug = !ident.starts_with_part(NS_STD);
                    if log_debug {
                        log::debug!("... resolved to {decl:?}");
                    }

                    match &decl.kind {
                        DeclKind::Expr(expr) => {
                            // keep as ident, but pull in the type
                            let ty = expr.ty.clone().unwrap();

                            // if the type contains generics, we need to instantiate those
                            // generics into current function scope
                            // let ty = self.instantiate_type(ty, id);

                            pr::Expr {
                                kind: pr::ExprKind::Ident(ident),
                                ty: Some(ty),
                                ..node
                            }
                        }

                        DeclKind::Ty(_) => {
                            return Err(Diagnostic::new_custom(
                                "expected a value, but found a type",
                            )
                            .with_span(*span));
                        }

                        DeclKind::Unresolved(_) => {
                            return Err(Diagnostic::new_assert(format!(
                                "bad resolution order: unresolved {ident} while resolving {}",
                                self.debug_current_decl
                            )));
                        }

                        _ => pr::Expr {
                            kind: pr::ExprKind::Ident(ident),
                            ..node
                        },
                    }
                };

                expr.id = expr.id.or(Some(id));
                // let flatten = expr.flatten;
                // expr.flatten = false;
                let alias = expr.alias.take();

                let mut expr = self.apply_indirections(expr, indirections);

                // expr.flatten = flatten;
                expr.alias = alias;
                expr
            }

            pr::ExprKind::Indirection { base, field } => {
                let base = self.fold_expr(*base)?;

                let ty = base.ty.as_ref().unwrap();

                let steps = self.resolve_indirection(ty, &field).with_span(*span)?;

                let expr = self.apply_indirections(base, steps);
                pr::Expr {
                    id: expr.id,
                    kind: expr.kind,
                    ty: expr.ty,
                    ..node
                }
            }

            pr::ExprKind::FuncCall(pr::FuncCall { name, args, .. })
                if (name.kind.as_ident()).map_or(false, |i| i.to_string() == "std.not")
                    && matches!(args[0].kind, pr::ExprKind::Tuple(_)) =>
            {
                let arg = args.into_iter().exactly_one().unwrap();
                self.resolve_column_exclusion(arg)?
            }

            pr::ExprKind::FuncCall(pr::FuncCall {
                name,
                args,
                named_args: _,
            }) => {
                // fold function name
                let old = self.in_func_call_name;
                self.in_func_call_name = true;
                let func = Box::new(self.fold_expr(*name)?);
                self.in_func_call_name = old;

                self.resolve_func_application(func, args, *span)?
            }

            pr::ExprKind::Func(func) => {
                let func = self.resolve_func(func)?;
                pr::Expr {
                    kind: pr::ExprKind::Func(func),
                    ..node
                }
            }

            pr::ExprKind::Tuple(exprs) => {
                let exprs = self.fold_exprs(exprs)?;

                // flatten
                let exprs = exprs.into_iter().flat_map(|e| vec![e]).collect_vec();

                pr::Expr {
                    kind: pr::ExprKind::Tuple(exprs),
                    ..node
                }
            }

            item => pr::Expr {
                kind: fold::fold_expr_kind(self, item)?,
                ..node
            },
        };
        self.finish_expr_resolve(r, id, *alias, *span)
    }
}

impl super::Resolver<'_> {
    fn finish_expr_resolve(
        &mut self,
        expr: pr::Expr,
        id: usize,
        alias: Option<String>,
        span: Option<Span>,
    ) -> Result<pr::Expr> {
        let mut r = Box::new(expr);

        r.id = r.id.or(Some(id));
        r.alias = r.alias.or(alias);
        r.span = r.span.or(span);

        if r.ty.is_none() {
            r.ty = Some(self.infer_type(&r)?);
        }
        if let Some(ty) = &mut r.ty {
            if ty.is_relation() {
                if let Some(alias) = r.alias.take() {
                    // This is relation wrapping operation.
                    // Convert:
                    //     alias = r
                    // into:
                    //     _local.select {alias = _local.this} r

                    let expr = pr::Expr::new(pr::ExprKind::FuncCall(pr::FuncCall {
                        name: Box::new(pr::Expr::new(pr::ExprKind::Ident(pr::Path::new(vec![
                            NS_STD, "select",
                        ])))),
                        args: vec![
                            pr::Expr::new(pr::ExprKind::Tuple(vec![pr::Expr {
                                alias: Some(alias),
                                ..pr::Expr::new(pr::Path::new(vec![NS_THIS]))
                            }])),
                            *r,
                        ],
                        named_args: Default::default(),
                    }));
                    return self.fold_expr(expr);
                }
            }
        }

        Ok(*r)
    }

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
    pub fn resolve_indirection(
        &mut self,
        base: &Ty,
        indirection: &pr::IndirectionKind,
    ) -> Result<Vec<StepOwned>> {
        match indirection {
            pr::IndirectionKind::Name(name) => {
                self.lookup_name_in_tuple(base, name).and_then(|res| {
                    res.ok_or_else(|| Diagnostic::new_custom(format!("Unknown name {name}")))
                })
            }
            pr::IndirectionKind::Position(pos) => {
                let step = super::tuple::lookup_position_in_tuple(base, *pos as usize)?
                    .ok_or_else(|| Diagnostic::new_custom("Out of bounds"))?;

                Ok(vec![step])
            }
            pr::IndirectionKind::Star => todo!(),
        }
    }
}
