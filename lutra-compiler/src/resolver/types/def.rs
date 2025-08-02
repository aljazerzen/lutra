use crate::Result;
use crate::resolver::NS_STD;
use crate::resolver::types::scope;
use crate::utils::fold::PrFold;
use crate::{pr, utils};

impl super::TypeResolver<'_> {
    /// Entry point to the resolver.
    pub fn resolve_defs(&mut self, order: &[Vec<pr::Path>]) -> Result<()> {
        for group in order {
            for fq_ident in group {
                self.resolve_def(fq_ident)?;
            }
        }
        Ok(())
    }

    /// fq_ident must point to an unresolved definition.
    #[tracing::instrument(name = "d", skip_all, fields(n = fq_ident.to_string()))]
    fn resolve_def(&mut self, fq_ident: &pr::Path) -> Result<()> {
        if !fq_ident.starts_with_part(NS_STD) {
            tracing::debug!("resolving def {fq_ident}");
        }

        self.debug_current_def = fq_ident.clone();

        // take def out of the module
        let def = self.root_mod.get_mut(fq_ident).unwrap();

        let def_kind = match &mut def.kind {
            // happens in first pass
            pr::DefKind::Unresolved(def) => {
                let def = def.take().unwrap();
                self.resolve_unresolved(fq_ident, *def)?
            }

            // happens in strict (second) pass
            pr::DefKind::Ty(ty) => {
                let ty_orig = ty.clone();

                pr::DefKind::Ty(pr::TyDef {
                    ty: self.fold_type(ty_orig.ty)?,
                })
            }

            pr::DefKind::Module(_) => unreachable!(),
            pr::DefKind::Expr(_) => unreachable!(),
            pr::DefKind::Import(_) => unreachable!(),
        };

        // put def back in
        let def = self.root_mod.get_mut(fq_ident).unwrap();
        def.kind = def_kind;
        Ok(())
    }

    pub fn resolve_unresolved(
        &mut self,
        fq_ident: &pr::Path,
        def: pr::DefKind,
    ) -> Result<pr::DefKind> {
        Ok(match def {
            pr::DefKind::Module(_) => {
                unreachable!("module def cannot be unresolved at this point")
                // it should have been converted into Module in resolve_defs::init_module_tree
            }
            pr::DefKind::Unresolved(_) => {
                unreachable!("nested unresolved?")
            }
            pr::DefKind::Expr(var_def) => {
                // push a top-level scope for exprs that need inference type args but are not wrapped into a function
                let scope = scope::Scope::new(usize::MAX, scope::ScopeKind::Isolated);
                self.scopes.push(scope);

                // resolve
                let def = self.fold_var_def(var_def)?;
                let expected_ty = def.ty;

                tracing::debug!("variable done");

                let def = match def.value {
                    Some(mut def_value) => {
                        // var value is provided

                        // validate type
                        if let Some(expected_ty) = &expected_ty {
                            let who = || Some(fq_ident.last().to_string());
                            self.validate_expr_type(&mut def_value, expected_ty, &who)?;
                        }

                        // finalize scope
                        let mapping = self.finalize_type_vars()?;
                        let def_value = utils::TypeReplacer::on_expr(*def_value, mapping);

                        pr::DefKind::Expr(pr::ExprDef {
                            value: Some(Box::new(def_value)),
                            ty: None,
                        })
                    }
                    None => {
                        // finalize scope
                        self.finalize_type_vars()?;

                        // var value is not provided: treat this var as value provided by the runtime
                        let mut expr = Box::new(pr::Expr::new(pr::ExprKind::Internal));
                        expr.ty = expected_ty;
                        pr::DefKind::Expr(pr::ExprDef {
                            value: Some(expr),
                            ty: None,
                        })
                    }
                };
                self.scopes.pop().unwrap();
                def
            }
            pr::DefKind::Ty(ty_def) => {
                let mut ty = self.fold_type(ty_def.ty)?;
                ty.name = Some(fq_ident.last().to_string());
                pr::DefKind::Ty(pr::TyDef { ty })
            }
            pr::DefKind::Import(target) => pr::DefKind::Import(target),
        })
    }
}
