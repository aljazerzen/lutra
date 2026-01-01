use crate::Result;
use crate::diagnostic::{Diagnostic, WithErrorInfo};
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
    #[tracing::instrument(name = "types", skip_all, fields(def = fq_ident.to_string()))]
    fn resolve_def(&mut self, fq_ident: &pr::Path) -> Result<()> {
        if !fq_ident.starts_with_part(NS_STD) {
            tracing::debug!("resolving def {fq_ident}");
        }

        self.debug_current_def = fq_ident.clone();

        // take def out of the module
        let def = self.root_mod.get_mut(fq_ident).unwrap();
        let pr::DefKind::Unresolved(def) = &mut def.kind else {
            unreachable!("resolving already resolved: {fq_ident}")
        };
        let def_kind = def.take().unwrap();

        // resolve
        let def_kind = self.resolve_unresolved(fq_ident, *def_kind)?;

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
                // it should have been converted into Module in [crate::resolver::module::init_root]
            }
            pr::DefKind::Unresolved(_) => {
                unreachable!("nested unresolved?")
            }
            pr::DefKind::Expr(expr_def) => {
                // push a top-level scope for exprs that need inference type args but are not wrapped into a function
                let scope = scope::Scope::new(usize::MAX, scope::ScopeKind::Isolated);
                self.scopes.push(scope);

                // resolve
                self.allow_native_functions = true;
                let expr_def = self.fold_expr_def(expr_def)?;
                let expected_ty = expr_def.ty;

                tracing::trace!("def done");

                let mut value = expr_def.value;

                // validate type
                if let Some(expected_ty) = &expected_ty {
                    let who = || Some(fq_ident.last().to_string());
                    self.validate_expr_type(&mut value, expected_ty, &who)
                        .unwrap_or_else(self.push_diagnostic());
                    value.ty = Some(expected_ty.clone());
                }

                // finalize scope
                let mapping = self.finalize_type_vars()?;
                let value = utils::TypeReplacer::on_expr(*value, mapping);

                // validate const
                if expr_def.constant {
                    self.const_validator
                        .validate_is_const(&value)
                        .map_err(|span| {
                            Diagnostic::new_custom("non-constant expression")
                                .with_span(span.or(value.span))
                                .push_hint("use `func` instead of `const`")
                        })
                        .unwrap_or_else(self.push_diagnostic());
                    self.const_validator.save_const(fq_ident.clone());
                }
                self.scopes.pop().unwrap();

                pr::DefKind::Expr(pr::ExprDef {
                    value: Box::new(value),
                    ty: None,
                    constant: expr_def.constant,
                })
            }
            pr::DefKind::Ty(ty_def) => {
                let mut ty = self.fold_type(ty_def.ty)?;
                ty.name = Some(fq_ident.last().to_string());
                pr::DefKind::Ty(pr::TyDef {
                    ty,
                    is_framed: ty_def.is_framed,
                })
            }
            pr::DefKind::Import(target) => pr::DefKind::Import(target),
        })
    }
}
