use crate::Result;
use crate::diagnostic::{Diagnostic, DiagnosticCode, WithErrorInfo};
use crate::resolver::NS_STD;
use crate::resolver::types::functions::FuncMetadata;
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
        self.resolve_root_anno();
        Ok(())
    }

    /// fq_ident must point to an unresolved definition.
    #[tracing::instrument(name = "types", skip_all, fields(def = fq_ident.to_string()))]
    fn resolve_def(&mut self, fq_ident: &pr::Path) -> Result<()> {
        if !fq_ident.starts_with_part(NS_STD) {
            tracing::debug!("resolving def {fq_ident}");
        }

        self.debug_current_def = fq_ident.clone();

        // take def out of the module (replace with a placeholder)
        let def_kind = {
            let def = self.root_mod.get_mut(fq_ident).unwrap();
            std::mem::replace(&mut def.kind, pr::DefKind::dummy())
        };

        // resolve
        let def_kind = self.resolve_unresolved(fq_ident, def_kind)?;

        // put def back in
        let def = self.root_mod.get_mut(fq_ident).unwrap();
        def.kind = def_kind;
        let annotations = std::mem::take(&mut def.annotations);

        // validate annotations
        let annotations = (annotations.into_iter())
            .map(|a| self.resolve_anno(a))
            .collect();

        // put annotations back in
        let def = self.root_mod.get_mut(fq_ident).unwrap();
        def.annotations = annotations;
        Ok(())
    }

    /// Validates a single annotation expression.
    /// Returns the annotation unchanged (validation is side-effect only via diagnostics).
    fn resolve_anno(&mut self, mut ann: pr::Anno) -> pr::Anno {
        // Extract the (target, args)
        let args = ann.get_args_mut().map(std::mem::take).unwrap_or_default();
        let target = match &mut ann.expr.kind {
            pr::ExprKind::Ident(_) => ann.expr.target.clone(),
            pr::ExprKind::Call(call) => call.subject.target.clone(),
            _ => {
                let d = Diagnostic::new_custom("invalid annotation").with_span(ann.expr.span);
                self.diagnostics.push(d);
                return ann;
            }
        };

        // Find target def
        let Some(pr::Ref::Global(target_fq)) = target else {
            // shouldn't happen after name resolution
            unreachable!()
        };
        let Some(def) = self.root_mod.get(&target_fq) else {
            // shouldn't happen after name resolution
            unreachable!()
        };
        let pr::DefKind::Anno(ann_def) = &def.kind else {
            self.diagnostics.push(
                Diagnostic::new("expected annotation", DiagnosticCode::NAME_KIND)
                    .with_span(ann.expr.span),
            );
            return ann;
        };
        let ann_def = ann_def.clone(); // clone to release self.root_mod
        let metadata = FuncMetadata::default();

        let scope = scope::Scope::new(usize::MAX, scope::ScopeKind::Isolated);
        self.scopes.push(scope);

        // resolve
        let args = self.resolve_call_args(args, &ann_def.params, metadata, ann.expr.span);

        let _ = self.finalize_type_vars();
        self.scopes.pop();

        // place resolved args back
        if let Some(a) = ann.get_args_mut() {
            *a = args;
        }
        ann
    }

    fn resolve_root_anno(&mut self) {
        let annotations = std::mem::take(&mut self.root_mod.annotations);

        let annotations = annotations
            .into_iter()
            .map(|a| self.resolve_anno(a))
            .collect();

        self.root_mod.annotations = annotations;
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
                    value.ty = Some(Box::new(expected_ty.clone()));
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
                    framed_label: ty_def.framed_label,
                })
            }
            pr::DefKind::Import(target) => pr::DefKind::Import(target),
            pr::DefKind::Anno(ann_def) => {
                let mut ann_def = utils::fold::fold_anno_def(self, ann_def)?;
                for param in &mut ann_def.params {
                    param.constant = true;
                }
                pr::DefKind::Anno(ann_def)
            }
        })
    }
}
