use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::{pr, utils};

use crate::resolver::types::scope::{Named, ScopeKind};
use crate::utils::fold::PrFold;
use crate::Result;

use super::scope::{Scope, TyRef};

impl<'a> super::TypeResolver<'a> {
    pub fn resolve_match(&mut self, expr: pr::Expr) -> Result<pr::Expr> {
        let pr::ExprKind::Match(match_) = expr.kind else {
            panic!()
        };

        // subject
        let subject = Box::new(self.fold_expr(*match_.subject)?);
        let subject_ty = subject.ty.as_ref().unwrap();

        // branches
        let mut branches = Vec::with_capacity(match_.branches.len());
        // type that will be inferred from the first branch and then validated against the following
        let mut ty = None;
        for branch in match_.branches {
            self.scopes.push(Scope::new(
                branch.value.scope_id.unwrap(),
                ScopeKind::Nested,
            ));

            // fold pattern (this will populate the scope)
            let pattern = self.resolve_pattern(subject_ty, branch.pattern)?;

            // fold value
            let value = self.fold_expr(*branch.value)?;

            let mapping = self.finalize_type_vars()?;
            let mut value = utils::TypeReplacer::on_expr(value, mapping);
            let scope = self.scopes.pop().unwrap();
            value.scope_id = Some(scope.id);

            match &ty {
                // first branch: infer its type
                None => {
                    ty = value.ty.clone();
                }
                // second and following branches: validate
                Some(ty) => {
                    self.validate_expr_type(&mut value, ty, &|| Some("match".into()))?;
                }
            }

            let value = Box::new(value);
            branches.push(pr::MatchBranch { pattern, value })
        }

        // Safety: there will always be at least one branch, so this will be set
        let ty = ty.unwrap();

        Ok(pr::Expr {
            kind: pr::ExprKind::Match(pr::Match { subject, branches }),
            ty: Some(ty),
            ..expr
        })
    }

    pub fn resolve_pattern(
        &mut self,
        subject_ty: &pr::Ty,
        pattern: pr::Pattern,
    ) -> Result<pr::Pattern> {
        match pattern.kind {
            pr::PatternKind::Enum(ident, inner) => {
                let target = pattern.target.unwrap();
                let named = self.get_ident(&target)?;

                let Named::EnumVariant(pattern_ty, tag) = named else {
                    return Err(Diagnostic::new_custom("expected an enum variant")
                        .with_span(Some(pattern.span)));
                };

                let pattern_ty = pattern_ty.clone();
                self.validate_type(&pattern_ty, subject_ty, &|| Some("pattern".into()))
                    .with_span(Some(pattern.span))?;

                let TyRef::Ty(subject_ty) = self.get_ty_mat(subject_ty)? else {
                    todo!();
                };
                let variants = subject_ty.kind.as_enum().unwrap();
                let variant_ty = variants.get(tag).unwrap().ty.clone();

                // inner
                let inner = if let Some(inner) = inner {
                    let inner = self.resolve_pattern(&variant_ty, *inner)?;
                    Some(Box::new(inner))
                } else {
                    None
                };

                Ok(pr::Pattern {
                    kind: pr::PatternKind::Enum(ident, inner),
                    target: None,
                    variant_tag: Some(tag),
                    ..pattern
                })
            }

            pr::PatternKind::Bind(_) => {
                let scope = self.scopes.last_mut().unwrap();
                scope.insert_local(subject_ty.clone());

                Ok(pattern)
            }
        }
    }
}
