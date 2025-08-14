use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::{pr, utils};

use crate::Result;
use crate::resolver::types::scope::ScopeKind;
use crate::utils::fold::PrFold;

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
            pr::PatternKind::Enum(variant_name, inner) => {
                let subject_ty_mat = self.get_ty_mat(subject_ty)?;

                let (tag, variant_ty) = match &subject_ty_mat {
                    // this a concrete type
                    TyRef::Ty(t) => {
                        // has to be an enum
                        let pr::TyKind::Enum(variants) = &t.kind else {
                            return Err(Diagnostic::new_custom("expected an enum")
                                .with_span(Some(pattern.span)));
                        };

                        // find variant by name
                        let (tag, variant) = lookup_variant(variants, &variant_name)
                            .with_span(Some(pattern.span))?;

                        (Some(tag), variant.ty.clone())
                    }

                    // invalid, we don't support syntax for enums param domains
                    TyRef::Param(_) => {
                        return Err(Diagnostic::new_custom("expected an enum")
                            .push_hint("found type parameter, which might not be an enum")
                            .with_span(Some(pattern.span)));
                    }

                    // we must infer that this var is an enum with this variant
                    TyRef::Var(_, id) => {
                        let variant_ty = if inner.is_some() {
                            // introduce a new type var for the type of this variant
                            self.introduce_ty_var(pr::TyParamDomain::Open, pattern.span)
                        } else {
                            // there is no inner pattern, it must be a unit type
                            pr::Ty::new(pr::TyKind::Tuple(vec![]))
                        };

                        // restrict existing ty var
                        let restriction =
                            pr::TyParamDomain::EnumVariants(vec![pr::TyDomainEnumVariant {
                                name: variant_name.clone(),
                                ty: variant_ty.clone(),
                            }]);
                        let scope = self.get_ty_var_scope();
                        scope.infer_type_var_in_domain(*id, restriction);

                        (None, variant_ty)
                    }
                };

                // inner
                let inner = if let Some(inner) = inner {
                    let inner = self.resolve_pattern(&variant_ty, *inner)?;
                    Some(Box::new(inner))
                } else {
                    None
                };

                Ok(pr::Pattern {
                    kind: pr::PatternKind::Enum(variant_name, inner),
                    variant_tag: tag,
                    ..pattern
                })
            }

            pr::PatternKind::Bind(_) => {
                let scope = self.scopes.last_mut().unwrap();
                scope.insert_local(subject_ty.clone());

                Ok(pattern)
            }

            pr::PatternKind::Literal(l) => {
                let found_ty = self.infer_type_of_literal(&l, Some(pattern.span));

                self.validate_type(&found_ty, subject_ty, &|| Some("match".into()))
                    .unwrap_or_else(self.push_diagnostic());

                Ok(pr::Pattern {
                    kind: pr::PatternKind::Literal(l),
                    ..pattern
                })
            }
        }
    }
}

pub fn lookup_variant<'a>(
    variants: &'a [pr::TyEnumVariant],
    variant_name: &str,
) -> Result<(usize, &'a pr::TyEnumVariant), Diagnostic> {
    variants
        .iter()
        .enumerate()
        .find(|(_, v)| v.name == variant_name)
        .ok_or_else(|| Diagnostic::new_custom("variant does not exist"))
}

pub fn lookup_variant_in_domain<'a>(
    variants: &'a [pr::TyDomainEnumVariant],
    variant_name: &str,
) -> Result<(usize, &'a pr::TyDomainEnumVariant), Diagnostic> {
    variants
        .iter()
        .enumerate()
        .find(|(_, v)| v.name == variant_name)
        .ok_or_else(|| Diagnostic::new_custom("variant does not exist"))
}
