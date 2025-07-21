use std::collections::HashSet;

use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::resolver::module::ExprOrTy;
use crate::{Span, decl, pr};

pub(crate) fn run(root_module: &decl::RootModule) -> Result<(), Vec<Diagnostic>> {
    let mut v = ConstantValidator {
        constants: Default::default(),
    };

    let mut diagnostics = Vec::new();

    for group in &root_module.ordering {
        for path in group {
            let ExprOrTy::Expr(expr) = root_module.module.get(path).unwrap() else {
                continue;
            };
            if expr.kind.is_func() || expr.kind.is_internal() {
                continue;
            }

            let res = v.validate(expr);
            match res {
                Ok(_) => {
                    v.constants.insert(path.clone());
                }
                Err(span) => {
                    diagnostics.push(
                        Diagnostic::new_custom("non-constant expression")
                            .with_span(span.or(expr.span))
                            .push_hint("use `func` instead of `const`"),
                    );
                }
            }
        }
    }

    if diagnostics.is_empty() {
        Ok(())
    } else {
        Err(diagnostics)
    }
}

struct ConstantValidator {
    constants: HashSet<pr::Path>,
}
impl ConstantValidator {
    /// Validates that expr is constant
    fn validate(&mut self, expr: &pr::Expr) -> Result<(), Option<Span>> {
        let r = match &expr.kind {
            pr::ExprKind::Literal(_) => Ok(()),

            pr::ExprKind::Ident(_) => match expr.target.as_ref().unwrap() {
                pr::Ref::FullyQualified { to_decl, within } => {
                    assert!(within.is_empty());
                    if self.constants.contains(to_decl) {
                        Ok(())
                    } else {
                        Err(expr.span)
                    }
                }
                pr::Ref::Local { .. } => Err(expr.span),
            },

            pr::ExprKind::Indirection { base, .. } => self.validate(base),

            pr::ExprKind::Tuple(fields) => fields
                .iter()
                .find_map(|f| self.validate(&f.expr).err())
                .map(Err)
                .unwrap_or(Ok(())),
            pr::ExprKind::Array(items) => items
                .iter()
                .find_map(|i| self.validate(i).err())
                .map(Err)
                .unwrap_or(Ok(())),
            pr::ExprKind::EnumVariant(variant) => variant
                .inner
                .as_ref()
                .map(|i| self.validate(i))
                .unwrap_or(Ok(())),

            pr::ExprKind::FuncCall(_) | pr::ExprKind::Func(_) | pr::ExprKind::Match(_) => {
                Err(expr.span)
            }

            // resolved away
            pr::ExprKind::TypeAnnotation(_)
            | pr::ExprKind::Range(_)
            | pr::ExprKind::Binary(_)
            | pr::ExprKind::Unary(_)
            | pr::ExprKind::Pipeline(_)
            | pr::ExprKind::FString(_)
            | pr::ExprKind::Internal => unreachable!(),
        };
        r.map_err(|s| s.or(expr.span))
    }
}
