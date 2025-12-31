use std::collections::HashSet;

use crate::pr;
use crate::{Result, Span};

pub(super) struct ConstantValidator {
    constants: HashSet<pr::Path>,
}

impl ConstantValidator {
    pub fn new() -> Self {
        Self {
            constants: Default::default(),
        }
    }

    pub fn save_const(&mut self, path: pr::Path) {
        self.constants.insert(path);
    }

    /// Validates that expr is constant
    pub fn validate_is_const(&mut self, expr: &pr::Expr) -> Result<(), Option<Span>> {
        let r = match &expr.kind {
            pr::ExprKind::Literal(_) => Ok(()),

            pr::ExprKind::Ident(_) => match expr.target.as_ref().unwrap() {
                pr::Ref::Global(target_fq) => {
                    if self.constants.contains(target_fq) {
                        Ok(())
                    } else {
                        Err(expr.span)
                    }
                }
                pr::Ref::Local { .. } => Err(expr.span),
            },

            pr::ExprKind::Lookup { base, .. } => self.validate_is_const(base),

            pr::ExprKind::Tuple(fields) => fields
                .iter()
                .find_map(|f| self.validate_is_const(&f.expr).err())
                .map(Err)
                .unwrap_or(Ok(())),
            pr::ExprKind::Array(items) => items
                .iter()
                .find_map(|i| self.validate_is_const(i).err())
                .map(Err)
                .unwrap_or(Ok(())),
            pr::ExprKind::Variant(variant) => variant
                .inner
                .as_ref()
                .map(|i| self.validate_is_const(i))
                .unwrap_or(Ok(())),

            pr::ExprKind::Call(_)
            | pr::ExprKind::Func(_)
            | pr::ExprKind::Match(_)
            | pr::ExprKind::If(_)
            | pr::ExprKind::VarBinding(_) => Err(expr.span),

            // resolved away
            pr::ExprKind::TypeAnnotation(_)
            | pr::ExprKind::Range(_)
            | pr::ExprKind::Binary(_)
            | pr::ExprKind::Unary(_)
            | pr::ExprKind::Nested(_)
            | pr::ExprKind::FString(_)
            | pr::ExprKind::FuncShort(_)
            | pr::ExprKind::Native => unreachable!(),
        };
        r.map_err(|s| s.or(expr.span))
    }
}
