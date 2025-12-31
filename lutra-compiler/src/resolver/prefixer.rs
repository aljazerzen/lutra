use itertools::Itertools;

use crate::pr;
use crate::utils::fold::{self, PrFold};

/// Adds a prefix to all global absolute references.
pub fn prefix(root_module: pr::ModuleDef, prefix: String) -> pr::ModuleDef {
    let mut p = Prefixer { prefix };
    p.fold_module_def(root_module).unwrap()
}

struct Prefixer {
    prefix: String,
}

impl Prefixer {
    fn prefix_ref(&self, ref_: Option<pr::Ref>) -> Option<pr::Ref> {
        let ref_ = ref_?;
        let pr::Ref::Global(mut absolute_ref) = ref_ else {
            return Some(ref_);
        };

        let mut prefixed = pr::Path::from_name(self.prefix.clone());
        prefixed.extend(absolute_ref);
        absolute_ref = prefixed;

        Some(pr::Ref::Global(absolute_ref))
    }
}

impl fold::PrFold for Prefixer {
    fn fold_expr(&mut self, mut expr: pr::Expr) -> crate::Result<pr::Expr> {
        // the thing
        expr.target = self.prefix_ref(expr.target);

        // boiler plate
        expr.kind = self.fold_expr_kind(expr.kind)?;
        expr.ty = fold::fold_type_opt(self, expr.ty)?;
        expr.ty_args = expr
            .ty_args
            .into_iter()
            .map(|t| self.fold_type(t))
            .try_collect()?;
        Ok(expr)
    }

    fn fold_type(&mut self, mut ty: pr::Ty) -> crate::Result<pr::Ty> {
        ty.target = self.prefix_ref(ty.target);
        fold::fold_type(self, ty)
    }
}
