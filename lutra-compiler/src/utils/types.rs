use std::collections::HashMap;

use crate::Result;
use crate::pr;
use crate::utils::fold;
use crate::utils::fold::PrFold;

pub struct TypeReplacer {
    mapping: HashMap<pr::Ref, pr::Ty>,
}

impl TypeReplacer {
    #[tracing::instrument(name = "TypeReplacer2", skip_all)]
    pub fn on_ty(ty: pr::Ty, mapping: HashMap<pr::Ref, pr::Ty>) -> pr::Ty {
        TypeReplacer { mapping }.fold_type(ty).unwrap()
    }

    #[tracing::instrument(name = "TypeReplacer2", skip_all)]
    pub fn on_func(func: pr::Func, mapping: HashMap<pr::Ref, pr::Ty>) -> pr::Func {
        TypeReplacer { mapping }.fold_func(func).unwrap()
    }
    #[tracing::instrument(name = "TypeReplacer2", skip_all)]
    pub fn on_expr(expr: pr::Expr, mapping: HashMap<pr::Ref, pr::Ty>) -> pr::Expr {
        TypeReplacer { mapping }.fold_expr(expr).unwrap()
    }
}

impl fold::PrFold for TypeReplacer {
    fn fold_type(&mut self, ty: pr::Ty) -> Result<pr::Ty> {
        if let Some(target) = &ty.target
            && let Some(new_ty) = self.mapping.get(target)
        {
            return self.fold_type(new_ty.clone());
        }
        fold::fold_type(self, ty)
    }
}
