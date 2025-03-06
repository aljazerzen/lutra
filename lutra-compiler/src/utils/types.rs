use std::collections::HashMap;

use crate::pr;
use crate::utils::fold;
use crate::utils::fold::PrFold;
use crate::Result;

pub struct TypeReplacer {
    mapping: HashMap<pr::Path, pr::Ty>,
}

impl TypeReplacer {
    #[tracing::instrument(name = "TypeReplacer", skip_all)]
    pub fn on_ty(ty: pr::Ty, mapping: HashMap<pr::Path, pr::Ty>) -> pr::Ty {
        TypeReplacer { mapping }.fold_type(ty).unwrap()
    }

    #[tracing::instrument(name = "TypeReplacer", skip_all)]
    pub fn on_func(func: pr::Func, mapping: HashMap<pr::Path, pr::Ty>) -> pr::Func {
        TypeReplacer { mapping }.fold_func(func).unwrap()
    }
    #[tracing::instrument(name = "TypeReplacer", skip_all)]
    pub fn on_expr(func: pr::Expr, mapping: HashMap<pr::Path, pr::Ty>) -> pr::Expr {
        TypeReplacer { mapping }.fold_expr(func).unwrap()
    }
}

impl fold::PrFold for TypeReplacer {
    fn fold_type(&mut self, mut ty: pr::Ty) -> Result<pr::Ty> {
        match ty.kind {
            pr::TyKind::Ident(ident) => {
                if let Some(new_ty) = self.mapping.get(&ident) {
                    let ty = new_ty.clone();
                    self.fold_type(ty)
                } else {
                    ty.kind = pr::TyKind::Ident(ident);
                    Ok(ty)
                }
            }
            _ => fold::fold_type(self, ty),
        }
    }
}
pub struct TypeLayoutResolver {}

impl TypeLayoutResolver {
    #[tracing::instrument(name = "TypeLayoutResolverSimple", skip_all)]
    pub fn on_expr(expr: pr::Expr) -> pr::Expr {
        TypeLayoutResolver {}.fold_expr(expr).unwrap()
    }

    #[tracing::instrument(name = "TypeLayoutResolverSimple", skip_all)]
    pub fn on_func(func: pr::Func) -> Result<pr::Func> {
        TypeLayoutResolver {}.fold_func(func)
    }
}

impl fold::PrFold for TypeLayoutResolver {
    fn fold_type(&mut self, ty: pr::Ty) -> Result<pr::Ty> {
        let mut ty = fold::fold_type(self, ty).unwrap();
        if ty.layout.is_none() {
            ty.layout = ty.kind.get_layout_simple();
        }
        Ok(ty)
    }
}
