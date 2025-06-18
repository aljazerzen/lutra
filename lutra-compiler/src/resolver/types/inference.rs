use itertools::Itertools;

use crate::pr::{self, *};
use crate::Result;

use super::TypeResolver;

impl TypeResolver<'_> {
    pub fn infer_type(&mut self, expr: &Expr) -> Result<Ty> {
        if let Some(ty) = &expr.ty {
            return Ok(ty.clone());
        }

        let kind = match &expr.kind {
            ExprKind::Literal(ref literal) => match literal {
                Literal::Integer(_) => TyKind::Primitive(TyPrimitive::int64),
                Literal::Float(_) => TyKind::Primitive(TyPrimitive::float64),
                Literal::Boolean(_) => TyKind::Primitive(TyPrimitive::bool),
                Literal::Text(_) => TyKind::Primitive(TyPrimitive::text),
                _ => panic!(),
            },

            ExprKind::FString(_) => TyKind::Primitive(TyPrimitive::text),

            ExprKind::TypeAnnotation(annotation) => {
                annotation.ty.kind.clone()
            }

            ExprKind::Tuple(fields) => {
                let mut ty_fields: Vec<TyTupleField> = Vec::with_capacity(fields.len());

                for field in fields {
                    let ty = self.infer_type(&field.expr)?;

                    let name = field
                        .name
                        .clone()
                        .or_else(|| self.infer_tuple_field_name(&field.expr));

                    ty_fields.push(TyTupleField { name, ty });
                }
                ty_tuple_kind(ty_fields)
            }
            ExprKind::Array(items) => {
                let mut variants = Vec::with_capacity(items.len());
                for item in items {
                    let item_ty = self.infer_type(item)?;
                    variants.push(item_ty);
                }
                let items_ty = match variants.len() {
                    0 => {
                        // no items, so we must infer the type
                        self.introduce_ty_var(pr::TyParamDomain::Open, expr.span)
                    }
                    1 => {
                        // single item, use its type
                        variants.into_iter().exactly_one().unwrap()
                    }
                    2.. => {
                        // ideally, we would enforce that all of items have
                        // the same type, but currently we don't have a good
                        // strategy for dealing with nullable types, which
                        // causes problems here.
                        // HACK: use only the first type
                        variants.into_iter().unique().next().unwrap()
                    }
                };
                TyKind::Array(Box::new(items_ty))
            }

            // ExprKind::All { within, except } => {
            //     let Some(within_ty) = self.infer_type(within)? else {
            //         return Ok(None);
            //     };
            //     let Some(except_ty) = self.infer_type(except)? else {
            //         return Ok(None);
            //     };
            //     self.ty_tuple_exclusion(within_ty, except_ty)?
            // }
            ExprKind::Match(_) => unreachable!(), // type computed in the main pass

            ExprKind::Func(func) => TyKind::Func(TyFunc {
                params: func.params.iter().map(|p| p.ty.clone()).collect_vec(),
                body: func
                    .return_ty
                    .clone()
                    .or_else(|| func.body.ty.clone())
                    .map(Box::new),
                ty_params: func.ty_params.clone(),
            }),

            ExprKind::Ident(_)
            | ExprKind::EnumVariant(_) // constructed only in type resolver
            | ExprKind::FuncCall(_)
            | ExprKind::Indirection { .. }
            | ExprKind::Pipeline(_) // desugar-ed
            | ExprKind::Range(_) // desugar-ed
            | ExprKind::Binary(_) // desugar-ed
            | ExprKind::Unary(_) // desugar-ed
            | ExprKind::Internal => unreachable!(),
        };
        let mut ty = Ty::new(kind);
        ty.span = expr.span;
        Ok(ty)
    }

    fn infer_tuple_field_name(&self, field: &Expr) -> Option<String> {
        // at this stage, this expr should already be fully resolved
        // this means that any indirections will be tuple positional
        // so we check for that and pull the name from the type of the base

        let ExprKind::Indirection {
            base,
            field: IndirectionKind::Position(pos),
        } = &field.kind
        else {
            return None;
        };

        let ty = base.ty.as_ref()?;
        self.get_ty_tuple_field_name(ty, *pos as usize)
    }

    fn get_ty_tuple_field_name(&self, ty: &Ty, pos: usize) -> Option<String> {
        // SAFETY: get_my_ty will not error, because it has been resolved earlier already
        let mat_ty = self.get_ty_mat(ty).unwrap();

        match mat_ty {
            super::scope::TyRef::Ty(ty) => match &ty.kind {
                TyKind::Tuple(fields) => {
                    // this tuple might contain Unpacks (which affect positions of fields after them)
                    // so we need to resolve this type full first.

                    // unpacks don't interfere with preceding fields
                    let field = fields.get(pos)?;

                    field.name.clone()
                }

                TyKind::Ident(_fq_ident) => unreachable!(),
                _ => None,
            },
            super::scope::TyRef::Param(..) => None,
            super::scope::TyRef::Var(..) => None,
        }
    }
}

fn ty_tuple_kind(fields: Vec<TyTupleField>) -> TyKind {
    let mut res: Vec<TyTupleField> = Vec::with_capacity(fields.len());
    for field in fields {
        let TyTupleField { name, .. } = &field;

        // remove names from previous fields with the same name
        if name.is_some() {
            for f in res.iter_mut() {
                if f.name.as_ref() == name.as_ref() {
                    f.name = None;
                }
            }
        }

        res.push(field);
    }
    TyKind::Tuple(res)
}
