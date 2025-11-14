use itertools::Itertools;

use crate::diagnostic::WithErrorInfo;
use crate::pr::{self, *};
use crate::resolver::NS_STD;
use crate::{Result, Span};

use super::TypeResolver;

impl TypeResolver<'_> {
    pub fn infer_type(&mut self, expr: &Expr) -> Result<Ty> {
        if let Some(ty) = &expr.ty {
            return Ok(ty.clone());
        }

        let kind = match &expr.kind {
            ExprKind::Literal(literal) => {
                return Ok(self.infer_type_of_literal(literal, expr.span));
            }

            ExprKind::FString(_) => TyKind::Primitive(TyPrimitive::text),

            ExprKind::TypeAnnotation(annotation) => annotation.ty.kind.clone(),

            ExprKind::Array(items) => {
                let mut items_ty = None;
                for item in items {
                    let item_ty = self.infer_type(item)?;
                    if let Some(items_ty) = &items_ty {
                        self.validate_type(&item_ty, items_ty, &|| None)
                            .with_span_fallback(item.span)?;
                    } else {
                        items_ty = Some(item_ty);
                    }
                }
                let items_ty = if let Some(t) = items_ty {
                    t
                } else {
                    // no items, so we must infer the type
                    self.introduce_ty_var(pr::TyParamDomain::Open, expr.span.unwrap())
                };
                TyKind::Array(Box::new(items_ty))
            }

            ExprKind::Func(func) => TyKind::Func(TyFunc {
                params: func
                    .params
                    .iter()
                    .map(|p| (p.ty.clone(), p.constant))
                    .collect_vec(),
                body: func
                    .return_ty
                    .clone()
                    .or_else(|| func.body.ty.clone())
                    .map(Box::new),
                ty_params: func.ty_params.clone(),
            }),

            // type computed in the main pass
            ExprKind::Ident(_)
            | ExprKind::Tuple(_)
            | ExprKind::FuncCall(_)
            | ExprKind::EnumVariant(_)
            | ExprKind::Match(_)
            | ExprKind::TupleLookup { .. }
            | ExprKind::If(_) => unreachable!(),

            // desugar-ed
            ExprKind::Nested(_)
            | ExprKind::Range(_)
            | ExprKind::Binary(_)
            | ExprKind::Unary(_)
            | ExprKind::Internal => unreachable!(),
        };
        let mut ty = Ty::new(kind);
        ty.span = expr.span;
        Ok(ty)
    }

    pub fn infer_type_of_literal(&mut self, literal: &pr::Literal, span: Option<Span>) -> pr::Ty {
        let kind = match literal {
            Literal::Boolean(_) => TyKind::Primitive(TyPrimitive::bool),

            Literal::Text(_) => TyKind::Primitive(TyPrimitive::text),

            Literal::Integer(_) => {
                // int literal (e.g. `4`) can be of type `int64` or `u8` or any other
                // integer type. So we have leave the type to be figured out later.
                // This is done with a new type param, constraint to integer types.
                return self.introduce_ty_var(
                    pr::TyParamDomain::OneOf(vec![
                        pr::TyPrimitive::int8,
                        pr::TyPrimitive::int16,
                        pr::TyPrimitive::int32,
                        pr::TyPrimitive::int64,
                        pr::TyPrimitive::uint8,
                        pr::TyPrimitive::uint16,
                        pr::TyPrimitive::uint32,
                        pr::TyPrimitive::uint64,
                    ]),
                    span.unwrap(),
                );
            }
            Literal::Float(_) => {
                // similar as integers
                return self.introduce_ty_var(
                    pr::TyParamDomain::OneOf(vec![
                        pr::TyPrimitive::float32,
                        pr::TyPrimitive::float64,
                    ]),
                    span.unwrap(),
                );
            }

            Literal::Date(_) => {
                let fq_path = pr::Path::new([NS_STD, "Date"]);
                let mut ty = pr::Ty::new(fq_path.clone());
                ty.span = span;
                ty.target = Some(pr::Ref::FullyQualified {
                    to_def: fq_path,
                    within: Path::empty(),
                });
                return ty;
            }

            _ => todo!(),
        };
        let mut ty = Ty::new(kind);
        ty.span = span;
        ty
    }
}

#[allow(dead_code)]
// TODO: decide how do we want to deal with duplicate tuple names
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
