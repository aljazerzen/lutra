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
                    let item_ty = item.ty.clone().unwrap();
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
                    self.introduce_ty_var(pr::TyDomain::Open, expr.span.unwrap())
                };
                TyKind::Array(Box::new(items_ty))
            }

            ExprKind::Func(func) => TyKind::Func(TyFunc {
                params: func
                    .params
                    .iter()
                    .map(|p| pr::TyFuncParam {
                        constant: p.constant,
                        label: p.label.clone(),
                        ty: p.ty.clone(),
                    })
                    .collect_vec(),
                body: func
                    .return_ty
                    .clone()
                    .or_else(|| func.body.as_ref().and_then(|b| b.ty.clone()))
                    .map(Box::new),
                ty_params: func.ty_params.clone(),
            }),

            // type computed in the main pass
            ExprKind::Ident(_)
            | ExprKind::Tuple(_)
            | ExprKind::Call(_)
            | ExprKind::Variant(_)
            | ExprKind::Match(_)
            | ExprKind::Lookup { .. }
            | ExprKind::If(_)
            | ExprKind::VarBinding(_) => unreachable!(),

            // desugar-ed
            ExprKind::Nested(_)
            | ExprKind::Range(_)
            | ExprKind::Binary(_)
            | ExprKind::Unary(_)
            | ExprKind::FuncShort(_) => unreachable!("{}", expr.kind.as_ref()),
        };
        let mut ty = Ty::new(kind);
        ty.span = expr.span;
        Ok(ty)
    }

    pub fn infer_type_of_literal(&mut self, literal: &pr::Literal, span: Option<Span>) -> pr::Ty {
        match literal {
            Literal::Boolean(_) => Ty::new_with_span(TyPrimitive::bool, span.unwrap()),

            Literal::Text(_) => Ty::new_with_span(TyPrimitive::text, span.unwrap()),

            Literal::Number(_) => {
                // number literals (e.g. `4`) can be of type `int64` or `u8` or any other
                // integer type. So we have leave the type to be figured out later.
                // This is done with a new type param, constrained to integer types.

                let mut candidates = Vec::new();
                if literal.as_integer().is_some() {
                    candidates.extend([
                        Ty::new(pr::TyPrimitive::int8),
                        Ty::new(pr::TyPrimitive::int16),
                        Ty::new(pr::TyPrimitive::int32),
                        Ty::new(pr::TyPrimitive::int64),
                        Ty::new(pr::TyPrimitive::uint8),
                        Ty::new(pr::TyPrimitive::uint16),
                        Ty::new(pr::TyPrimitive::uint32),
                        Ty::new(pr::TyPrimitive::uint64),
                    ]);
                }
                if literal.as_float().is_some() {
                    candidates.extend([
                        Ty::new(pr::TyPrimitive::float32),
                        Ty::new(pr::TyPrimitive::float64),
                    ]);
                }
                if literal.as_decimal().is_some() {
                    candidates.push(new_ty_ident([NS_STD, "Decimal"], None));
                }
                self.introduce_ty_var(pr::TyDomain::OneOf(candidates), span.unwrap())
            }

            Literal::Date(_) => new_ty_ident([NS_STD, "Date"], span),
            Literal::Time(_) => new_ty_ident([NS_STD, "Time"], span),
            Literal::DateTime(..) => new_ty_ident([NS_STD, "Timestamp"], span),
        }
    }
}

fn new_ty_ident<S: ToString, I: IntoIterator<Item = S>>(fq_path: I, span: Option<Span>) -> Ty {
    let fq_path = pr::Path::new(fq_path);
    let mut ty = pr::Ty::new(fq_path.clone());
    ty.span = span;
    ty.target = Some(pr::Ref::Global(fq_path));
    ty
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
