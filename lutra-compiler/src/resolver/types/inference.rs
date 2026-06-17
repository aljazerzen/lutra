use itertools::Itertools;

use crate::diagnostic::WithErrorInfo;
use crate::pr::{self, *};
use crate::resolver::NS_STD;
use crate::{Result, Span};

use super::TypeResolver;

impl TypeResolver<'_> {
    pub fn infer_type(&mut self, expr: &Expr) -> Result<Ty> {
        if let Some(ty) = expr.ty.as_deref() {
            return Ok(ty.clone());
        }

        let kind = match &expr.kind {
            ExprKind::Literal(literal) => {
                return Ok(self.infer_type_of_literal(literal, expr.span));
            }

            ExprKind::FString(_) => return Ok(self.new_ty_std("Text", expr.span)),

            ExprKind::TypeAnnotation(annotation) => annotation.ty.kind.clone(),

            ExprKind::Array(items) => {
                let mut items_ty = None;
                for item in items {
                    let item_ty = item.ty.as_deref().cloned().unwrap();
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
                        default: p.default.clone(),
                        span: Some(p.span),
                    })
                    .collect_vec(),
                body: func
                    .return_ty
                    .clone()
                    .or_else(|| func.body.ty.as_deref().cloned())
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
            Literal::Boolean(_) => self.new_ty_std("Bool", span),
            Literal::Text(_) => self.new_ty_std("Text", span),

            Literal::Number(_) => {
                // number literals (e.g. `4`) can be of type `int64` or `u8` or any other
                // integer type. So we have leave the type to be figured out later.
                // This is done with a new type param, constrained to integer types.

                let mut candidates = Vec::new();
                if literal.as_integer().is_some() {
                    candidates.extend([
                        self.new_ty_std("Int8", span),
                        self.new_ty_std("Int16", span),
                        self.new_ty_std("Int32", span),
                        self.new_ty_std("Int64", span),
                        self.new_ty_std("Uint8", span),
                        self.new_ty_std("Uint16", span),
                        self.new_ty_std("Uint32", span),
                        self.new_ty_std("Uint64", span),
                    ]);
                }
                if literal.as_float().is_some() {
                    candidates.push(self.new_ty_std("Float32", span));
                    candidates.push(self.new_ty_std("Float64", span));
                }
                if literal.as_decimal().is_some() {
                    candidates.push(self.new_ty_std("Decimal", span));
                }
                self.introduce_ty_var(pr::TyDomain::OneOf(candidates), span.unwrap())
            }

            Literal::Date(_) => self.new_ty_std("Date", span),
            Literal::Duration(_) => self.new_ty_std("Duration", span),
            Literal::DateTime(..) => self.new_ty_std("Timestamp", span),
        }
    }

    pub(super) fn new_ty_std(&self, name: &str, span: Option<Span>) -> Ty {
        let path = if self.is_std {
            pr::Path::from_name(name)
        } else {
            pr::Path::new([NS_STD, name])
        };

        let mut ty = pr::Ty::new(path.clone());
        ty.span = span;
        ty.target = Some(pr::Ref::Global(path));
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
