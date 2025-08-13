use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::pr::{self, Ty};
use crate::utils::fold::PrFold;
use crate::{Result, Span, printer};

use super::scope;

impl super::TypeResolver<'_> {
    pub fn resolve_tuple_constructor(
        &mut self,
        fields_in: Vec<pr::TupleField>,
    ) -> Result<(pr::ExprKind, pr::Ty)> {
        let mut fields = Vec::with_capacity(fields_in.len());
        let mut ty_fields: Vec<pr::TyTupleField> = Vec::with_capacity(fields_in.len());

        for f in fields_in {
            let name = (f.name.clone()).or_else(|| self.infer_tuple_field_name(&f.expr));

            let expr = self.fold_expr(f.expr)?;
            let ty = expr.ty.clone().unwrap();

            if f.unpack {
                // validate that ty is a tuple
                let ty_ref = self.get_ty_mat(&ty).unwrap();
                match ty_ref {
                    scope::TyRef::Ty(t) => {
                        if !t.kind.is_tuple() {
                            return Err(Diagnostic::new_custom("only tuples can be unpacked")
                                .with_span(expr.span)
                                .push_hint(format!("got type {}", printer::print_ty(t.as_ref()))));
                        }
                    }
                    scope::TyRef::Param(id) => {
                        let (p_name, domain) = self.get_ty_param(id);
                        match domain {
                            pr::TyParamDomain::Open
                            | pr::TyParamDomain::OneOf(_)
                            | pr::TyParamDomain::EnumVariants(_) => {
                                return Err(Diagnostic::new_custom(format!(
                                    "unpack expected a tuple, found {p_name}"
                                ))
                                .push_hint(format!("{p_name} is not constrained to tuples only")));
                            }
                            pr::TyParamDomain::TupleFields(_) => {
                                // ok
                            }
                        }
                    }
                    scope::TyRef::Var(_, o) => {
                        // restrict var to be a tuple

                        let domain = pr::TyParamDomain::TupleFields(vec![]);
                        let scope = self.get_ty_var_scope();
                        scope.infer_type_var_in_domain(o, domain);
                    }
                };
            }

            fields.push(pr::TupleField {
                name: f.name,
                unpack: f.unpack,
                expr,
            });
            ty_fields.push(pr::TyTupleField {
                name,
                unpack: f.unpack,
                ty,
            });
        }
        let kind = pr::ExprKind::Tuple(fields);
        let ty = pr::Ty::new(pr::TyKind::Tuple(ty_fields));
        Ok((kind, ty))
    }

    fn infer_tuple_field_name(&self, field: &pr::Expr) -> Option<String> {
        // at this stage, this expr should already be fully resolved
        // this means that any indirections will be tuple positional
        // so we check for that and pull the name from the type of the base

        let pr::ExprKind::Indirection {
            base,
            field: pr::IndirectionKind::Position(pos),
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
                pr::TyKind::Tuple(fields) => {
                    // this tuple might contain Unpacks (which affect positions of fields after them)
                    // so we need to resolve this type full first.

                    // unpacks don't interfere with preceding fields
                    let field = fields.get(pos)?;

                    field.name.clone()
                }

                pr::TyKind::Ident(_fq_ident) => unreachable!(),
                _ => None,
            },
            super::scope::TyRef::Param(..) => None,
            super::scope::TyRef::Var(..) => None,
        }
    }

    /// Resolve indirections (lookups).
    /// For example, `base.indirection` where `base` either has a tuple or array type.
    pub fn resolve_indirection(
        &mut self,
        base: &Ty,
        indirection: &pr::IndirectionKind,
        span: Span,
    ) -> Result<Indirection> {
        let base_ref = self.get_ty_mat(base)?;

        let base = match base_ref {
            scope::TyRef::Ty(b) => b,
            scope::TyRef::Param(id) => {
                let (_, param) = self.get_ty_param(id);
                return match param {
                    pr::TyParamDomain::Open
                    | pr::TyParamDomain::OneOf(_)
                    | pr::TyParamDomain::EnumVariants(_) => Err(Diagnostic::new_custom(format!(
                        "expected a tuple or an array, found {}",
                        base.kind.as_ref()
                    ))),
                    pr::TyParamDomain::TupleFields(fields) => lookup_in_domain(fields, indirection),
                };
            }
            scope::TyRef::Var(_, o) => {
                // introduce a new type var for the field
                let field_ty = self.introduce_ty_var(pr::TyParamDomain::Open, span);

                // restrict existing ty var to tuples with this field
                let domain = pr::TyParamDomain::TupleFields(vec![pr::TyDomainTupleField {
                    location: indirection.clone(),
                    ty: field_ty.clone(),
                }]);
                let scope = self.get_ty_var_scope();
                scope.infer_type_var_in_domain(o, domain);

                return Ok(Indirection {
                    base: BaseKind::Tuple,
                    position: None,
                    target_ty: field_ty,
                });
            }
        };

        match &base.kind {
            pr::TyKind::Ident(_) => unreachable!(),

            pr::TyKind::Tuple(fields) => lookup_in_tuple(&base, fields, indirection),

            pr::TyKind::Array(items_ty) => match indirection {
                pr::IndirectionKind::Name(_) => {
                    Err(Diagnostic::new_custom("cannot lookup array items by name"))
                }
                pr::IndirectionKind::Position(pos) => Ok(Indirection {
                    base: BaseKind::Array,
                    position: Some(*pos as usize),
                    target_ty: *items_ty.clone(),
                }),
            },

            pr::TyKind::Primitive(_) | pr::TyKind::Enum(_) | pr::TyKind::Func(_) => {
                Err(Diagnostic::new_custom(format!(
                    "expected a tuple or an array, found {}",
                    base.kind.as_ref()
                )))
            }
        }
    }
}

pub fn lookup_in_tuple(
    base: &pr::Ty,
    fields: &[pr::TyTupleField],
    indirection: &pr::IndirectionKind,
) -> Result<Indirection> {
    let r = match indirection {
        pr::IndirectionKind::Name(name) => lookup_name_in_tuple(fields, name),
        pr::IndirectionKind::Position(pos) => lookup_position_in_tuple(fields, *pos as usize),
    };
    let Some(r) = r else {
        return Err(Diagnostic::new_custom(format!(
            "field {} does not exist in type {}",
            print_indirection_kind(indirection),
            printer::print_ty(base)
        )));
    };
    Ok(r)
}

fn lookup_position_in_tuple(fields: &[pr::TyTupleField], position: usize) -> Option<Indirection> {
    let field = fields.get(position)?;
    Some(Indirection {
        base: BaseKind::Tuple,
        position: Some(position),
        target_ty: field.ty.clone(),
    })
}

fn lookup_name_in_tuple(fields: &[pr::TyTupleField], name: &str) -> Option<Indirection> {
    let (position, field) = fields
        .iter()
        .enumerate()
        .find(|(_, f)| f.name.as_ref().is_some_and(|n| n == name))?;
    Some(Indirection {
        base: BaseKind::Tuple,
        position: Some(position),
        target_ty: field.ty.clone(),
    })
}

pub fn lookup_in_domain(
    fields: &[pr::TyDomainTupleField],
    indirection: &pr::IndirectionKind,
) -> Result<Indirection> {
    let Some(field) = fields.iter().find(|f| &f.location == indirection) else {
        return Err(Diagnostic::new_custom(format!(
            "field {} does not exist",
            print_indirection_kind(indirection),
        )));
    };
    Ok(Indirection {
        base: BaseKind::Tuple,
        position: match indirection {
            pr::IndirectionKind::Name(_) => None,
            pr::IndirectionKind::Position(pos) => Some(*pos as usize),
        },
        target_ty: field.ty.clone(),
    })
}

#[derive(Debug, Clone)]
pub struct Indirection {
    pub base: BaseKind,

    // When base is a type var or type param, we cannot determine the
    // final position of a named field in the tuple.
    pub position: Option<usize>,

    pub target_ty: pr::Ty,
}

#[derive(Debug, Clone)]
pub enum BaseKind {
    Tuple,
    Array,
}

pub fn print_indirection_kind(indirection: &pr::IndirectionKind) -> String {
    match indirection {
        pr::IndirectionKind::Name(n) => format!(".{}", pr::display_ident(n)),
        pr::IndirectionKind::Position(p) => format!(".{p}"),
    }
}
