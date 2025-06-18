use crate::diagnostic::Diagnostic;
use crate::pr::{self, Ty};
use crate::utils::fold::PrFold;
use crate::{printer, Result};

use super::scope;

impl super::TypeResolver<'_> {
    pub fn resolve_column_exclusion(&mut self, expr: pr::Expr) -> Result<pr::Expr> {
        let expr = self.fold_expr(expr)?;
        let _except = self.coerce_into_tuple(expr)?;

        todo!()
        // self.fold_expr(pr::Expr::new(pr::ExprKind::All {
        //     within: Box::new(pr::Expr::new(pr::Path::from_path(vec![NS_THIS]))),
        //     except: Box::new(except),
        // }))
    }

    /// Resolve indirections (lookups).
    /// For example, `base.indirection` where `base` either has a tuple or array type.
    ///
    /// Returns a positional indirection into the base.
    pub fn resolve_indirection(
        &self,
        base: &Ty,
        indirection: &pr::IndirectionKind,
    ) -> Result<Indirection> {
        let base_ref = self.get_ty_mat(base)?;

        let base = match base_ref {
            scope::TyRef::Ty(b) => b,
            scope::TyRef::Param(id) => {
                let (_, param) = self.get_ty_param(id);
                return match param {
                    pr::TyParamDomain::Open | pr::TyParamDomain::OneOf(_) => {
                        Err(Diagnostic::new_custom(format!(
                            "expected a tuple or an array, found {}",
                            base.kind.as_ref()
                        )))
                    }
                    pr::TyParamDomain::TupleFields(fields) => lookup_in_domain(fields, indirection)
                        .ok_or_else(|| {
                            Diagnostic::new_custom(format!(
                                "Field {} does not exist in type {}",
                                print_indirection_kind(indirection),
                                printer::print_ty(base)
                            ))
                        }),
                };
            }
            scope::TyRef::Var(s, o) => {
                todo!("tuple indirection into generic type Arg: {s}.{o}")
            }
        };

        match &base.kind {
            pr::TyKind::Ident(_) => unreachable!(),

            pr::TyKind::Tuple(fields) => lookup_in_tuple(fields, indirection).ok_or_else(|| {
                Diagnostic::new_custom(format!(
                    "Field {} does not exist in type {}",
                    print_indirection_kind(indirection),
                    printer::print_ty(&base)
                ))
            }),

            pr::TyKind::Array(items_ty) => match indirection {
                pr::IndirectionKind::Name(_) => {
                    Err(Diagnostic::new_custom("cannot lookup array items by name"))
                }
                pr::IndirectionKind::Position(pos) => Ok(Indirection {
                    base: BaseKind::Array,
                    position: *pos as usize,
                    target_ty: *items_ty.clone(),
                }),
                pr::IndirectionKind::Star => todo!(),
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
    fields: &[pr::TyTupleField],
    indirection: &pr::IndirectionKind,
) -> Option<Indirection> {
    match indirection {
        pr::IndirectionKind::Name(name) => lookup_name_in_tuple(fields, name),
        pr::IndirectionKind::Position(pos) => lookup_position_in_tuple(fields, *pos as usize),
        pr::IndirectionKind::Star => todo!(),
    }
}

fn lookup_position_in_tuple(fields: &[pr::TyTupleField], position: usize) -> Option<Indirection> {
    if position < fields.len() {
        fields.get(position).map(|f| Indirection {
            base: BaseKind::Tuple,
            position,
            target_ty: f.ty.clone(),
        })
    } else {
        None
    }
}

fn lookup_name_in_tuple(fields: &[pr::TyTupleField], name: &str) -> Option<Indirection> {
    for (position, field) in fields.iter().enumerate() {
        if field.name.as_ref().is_some_and(|n| n == name) {
            return Some(Indirection {
                base: BaseKind::Tuple,
                position,
                target_ty: field.ty.clone(),
            });
        }
    }
    None
}

pub fn lookup_in_domain(
    fields: &[pr::TyDomainTupleField],
    indirection: &pr::IndirectionKind,
) -> Option<Indirection> {
    match indirection {
        pr::IndirectionKind::Name(name) => lookup_name_in_domain(fields, name),
        pr::IndirectionKind::Position(pos) => lookup_position_in_domain(fields, *pos as usize),
        pr::IndirectionKind::Star => todo!(),
    }
}

fn lookup_position_in_domain(
    fields: &[pr::TyDomainTupleField],
    position: usize,
) -> Option<Indirection> {
    if position < fields.len() {
        fields.get(position).map(|f| Indirection {
            base: BaseKind::Tuple,
            position,
            target_ty: Ty::new(f.ty.clone()),
        })
    } else {
        None
    }
}

fn lookup_name_in_domain(fields: &[pr::TyDomainTupleField], name: &str) -> Option<Indirection> {
    for (position, field) in fields.iter().enumerate() {
        if field.name.as_ref().is_some_and(|n| n == name) {
            return Some(Indirection {
                base: BaseKind::Tuple,
                position,
                target_ty: Ty::new(field.ty.clone()),
            });
        }
    }
    None
}

#[derive(Debug, Clone)]
pub struct Indirection {
    pub base: BaseKind,
    pub position: usize,
    pub target_ty: pr::Ty,
}

#[derive(Debug, Clone)]
pub enum BaseKind {
    Tuple,
    Array,
}

pub fn print_indirection_kind(indirection: &pr::IndirectionKind) -> String {
    match indirection {
        pr::IndirectionKind::Name(n) => format!("`{n}`"),
        pr::IndirectionKind::Position(p) => p.to_string(),
        pr::IndirectionKind::Star => "*".to_string(),
    }
}
