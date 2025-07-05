use crate::diagnostic::Diagnostic;
use crate::pr::{self, Ty};
use crate::{Result, printer};

use super::scope;

impl super::TypeResolver<'_> {
    /// Resolve indirections (lookups).
    /// For example, `base.indirection` where `base` either has a tuple or array type.
    ///
    /// Returns a positional indirection into the base.
    pub fn resolve_indirection(
        &mut self,
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
                    pr::TyParamDomain::TupleFields(fields) => lookup_in_domain(fields, indirection),
                };
            }
            scope::TyRef::Var(_, o) => {
                // introduce a new type var for the field
                let field_ty = self.introduce_ty_var(pr::TyParamDomain::Open, None);

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
