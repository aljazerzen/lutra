use crate::pr::{self, Ty};

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
        if field.name.as_ref().map_or(false, |n| n == name) {
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
        if field.name.as_ref().map_or(false, |n| n == name) {
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
