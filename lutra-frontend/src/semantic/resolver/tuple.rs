use crate::pr;
use crate::Result;

pub fn lookup_position_in_tuple(base: &pr::Ty, position: usize) -> Result<Option<Indirection>> {
    // get base fields
    let pr::TyKind::Tuple(fields) = &base.kind else {
        return Ok(None);
    };

    let singles = fields.as_slice();

    Ok(if position < singles.len() {
        fields.get(position).map(|f| Indirection {
            base: BaseKind::Tuple,
            position,
            target_ty: f.ty.clone(),
        })
    } else {
        None
    })
}

impl super::Resolver<'_> {
    /// Performs tuple indirection by name.
    pub fn lookup_name_in_tuple<'a>(
        &'a self,
        fields: &'a [pr::TyTupleField],
        name: &str,
    ) -> Result<Option<Indirection>> {
        log::debug!("looking up `.{name}` in {:?}", fields);

        for (position, field) in fields.iter().enumerate() {
            if field.name.as_ref().map_or(false, |n| n == name) {
                return Ok(Some(Indirection {
                    base: BaseKind::Tuple,
                    position,
                    target_ty: field.ty.clone(),
                }));
            }
        }
        Ok(None)
    }
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
