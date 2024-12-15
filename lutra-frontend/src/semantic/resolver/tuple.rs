use std::borrow::Cow;

use crate::pr;
use crate::Result;

pub fn lookup_position_in_tuple(base: &pr::Ty, position: usize) -> Result<Option<Step>> {
    // get base fields
    let pr::TyKind::Tuple(fields) = &base.kind else {
        return Ok(None);
    };

    let singles = fields.as_slice();

    Ok(if position < singles.len() {
        fields.get(position).map(|f| Step {
            position,
            target_ty: Cow::Borrowed(&f.ty),
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
    ) -> Result<Option<Step<'a>>> {
        log::debug!("looking up `.{name}` in {:?}", fields);

        for (position, field) in fields.iter().enumerate() {
            if field.name.as_ref().map_or(false, |n| n == name) {
                return Ok(Some(Step {
                    position,
                    target_ty: Cow::Borrowed(&field.ty),
                }));
            }
        }
        Ok(None)
    }
}

#[derive(Debug, Clone)]
pub struct Step<'a> {
    pub position: usize,
    pub target_ty: Cow<'a, pr::Ty>,
}
