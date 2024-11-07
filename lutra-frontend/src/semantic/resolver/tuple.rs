use std::borrow::Cow;

use itertools::Itertools;

use crate::error::{Diagnostic, WithErrorInfo};
use crate::pr;
use crate::Result;

// TODO: i'm not proud of the naming scheme in this file

pub fn lookup_position_in_tuple(base: &pr::Ty, position: usize) -> Result<Option<StepOwned>> {
    // get base fields
    let pr::TyKind::Tuple(fields) = &base.kind else {
        return Ok(None);
    };

    let singles = fields.as_slice();

    Ok(if position < singles.len() {
        fields.get(position).map(|f| StepOwned {
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
        &'a mut self,
        ty: &'a pr::Ty,
        name: &str,
    ) -> Result<Option<Vec<StepOwned>>> {
        log::debug!("looking up `.{name}` in {:?}", ty);

        // find existing field
        let found = self.find_name_in_tuple(ty, name);
        match found.len() {
            // no match: pass though
            0 => {}

            // single match, great!
            1 => {
                let found = found.into_iter().next().unwrap();
                return Ok(Some(
                    found.into_iter().map(|s| s.into_owned()).collect_vec(),
                ));
            }

            // ambiguous
            _ => return Err(ambiguous_error(found)),
        }

        Ok(None)
    }

    /// Find in fields of this tuple (including the unpack)
    fn find_name_in_tuple<'a>(&'a self, ty: &'a pr::Ty, name: &str) -> Vec<Vec<Step>> {
        let pr::TyKind::Tuple(fields) = &ty.kind else {
            return vec![];
        };

        if let Some(step) = self.find_name_in_tuple_direct(ty, name) {
            return vec![vec![step]];
        };

        let mut res = vec![];
        for (position, field) in fields.iter().enumerate() {
            for mut x in self.find_name_in_tuple(&field.ty, name) {
                x.insert(
                    0,
                    Step {
                        position,
                        name: field.name.as_ref(),
                        target_ty: Cow::Borrowed(&field.ty),
                    },
                );
                res.push(x);
            }
        }
        res
    }

    /// Find in this tuple (including the unpack)
    fn find_name_in_tuple_direct<'a>(&'a self, ty: &'a pr::Ty, name: &str) -> Option<Step<'a>> {
        let pr::TyKind::Tuple(fields) = &ty.kind else {
            return None;
        };

        for (position, field) in fields.iter().enumerate() {
            if field.name.as_ref().map_or(false, |n| n == name) {
                return Some(Step {
                    position,
                    name: field.name.as_ref(),
                    target_ty: Cow::Borrowed(&field.ty),
                });
            }
        }
        None
    }

    /// Utility function for wrapping an expression into additional indirections.
    /// For example, when we have `x.a`, but `x = {b = {a = int}}`, lookup will return steps `[b, a]`.
    /// This function converts `x` and `[b, a]` into `((x).b).a`.
    pub fn apply_indirections(&mut self, mut base: pr::Expr, steps: Vec<StepOwned>) -> pr::Expr {
        for step in steps {
            base = pr::Expr {
                id: Some(self.id.gen()),
                ty: Some(step.target_ty),
                ..pr::Expr::new(pr::ExprKind::Indirection {
                    base: Box::new(base),
                    field: pr::IndirectionKind::Position(step.position as i64),
                })
            }
        }
        base
    }
}

#[derive(Debug, Clone)]
pub struct Step<'a> {
    position: usize,
    name: Option<&'a String>,
    target_ty: Cow<'a, pr::Ty>,
}

impl<'a> Step<'a> {
    #[allow(dead_code)]
    fn into_indirection(self) -> pr::IndirectionKind {
        if let Some(name) = self.name {
            pr::IndirectionKind::Name(name.clone())
        } else {
            pr::IndirectionKind::Position(self.position as i64)
        }
    }

    fn as_str(&self) -> Cow<str> {
        if let Some(name) = self.name {
            name.into()
        } else {
            self.position.to_string().into()
        }
    }

    fn into_owned(self) -> StepOwned {
        StepOwned {
            position: self.position,
            target_ty: self.target_ty.into_owned(),
        }
    }
}

#[derive(PartialEq)]
pub struct StepOwned {
    position: usize,
    target_ty: pr::Ty,
}

impl std::fmt::Debug for StepOwned {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StepOwned")
            .field("position", &self.position)
            .field("target_ty", &self.target_ty)
            .finish()
    }
}

fn ambiguous_error(candidates: Vec<Vec<Step>>) -> Diagnostic {
    let mut candidates_str = Vec::new();
    for steps in candidates {
        let mut steps = steps.into_iter();

        let first = steps.next().unwrap();
        let mut r = first.as_str().to_string();
        for step in steps {
            r += ".";
            r += &step.as_str();
        }
        candidates_str.push(r);
    }
    let hint = format!("could be any of: {}", candidates_str.join(", "));
    Diagnostic::new_simple("Ambiguous name").push_hint(hint)
}
