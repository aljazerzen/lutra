use std::borrow::Cow;

use itertools::Itertools;

use crate::ir::pl::{Expr, ExprKind, IndirectionKind};
use crate::pr::{Ty, TyKind};
use crate::{Error, Result, WithErrorInfo};

// TODO: i'm not proud of the naming scheme in this file

pub fn lookup_position_in_tuple(base: &Ty, position: usize) -> Result<Option<StepOwned>> {
    // get base fields
    let TyKind::Tuple(fields) = &base.kind else {
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
        ty: &'a Ty,
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
    fn find_name_in_tuple<'a>(&'a self, ty: &'a Ty, name: &str) -> Vec<Vec<Step>> {
        let TyKind::Tuple(fields) = &ty.kind else {
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
    fn find_name_in_tuple_direct<'a>(&'a self, ty: &'a Ty, name: &str) -> Option<Step<'a>> {
        let TyKind::Tuple(fields) = &ty.kind else {
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
    pub fn apply_indirections(&mut self, mut base: Expr, steps: Vec<StepOwned>) -> Expr {
        for step in steps {
            base = Expr {
                id: Some(self.id.gen()),
                ty: Some(step.target_ty),
                ..Expr::new(ExprKind::Indirection {
                    base: Box::new(base),
                    field: IndirectionKind::Position(step.position as i64),
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
    target_ty: Cow<'a, Ty>,
}

impl<'a> Step<'a> {
    #[allow(dead_code)]
    fn into_indirection(self) -> IndirectionKind {
        if let Some(name) = self.name {
            IndirectionKind::Name(name.clone())
        } else {
            IndirectionKind::Position(self.position as i64)
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
    target_ty: Ty,
}

impl std::fmt::Debug for StepOwned {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StepOwned")
            .field("position", &self.position)
            .field("target_ty", &self.target_ty)
            .finish()
    }
}

fn ambiguous_error(candidates: Vec<Vec<Step>>) -> Error {
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
    Error::new_simple("Ambiguous name").push_hint(hint)
}

#[cfg(test)]
mod test {
    use crate::ir::decl::RootModule;
    use crate::parser::parse;
    use crate::pr::Ty;
    use crate::semantic::resolver::tuple::StepOwned;
    use crate::semantic::resolver::Resolver;
    use crate::{Error, Result, SourceTree};

    fn parse_ty(source: &str) -> Ty {
        let s = SourceTree::from(format!("type X = {source}"));
        let mod_def = parse(&s).unwrap();
        let stmt = mod_def.stmts.into_iter().next().unwrap();
        let ty_def = stmt.kind.into_type_def().unwrap();

        ty_def.value.unwrap()
    }

    fn tuple_lookup(tuple: &str, name: &str) -> Result<Vec<StepOwned>> {
        let mut root_module = RootModule::default();
        let mut r = Resolver::new(&mut root_module);

        r.lookup_name_in_tuple(&parse_ty(tuple), name)
            .and_then(|x| match x {
                Some(x) => Ok(x),
                None => Err(Error::new_simple("unknown name")),
            })
    }

    fn tuple_lookup_with_generic(tuple: &str, name: &str) -> Result<(Vec<StepOwned>, Ty)> {
        let mut root_module = RootModule::default();
        let mut r = Resolver::new(&mut root_module);

        // generate a new generic type (tests expect it to get name 'X1')
        let ident = r.init_new_global_generic("X");
        assert_eq!(ident.to_string(), "_generic.X1");

        // do the lookup
        let res = r.lookup_name_in_tuple(&parse_ty(tuple), name)?.unwrap();

        // get the generic candidate that was inferred
        let decl = r.get_ident(&ident).unwrap();
        let generic = decl.kind.as_generic_param().unwrap();
        let generic_candidate = generic.domain[0].clone();
        Ok((res, generic_candidate))
    }

    // ```prql
    // let x = {
    //   a = 1,
    //   b = {
    //     a = 2,
    //     b = 3,
    //     c = 4,
    //     d = {
    //       e = 5,
    //       ..G101
    //     }
    //   },
    //   c = 3,
    //   ..G100
    // }
    //
    // let y5 = x.f   # G102 (indirections: .3)
    // let y6 = x.b.f # G103 (indirections: .1.3.1)

    #[test]
    fn simple() {
        assert_eq!(
            tuple_lookup("{a = int}", "a").unwrap(),
            vec![StepOwned {
                position: 0,
                target_ty: parse_ty("int")
            }]
        );

        assert_eq!(
            tuple_lookup("{a = int, int, b = bool}", "b").unwrap(),
            vec![StepOwned {
                position: 2,
                target_ty: parse_ty("bool")
            }]
        );
    }

    #[test]
    fn unpack() {
        assert_eq!(
            tuple_lookup("{a = int, ..{b = bool}}", "b").unwrap(),
            vec![StepOwned {
                position: 1,
                target_ty: parse_ty("bool")
            }]
        );

        assert_eq!(
            tuple_lookup(
                "{a = int, ..{b = bool, ..{c = int, bool, ..{d = bool}}}}",
                "d"
            )
            .unwrap(),
            vec![StepOwned {
                position: 4,
                target_ty: parse_ty("bool")
            }]
        );
    }

    #[test]
    fn nested() {
        assert_eq!(
            tuple_lookup("{a = int, b = {bool, bool, c = int}}", "c").unwrap(),
            vec![
                StepOwned {
                    position: 1,
                    target_ty: parse_ty("{bool, bool, c = int}")
                },
                StepOwned {
                    position: 2,
                    target_ty: parse_ty("int")
                }
            ]
        );

        assert_eq!(
            tuple_lookup("{a = int, {b = int, {{c = int}, d = bool}, e = bool}}", "c").unwrap(),
            vec![
                StepOwned {
                    position: 1,
                    target_ty: parse_ty("{b = int, {{c = int}, d = bool}, e = bool}")
                },
                StepOwned {
                    position: 1,
                    target_ty: parse_ty("{{c = int}, d = bool}")
                },
                StepOwned {
                    position: 0,
                    target_ty: parse_ty("{c = int}")
                },
                StepOwned {
                    position: 0,
                    target_ty: parse_ty("int")
                },
            ]
        );

        // ambiguous
        tuple_lookup("{a = {c = int}, b = {c = int}}", "c").unwrap_err();

        // ambiguous
        tuple_lookup("{{c = int}, {c = int}}", "c").unwrap_err();

        // ambiguous
        tuple_lookup("{a = {c = int}, ..{b = {c = int}}}", "c").unwrap_err();

        assert_eq!(
            tuple_lookup("{a = int, b = {a = int}}", "a").unwrap(),
            vec![StepOwned {
                position: 0,
                target_ty: parse_ty("int")
            }]
        );

        assert_eq!(
            tuple_lookup("{a = int, b = {a = int}}", "a").unwrap(),
            vec![StepOwned {
                position: 0,
                target_ty: parse_ty("int")
            }]
        );
    }

    #[test]
    fn generic() {
        assert_eq!(
            tuple_lookup_with_generic("{a = int, .._generic.X1}", "b").unwrap(),
            (
                vec![StepOwned {
                    position: 1,
                    target_ty: parse_ty("_generic.F2")
                }],
                parse_ty("{b = _generic.F2}")
            )
        );

        assert_eq!(
            tuple_lookup_with_generic(
                "{a = int, b = {c = int, .._generic.X1}, .._generic.X1}",
                "d"
            )
            .unwrap(),
            (
                vec![StepOwned {
                    position: 2,
                    target_ty: parse_ty("_generic.F2")
                }],
                parse_ty("{d = _generic.F2}")
            )
        );

        assert_eq!(
            tuple_lookup_with_generic(
                "{a = int, b = {c = int, .._generic.X1}, ..{c = int, .._generic.X1}}",
                "d"
            )
            .unwrap(),
            (
                vec![StepOwned {
                    position: 3,
                    target_ty: parse_ty("_generic.F2")
                }],
                parse_ty("{d = _generic.F2}")
            )
        );

        assert_eq!(
            tuple_lookup_with_generic("{a = int, b = {c = int, .._generic.X1}, ..{c = int}}", "d")
                .unwrap(),
            (
                vec![
                    StepOwned {
                        position: 1,
                        target_ty: parse_ty("{c = int, .._generic.X1}")
                    },
                    StepOwned {
                        position: 1,
                        target_ty: parse_ty("_generic.F2")
                    }
                ],
                parse_ty("{d = _generic.F2}")
            )
        );

        assert_eq!(
            tuple_lookup_with_generic("{a = _generic.X1, .._generic.X1}", "b").unwrap(),
            (
                vec![
                    StepOwned {
                        position: 0,
                        target_ty: parse_ty("_generic.X1")
                    },
                    StepOwned {
                        position: 0,
                        target_ty: parse_ty("_generic.F2")
                    }
                ],
                parse_ty("{b = _generic.F2}")
            )
        );
    }
}
