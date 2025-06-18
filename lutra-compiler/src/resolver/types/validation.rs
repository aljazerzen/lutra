use itertools::Itertools;
use std::collections::HashMap;

use crate::diagnostic::{Diagnostic, DiagnosticCode, WithErrorInfo};
use crate::pr::{self, *};
use crate::printer;
use crate::resolver::types::scope;
use crate::Result;

use super::scope::TyRef;
use super::TypeResolver;

impl TypeResolver<'_> {
    /// Validates that a found expr has an expected type.
    /// Might infer type variable constraints, that need to be finalized later.
    pub fn validate_expr_type<F>(
        &mut self,
        found: &mut pr::Expr,
        expected: &Ty,
        who: &F,
    ) -> Result<(), Diagnostic>
    where
        F: Fn() -> Option<String>,
    {
        let Some(found_ty) = &mut found.ty else {
            // found is none: infer from expected
            found.ty = Some(expected.clone());
            return Ok(());
        };

        self.validate_type(found_ty, expected, who)
            .with_span_fallback(found.span)
    }

    /// Validates that a type of a found node has an expected type.
    /// Might infer type variable constraints, that need to be finalized later.
    pub fn validate_type<F>(&mut self, found: &Ty, expected: &Ty, who: &F) -> Result<(), Diagnostic>
    where
        F: Fn() -> Option<String>,
    {
        tracing::trace!(
            "validate_type, f: {}, e: {}",
            printer::print_ty(found),
            printer::print_ty(expected)
        );

        if found.target.is_some() && found.target == expected.target {
            return Ok(());
        }

        let found_ref = self.get_ty_mat(found)?;
        let expected_ref = self.get_ty_mat(expected)?;

        match (found_ref, expected_ref) {
            // base case: both types are concrete types, check for compatibility
            (TyRef::Ty(f), TyRef::Ty(e)) => {
                self.validate_type_material(f.into_owned(), e.into_owned(), who)?
            }

            // type params
            (TyRef::Ty(..), TyRef::Param(..)) => {
                // validate that a found concrete type is a type parameter - which is never true.
                // example:
                //     func <T> () -> (false: T)
                // This *could* be true, if `T` would have domain of `OneOf(bool)`, but such domains
                // don't make sense and we don't need to add special cases for them.
                return Err(compose_type_error(found, expected, who));
            }
            (TyRef::Param(..), TyRef::Ty(expected)) => {
                // validate that a type parameter is an concrete type - which is never true.
                // example:
                //     func <T> (x: T) -> (x: int64)
                return Err(compose_type_error(found, &expected, who));
            }
            (TyRef::Param(found_id), TyRef::Param(expected_id)) => {
                if found_id != expected_id {
                    return Err(compose_type_error(found, expected, who));
                }
            }

            // type vars
            (TyRef::Ty(ty), TyRef::Var(_, var_id)) | (TyRef::Var(_, var_id), TyRef::Ty(ty)) => {
                let scope = self.scopes.last().unwrap();
                scope.infer_type_var(var_id, ty.into_owned());
            }
            (TyRef::Var(_, a_id), TyRef::Var(_, b_id)) => {
                let scope = self.scopes.last().unwrap();
                scope.infer_type_vars_equal(a_id, b_id);
            }
            (TyRef::Param(..), TyRef::Var(_, var_id)) => {
                // example:
                //     func <T> (x: T) -> twice(x)
                // (twice will create a type var for its arg)

                let scope = self.scopes.last().unwrap();
                scope.infer_type_var(var_id, found.clone());
            }
            (TyRef::Var(_, var_id), TyRef::Param(..)) => {
                // example:
                //     func <T> () -> []: [T]
                // (empty array creates a type var and type annotations triggers validation against the param)

                let scope = self.scopes.last().unwrap();
                scope.infer_type_var(var_id, expected.clone());
            }
        };
        Ok(())
    }

    fn validate_type_material<F>(&mut self, found: Ty, expected: Ty, who: &F) -> crate::Result<()>
    where
        F: Fn() -> Option<String>,
    {
        match (&found.kind, &expected.kind) {
            (TyKind::Ident(_), _) | (_, TyKind::Ident(_)) => unreachable!(),

            // base case
            (TyKind::Primitive(f), TyKind::Primitive(e)) if e == f => Ok(()),

            // containers: recurse
            (TyKind::Array(found_items), TyKind::Array(expected_items)) => {
                // co-variant contained type
                self.validate_type(&found_items.clone(), &expected_items.clone(), who)
            }
            (TyKind::Tuple(found_fields), TyKind::Tuple(expected_fields)) => {
                // here we need to check that found tuple has all fields that are expected.

                // build index of found fields
                let found_types: HashMap<_, _> = found_fields
                    .iter()
                    .filter_map(|e| e.name.as_ref().map(|n| (n, &e.ty)))
                    .collect();

                let mut expected_but_not_found = Vec::new();
                for (index, e_field) in expected_fields.iter().enumerate() {
                    if let Some(e_name) = &e_field.name {
                        // when a named field is expected

                        // if it was found
                        if let Some(f_ty) = found_types.get(e_name) {
                            // check its type

                            // co-variant contained type
                            self.validate_type(f_ty, &e_field.ty, who)?;
                        } else {
                            expected_but_not_found.push(e_field);
                        }
                    } else {
                        // when a positional field is expected
                        if let Some(f_field) = found_fields.get(index) {
                            // co-variant contained type
                            self.validate_type(&f_field.ty, &e_field.ty, who)?;
                        } else {
                            expected_but_not_found.push(e_field);
                        }
                    }
                }

                if !expected_but_not_found.is_empty() {
                    // not all fields were found
                    return Err(compose_type_error(&found, &expected, who));
                }

                Ok(())
            }
            (TyKind::Func(f_func), TyKind::Func(e_func))
                if f_func.params.len() == e_func.params.len() =>
            {
                for (f_arg, e_arg) in itertools::zip_eq(&f_func.params, &e_func.params) {
                    if let Some((f_arg, e_arg)) = Option::zip(f_arg.as_ref(), e_arg.as_ref()) {
                        // contra-variant contained types
                        self.validate_type(e_arg, f_arg, who)?;
                    }
                }

                // return types
                if let Some((f_ret, e_ret)) =
                    Option::zip(Option::as_ref(&f_func.body), Option::as_ref(&e_func.body))
                {
                    // co-variant contained type
                    self.validate_type(f_ret, e_ret, who)?;
                }
                Ok(())
            }

            (TyKind::Enum(f_variants), TyKind::Enum(e_variants))
                if f_variants.len() == e_variants.len() =>
            {
                // require same variant names
                let names_match =
                    itertools::zip_eq(f_variants, e_variants).all(|(f, e)| f.name == e.name);
                if !names_match {
                    return Err(compose_type_error(&found, &expected, who));
                }

                for (f_variant, e_variant) in itertools::zip_eq(f_variants, e_variants) {
                    // co-variant contained types
                    self.validate_type(&f_variant.ty, &e_variant.ty, who)?;
                }
                Ok(())
            }

            _ => Err(compose_type_error(&found, &expected, who)),
        }
    }

    pub fn finalize_type_vars(&mut self) -> crate::Result<HashMap<pr::Ref, pr::Ty>> {
        fn finalize_var(
            known: &mut HashMap<usize, pr::Ty>,
            id: usize,
            ty: pr::Ty,
        ) -> crate::Result<()> {
            use std::collections::hash_map::Entry;

            let entry = known.entry(id);
            match entry {
                Entry::Occupied(existing) => {
                    if existing.get() != &ty {
                        return Err(Diagnostic::new_custom(format!(
                            "incompatible types: {} and {}",
                            printer::print_ty(existing.get()),
                            printer::print_ty(&ty)
                        )));
                    }
                }
                Entry::Vacant(entry) => {
                    entry.insert(ty);
                }
            }
            Ok(())
        }

        let mut known_types = HashMap::new();
        let mut constraints = {
            let scope = self.scopes.last_mut().unwrap();
            scope.ty_var_constraints.take()
        };
        while !constraints.is_empty() {
            let mut remaining_constraints = Vec::with_capacity(constraints.len());

            let len = constraints.len();
            for constraint in constraints {
                match constraint {
                    scope::TyVarConstraint::IsTy(id, ty) => {
                        finalize_var(&mut known_types, id, ty)?;
                    }
                    scope::TyVarConstraint::Equals(a, b) => {
                        if let Some(ty) = known_types.get(&a).cloned() {
                            finalize_var(&mut known_types, b, ty)?;
                        } else if let Some(ty) = known_types.get(&b).cloned() {
                            finalize_var(&mut known_types, a, ty)?;
                        } else {
                            remaining_constraints.push(constraint);
                        }
                    }
                    scope::TyVarConstraint::InDomain(ref id, ref domain) => {
                        if let Some(ty) = known_types.get(id) {
                            let var = self.get_ty_var(*id);
                            self.validate_type_domain(ty, domain, var.name_hint.as_deref())?;
                        } else {
                            remaining_constraints.push(constraint);
                        }
                    }
                }
            }
            constraints = remaining_constraints;

            if len == constraints.len() {
                // no constraints were enforced in this loop, error out
                break;
            }
        }

        let mut mapping = HashMap::new();
        let mut errors = Vec::new();

        let scope = self.scopes.last().unwrap();
        for (offset, scoped) in scope.names.iter().enumerate() {
            let scope::ScopedKind::TyVar(var) = scoped else {
                continue;
            };

            let Some(ty) = known_types.get(&offset) else {
                errors.push(
                    Diagnostic::new_custom(if let Some(name_hint) = &var.name_hint {
                        format!("cannot infer type of {name_hint}")
                    } else {
                        "cannot infer type".into()
                    })
                    .with_span(var.span),
                );
                continue;
            };

            mapping.insert(
                pr::Ref::Local {
                    scope: scope.id,
                    offset,
                },
                ty.clone(),
            );
        }

        if !constraints.is_empty() {
            for c in constraints {
                tracing::debug!("cannot enforce ty var constraint: {c:?}");
            }
            if errors.is_empty() {
                errors.push(
                    Diagnostic::new_custom("cannot infer types")
                        .push_hint("this is a bad error message, it should be improved"),
                );
            }
        }

        if !errors.is_empty() {
            for e in &errors[1..] {
                tracing::error!("hidden diagnostic: {e:?}");
            }
            return Err(errors.into_iter().next().unwrap());
        }

        if !mapping.is_empty() {
            tracing::debug!("finalized scope: {mapping:#?}");
        }
        Ok(mapping)
    }

    /// Validates that a type is an a domain of a type param.
    pub fn validate_type_domain(
        &self,
        ty: &Ty,
        domain: &TyParamDomain,
        var_name: Option<&str>,
    ) -> Result<(), Diagnostic> {
        let ty_ref = self.get_ty_mat(ty)?;
        let ty = match ty_ref {
            TyRef::Ty(cow) => cow,
            TyRef::Param(param_id) => {
                let (found_name, found) = self.get_ty_param(param_id);
                self.validate_type_domains(found, domain, found_name, var_name)?;
                return Ok(());
            }
            TyRef::Var(_, _) => unreachable!(""),
        };

        match domain {
            TyParamDomain::Open => Ok(()),

            TyParamDomain::OneOf(possible_tys) => {
                let is_match = ty
                    .kind
                    .as_primitive()
                    .is_some_and(|t| possible_tys.iter().any(|p| t == p));

                if !is_match {
                    let possible_tys = possible_tys.iter().map(|t| t.to_string()).join(", ");

                    return Err(Diagnostic::new(
                        format!(
                            "{} one of {possible_tys}, found {}",
                            msg_restricted_to(var_name, None),
                            printer::print_ty(&ty)
                        ),
                        DiagnosticCode::TYPE_DOMAIN,
                    ));
                }

                Ok(())
            }

            TyParamDomain::TupleFields(domain_fields) => {
                let TyKind::Tuple(ty_fields) = &ty.kind else {
                    return Err(Diagnostic::new(
                        format!(
                            "{} to tuples, found {}",
                            msg_restricted_to(var_name, None),
                            printer::print_ty(&ty)
                        ),
                        DiagnosticCode::TYPE_DOMAIN,
                    ));
                };

                let num_positional = domain_fields
                    .iter()
                    .enumerate()
                    .filter(|(_, f)| f.name.is_none())
                    .next_back()
                    .map(|(p, _)| p + 1)
                    .unwrap_or_default();

                for (position, domain_field) in domain_fields.iter().enumerate() {
                    let (ind_display, ty_field) = if let Some(name) = &domain_field.name {
                        // named
                        let res = ty_fields.iter().find(|f| f.name.as_ref() == Some(name));

                        (
                            name.clone(),
                            res.ok_or_else(|| {
                                Diagnostic::new(
                                    format!(
                                        "{} tuples with a field named `{name}`",
                                        msg_restricted_to(var_name, None)
                                    ),
                                    DiagnosticCode::TYPE_DOMAIN,
                                )
                            })?,
                        )
                    } else {
                        // positional
                        (
                            position.to_string(),
                            ty_fields.get(position).ok_or_else(|| {
                                Diagnostic::new(
                                    format!(
                                        "{} tuples with at least {num_positional} fields",
                                        msg_restricted_to(var_name, None)
                                    ),
                                    DiagnosticCode::TYPE_DOMAIN,
                                )
                            })?,
                        )
                    };

                    let TyKind::Primitive(ty_field_ty) = &ty_field.ty.kind else {
                        return Err(Diagnostic::new(
                            format!(
                                "{} primitive types",
                                msg_restricted_to(var_name, Some(&ind_display))
                            ),
                            DiagnosticCode::TYPE_DOMAIN,
                        )
                        .push_hint("This is a temporary restriction. Work in progress."))
                        .with_span(ty_field.ty.span);
                    };

                    if ty_field_ty != &domain_field.ty {
                        return Err(Diagnostic::new(
                            format!(
                                "{} {}",
                                msg_restricted_to(var_name, Some(&ind_display)),
                                domain_field.ty
                            ),
                            DiagnosticCode::TYPE_DOMAIN,
                        )
                        .with_span(ty_field.ty.span));
                    }

                    // ok
                }

                // all ok
                Ok(())
            }
        }
    }

    /// Validates that found domain is subset of expected domain.
    pub fn validate_type_domains(
        &self,
        found: &TyParamDomain,
        expected: &TyParamDomain,
        found_name: &str,
        expected_name: Option<&str>,
    ) -> Result<(), Diagnostic> {
        match (found, expected) {
            // if expected is open, any found domain is ok
            (_, TyParamDomain::Open) => Ok(()),

            // if found is open, expected must be open too (but that was matched above)
            (TyParamDomain::Open, _) => Err(Diagnostic::new(
                if let Some(expected_name) = expected_name {
                    format!("{found_name} can be any type, but {expected_name} has restrictions")
                } else {
                    format!("{found_name} can be any type, but there are restrictions")
                },
                DiagnosticCode::TYPE_DOMAIN,
            )),

            // each found must be in expected domain
            (TyParamDomain::OneOf(found_tys), expected_domain) => {
                for found_ty in found_tys {
                    let ty = Ty::new(found_ty.clone());
                    self.validate_type_domain(&ty, expected_domain, Some(found_name))?;
                }
                Ok(())
            }

            (TyParamDomain::TupleFields(_), TyParamDomain::OneOf(_)) => Err(Diagnostic::new(
                // TODO: bad error message
                format!(
                    "{} concrete types, but {found_name} is a tuple with possibly unknown fields",
                    msg_restricted_to(expected_name, None)
                ),
                DiagnosticCode::TYPE_DOMAIN,
            )),

            (
                TyParamDomain::TupleFields(found_fields),
                TyParamDomain::TupleFields(expected_fields),
            ) => {
                // TODO: maybe reuse the code from [validate_type_domain]?

                let num_positional = expected_fields
                    .iter()
                    .enumerate()
                    .filter(|(_, f)| f.name.is_none())
                    .next_back()
                    .map(|(p, _)| p + 1)
                    .unwrap_or_default();

                for (position, expected_field) in expected_fields.iter().enumerate() {
                    let (ind_display, ty_field) = if let Some(name) = &expected_field.name {
                        // named
                        let res = found_fields.iter().find(|f| f.name.as_ref() == Some(name));

                        (
                            name.clone(),
                            res.ok_or_else(|| {
                                Diagnostic::new(
                                    format!(
                                        "{} tuples with a field named `{name}`",
                                        msg_restricted_to(expected_name, None)
                                    ),
                                    DiagnosticCode::TYPE_DOMAIN,
                                )
                            })?,
                        )
                    } else {
                        // positional
                        (
                            position.to_string(),
                            found_fields.get(position).ok_or_else(|| {
                                Diagnostic::new(
                                    format!(
                                        "{} tuples with at least {num_positional} fields",
                                        msg_restricted_to(expected_name, None)
                                    ),
                                    DiagnosticCode::TYPE_DOMAIN,
                                )
                            })?,
                        )
                    };

                    if ty_field.ty != expected_field.ty {
                        return Err(Diagnostic::new(
                            format!(
                                "{} {}",
                                msg_restricted_to(expected_name, Some(&ind_display)),
                                expected_field.ty
                            ),
                            DiagnosticCode::TYPE_DOMAIN,
                        ));
                    }

                    // ok
                }

                // all ok
                Ok(())
            }
        }
    }
}

fn msg_restricted_to(arg_name: Option<&str>, suffix: Option<&str>) -> String {
    let Some(arg_name) = arg_name else {
        return "restricted to".to_string();
    };
    if let Some(suffix) = suffix {
        return format!("{arg_name}.{suffix} is restricted to");
    }
    format!("{arg_name} is restricted to")
}

fn compose_type_error<F>(found: &Ty, expected: &Ty, who: &F) -> Diagnostic
where
    F: Fn() -> Option<String>,
{
    fn display_ty(ty: &Ty) -> String {
        if let Some(name) = &ty.name {
            format!("type `{name}`")
        } else {
            format!("type `{}`", printer::print_ty(ty))
        }
    }

    let who = who();
    let is_join = who
        .as_ref()
        .map(|x| x.contains("std.join"))
        .unwrap_or_default();
    let who = who.map(|x| format!("{x} ")).unwrap_or_default();

    let mut e = Diagnostic::new(
        format!(
            "{who}expected {}, but found {}",
            display_ty(expected),
            display_ty(found)
        ),
        DiagnosticCode::TYPE,
    );

    if found.kind.is_func() && !expected.kind.is_func() {
        let to_what = "in this function call?";

        e = e.push_hint(format!("Have you forgotten an argument {to_what}?"));
    }

    if is_join && found.kind.is_tuple() && !expected.kind.is_tuple() {
        e = e.push_hint("Try using `(...)` instead of `{...}`");
    }

    if let Some(expected_name) = &expected.name {
        e = e.push_hint(format!(
            "type `{expected_name}` expands to `{}`",
            printer::print_ty(expected)
        ));
    }
    if let Some(found_name) = &found.name {
        e = e.push_hint(format!(
            "type `{found_name}` expands to `{}`",
            printer::print_ty(found)
        ));
    }
    e
}
