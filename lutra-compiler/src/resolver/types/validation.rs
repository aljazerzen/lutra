use itertools::Itertools;
use std::collections::HashMap;

use crate::diagnostic::{Diagnostic, DiagnosticCode, WithErrorInfo};
use crate::pr::{self, *};
use crate::printer;
use crate::Result;

use super::scope::TyRef;
use super::TypeResolver;

impl TypeResolver<'_> {
    /// Validates that found node has expected type. Returns assumed type of the node.
    pub fn validate_expr_type<F>(
        &mut self,
        found: &mut pr::Expr,
        expected: Option<&Ty>,
        who: &F,
    ) -> Result<(), Diagnostic>
    where
        F: Fn() -> Option<String>,
    {
        let Some(expected) = expected else {
            // expected is none: there is no validation to be done and no generic to be inferred
            return Ok(());
        };

        let Some(found_ty) = &mut found.ty else {
            // found is none: infer from expected
            found.ty = Some(expected.clone());
            return Ok(());
        };

        self.validate_type(found_ty, expected, who)
            .with_span_fallback(found.span)
    }

    /// Validates that found node has expected type. Returns assumed type of the node.
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

        let (found, expected) = match (found_ref, expected_ref) {
            // base case: neither found or expected are generic
            (TyRef::Ty(f), TyRef::Ty(e)) => (f.into_owned(), e.into_owned()),

            // type params
            (TyRef::Param(_, _), TyRef::Ty(expected)) => {
                return Err(compose_type_error(found, &expected, who));
            }
            (TyRef::Param(param_id, param_name), TyRef::Arg(_, arg_id, arg_name)) => {
                // validate
                let scope = self.scopes.last().unwrap();
                let ty_param = self.get_ty_param(param_id);
                let ty_arg = scope.get_ty_arg(&arg_id);
                self.validate_type_domains(ty_param, &ty_arg.domain, &param_name, &arg_name)?;

                // infer
                let scope = self.scopes.last_mut().unwrap();
                scope.infer_type_arg(&arg_id, found.clone());
                return Ok(());
            }
            (TyRef::Param(found_id, _), TyRef::Param(expected_id, _)) => {
                return if found_id == expected_id {
                    Ok(())
                } else {
                    Err(compose_type_error(found, expected, who))
                };
            }

            // I don't know how to construct a test case for this
            (found, TyRef::Param(expected_id, _)) => {
                todo!("found={found:?} expected={expected_id}")
            }

            // type args
            (TyRef::Ty(ty), TyRef::Arg(_, ty_arg_id, ty_arg_name))
            | (TyRef::Arg(_, ty_arg_id, ty_arg_name), TyRef::Ty(ty)) => {
                let ty = ty.into_owned();

                // validate
                let scope = self.scopes.last().unwrap();
                let ty_arg = scope.get_ty_arg(&ty_arg_id);
                self.validate_type_domain(&ty, &ty_arg.domain, &ty_arg_name)?;

                // infer
                let scope = self.scopes.last_mut().unwrap();
                scope.infer_type_arg(&ty_arg_id, ty);
                return Ok(());
            }
            (TyRef::Arg(_, a_id, a_name), TyRef::Arg(_, b_id, b_name)) => {
                // validate
                let scope = self.scopes.last().unwrap();
                let a = scope.get_ty_arg(&a_id);
                let b = scope.get_ty_arg(&b_id);
                if let Some(a_inferred) = a.inferred.get() {
                    self.validate_type_domain(a_inferred, &b.domain, &a_name)?;
                }
                if let Some(b_inferred) = b.inferred.get() {
                    self.validate_type_domain(b_inferred, &a.domain, &b_name)?;
                }

                // infer
                let scope = self.scopes.last_mut().unwrap();
                scope.infer_type_args_equal(a_id, b_id);

                return Ok(());
            }
        };

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
                for (f_variant, e_variant) in itertools::zip_eq(f_variants, e_variants) {
                    // co-variant contained types
                    self.validate_type(&f_variant.ty, &e_variant.ty, who)?;
                }
                Ok(())
            }

            _ => Err(compose_type_error(&found, &expected, who)),
        }
    }

    /// Validates that a type is an a domain of a type param.
    pub fn validate_type_domain(
        &self,
        ty: &Ty,
        domain: &TyParamDomain,
        arg_name: &str,
    ) -> Result<(), Diagnostic> {
        match domain {
            TyParamDomain::Open => Ok(()),

            TyParamDomain::OneOf(possible_tys) => {
                let is_match = ty
                    .kind
                    .as_primitive()
                    .map_or(false, |t| possible_tys.iter().any(|p| t == p));

                if !is_match {
                    let possible_tys = possible_tys.iter().map(|t| t.to_string()).join(", ");

                    return Err(Diagnostic::new(
                        format!(
                            "{arg_name} is restricted to one of {possible_tys}, found {}",
                            printer::print_ty(ty)
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
                            "{arg_name} is restricted to tuples, found {}",
                            printer::print_ty(ty)
                        ),
                        DiagnosticCode::TYPE_DOMAIN,
                    ));
                };

                let num_positional = domain_fields
                    .iter()
                    .enumerate()
                    .filter(|(_, f)| f.name.is_none())
                    .last()
                    .map(|(p, _)| p + 1)
                    .unwrap_or_default();

                for (position, domain_field) in domain_fields.iter().enumerate() {
                    let (ind_display, ty_field) = if let Some(name) = &domain_field.name {
                        // named
                        let res = ty_fields.iter().find(|f| f.name.as_ref() == Some(name));

                        (name.clone(), res.ok_or_else(|| {
                            Diagnostic::new(
                                format!("{arg_name} is restricted to tuples with a field named `{name}`"),
                                DiagnosticCode::TYPE_DOMAIN,
                            )
                        })?)
                    } else {
                        // positional
                        (position.to_string(), ty_fields.get(position).ok_or_else(|| {
                            Diagnostic::new(
                                format!("{arg_name} is restricted to tuples with at least {num_positional} fields"),
                                DiagnosticCode::TYPE_DOMAIN,
                            )
                        })?)
                    };

                    let TyKind::Primitive(ty_field_ty) = &ty_field.ty.kind else {
                        return Err(Diagnostic::new(
                            format!("{arg_name}.{ind_display} is restricted to primitive types"),
                            DiagnosticCode::TYPE_DOMAIN,
                        )
                        .push_hint("This is a temporary restriction. Work in progress."))
                        .with_span(ty_field.ty.span);
                    };

                    if ty_field_ty != &domain_field.ty {
                        return Err(Diagnostic::new(
                            format!(
                                "{arg_name}.{ind_display} is restricted to {}",
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
        expected_name: &str,
    ) -> Result<(), Diagnostic> {
        match (found, expected) {
            // if expected is open, any found domain is ok
            (_, TyParamDomain::Open) => Ok(()),

            // if found is open, expected must be open too (but that was matched above)
            (TyParamDomain::Open, _) => Err(Diagnostic::new(
                format!("{found_name} can be any type, but {expected_name} has restrictions"),
                DiagnosticCode::TYPE_DOMAIN,
            )),

            // each found must be in expected domain
            (TyParamDomain::OneOf(found_tys), expected_domain) => {
                for found_ty in found_tys {
                    let ty = Ty::new(found_ty.clone());
                    self.validate_type_domain(&ty, expected_domain, found_name)?;
                }
                Ok(())
            }

            (TyParamDomain::TupleFields(_), TyParamDomain::OneOf(_)) => Err(Diagnostic::new(
                // TODO: bad error message
                format!("{expected_name} is restricted to concrete types, but {found_name} is a tuple with possibly unknown fields"),
                DiagnosticCode::TYPE_DOMAIN,
            )),

            (TyParamDomain::TupleFields(found_fields), TyParamDomain::TupleFields(expected_fields)) => {
                // TODO: maybe reuse the code from [validate_type_domain]?

                let num_positional = expected_fields
                    .iter()
                    .enumerate()
                    .filter(|(_, f)| f.name.is_none())
                    .last()
                    .map(|(p, _)| p + 1)
                    .unwrap_or_default();

                for (position, expected_field) in expected_fields.iter().enumerate() {
                    let (ind_display, ty_field) = if let Some(name) = &expected_field.name {
                        // named
                        let res = found_fields.iter().find(|f| f.name.as_ref() == Some(name));

                        (name.clone(), res.ok_or_else(|| {
                            Diagnostic::new(
                                format!("{expected_name} is restricted to tuples with a field named `{name}`"),
                                DiagnosticCode::TYPE_DOMAIN,
                            )
                        })?)
                    } else {
                        // positional
                        (position.to_string(), found_fields.get(position).ok_or_else(|| {
                            Diagnostic::new(
                                format!("{expected_name} is restricted to tuples with at least {num_positional} fields"),
                                DiagnosticCode::TYPE_DOMAIN,
                            )
                        })?)
                    };

                    if ty_field.ty != expected_field.ty {
                        return Err(Diagnostic::new(
                            format!(
                                "{expected_name}.{ind_display} is restricted to {}",
                                expected_field.ty
                            ),
                            DiagnosticCode::TYPE_DOMAIN,
                        ));
                    }

                    // ok
                }

                // all ok
                Ok(())
            },
        }
    }
}

fn compose_type_error<F>(found_ty: &Ty, expected: &Ty, who: &F) -> Diagnostic
where
    F: Fn() -> Option<String>,
{
    fn display_ty(ty: &Ty) -> String {
        format!("type `{}`", printer::print_ty(ty))
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
            display_ty(found_ty)
        ),
        DiagnosticCode::TYPE,
    );

    if found_ty.kind.is_func() && !expected.kind.is_func() {
        let to_what = "in this function call?";

        e = e.push_hint(format!("Have you forgotten an argument {to_what}?"));
    }

    if is_join && found_ty.kind.is_tuple() && !expected.kind.is_tuple() {
        e = e.push_hint("Try using `(...)` instead of `{...}`");
    }

    if let Some(expected_name) = &expected.name {
        e = e.push_hint(format!(
            "Type `{expected_name}` expands to `{}`",
            printer::print_ty(expected)
        ));
    }
    e
}
