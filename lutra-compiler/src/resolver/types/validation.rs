use indexmap::IndexMap;
use itertools::Itertools;
use std::collections::HashMap;

use crate::Result;
use crate::diagnostic::{Diagnostic, DiagnosticCode, WithErrorInfo};
use crate::pr::{self, *};
use crate::printer;
use crate::resolver::types::scope;

use super::TypeResolver;
use super::scope::TyRef;

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
                self.validate_type_concrete(f.clone(), e.clone(), who)?
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
                return Err(compose_type_error(found, expected, who));
            }
            (TyRef::Param(found_id), TyRef::Param(expected_id)) => {
                if found_id != expected_id {
                    return Err(compose_type_error(found, expected, who));
                }
            }

            // type vars
            (TyRef::Ty(ty), TyRef::Var(_, var_id)) | (TyRef::Var(_, var_id), TyRef::Ty(ty)) => {
                let scope = self.get_ty_var_scope();
                scope.infer_type_var(var_id, ty.clone());
            }
            (TyRef::Var(_, a_id), TyRef::Var(_, b_id)) => {
                let scope = self.get_ty_var_scope();
                scope.infer_type_vars_equal(a_id, b_id);
            }
            (TyRef::Param(..), TyRef::Var(_, var_id)) => {
                // example:
                //     func <T> (x: T) -> twice(x)
                // (twice will create a type var for its arg)

                let scope = self.get_ty_var_scope();
                scope.infer_type_var(var_id, found.clone());
            }
            (TyRef::Var(_, var_id), TyRef::Param(..)) => {
                // example:
                //     func <T> () -> []: [T]
                // (empty array creates a type var and type annotations triggers validation against the param)

                let scope = self.get_ty_var_scope();
                scope.infer_type_var(var_id, expected.clone());
            }
        };
        Ok(())
    }

    /// Validate that an found type matches the expected type.
    /// Both type are concrete: they cannot be identifiers, variables or parameters.
    fn validate_type_concrete<F>(&mut self, found: Ty, expected: Ty, who: &F) -> crate::Result<()>
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
                // here we need to check that all tuple fields match in types (but not necessarily names)

                if found_fields.len() != expected_fields.len() {
                    return Err(compose_type_error(&found, &expected, who));
                }

                let mut last_err = None;
                for (f, e) in std::iter::zip(found_fields.iter(), expected_fields.iter()) {
                    // co-variant contained type
                    let r = self
                        .validate_type(&f.ty, &e.ty, who)
                        .with_span_fallback(f.ty.span);

                    if let Err(d) = r {
                        last_err
                            .take()
                            .map(self.try_push_diagnostic())
                            .transpose()?;
                        last_err = Some(d);
                    }
                }

                if let Some(d) = last_err {
                    Err(d)
                } else {
                    Ok(())
                }
            }

            (TyKind::TupleComprehension(comp), TyKind::Tuple(fields))
            | (TyKind::Tuple(fields), TyKind::TupleComprehension(comp)) => {
                let comp_scope_id = if found.kind.is_tuple_comprehension() {
                    found.scope_id.unwrap()
                } else {
                    expected.scope_id.unwrap()
                };

                // here we need to check:

                // a) the result of comprehension has all fields that are expected
                for (position, e_field) in fields.iter().enumerate() {
                    if e_field.unpack {
                        todo!();
                    }

                    // lookup the field in the comprehended tuple
                    let lookup = pr::Lookup::Position(position as i64);
                    let span = e_field.ty.span.unwrap();
                    let var_input = self.resolve_tuple_lookup(&comp.tuple, &lookup, span)?;

                    // setup scope, so it provides value of the comp.variable_ty
                    let mut scope = scope::Scope::new(comp_scope_id, scope::ScopeKind::Nested);
                    scope.insert_local_ty(var_input);
                    self.scopes.push(scope);

                    // validate comp.body_ty
                    self.validate_type(&comp.body_ty, &e_field.ty, who)?;

                    self.scopes.pop();
                }

                // b) the number of fields matches between the two tuples.
                let domain = pr::TyParamDomain::TupleLen { n: fields.len() };
                self.validate_type_domain(&comp.tuple, &domain, None)?;
                Ok(())
            }
            (TyKind::TupleComprehension(found_comp), TyKind::TupleComprehension(expected_comp)) => {
                self.validate_type(&found_comp.tuple, &expected_comp.tuple, who)?;

                // TODO: validate body_ty
                // self.validate_type(&found_comp.body_ty, &expected_comp.body_ty, who)?;
                Ok(())
            }

            (TyKind::Func(f_func), TyKind::Func(e_func))
                if f_func.params.len() == e_func.params.len() =>
            {
                for ((f_p_ty, f_p_const), (e_p_ty, e_p_const)) in
                    std::iter::zip(&f_func.params, &e_func.params)
                {
                    // if we expect a const param, validate that found param is const
                    if *e_p_const && !*f_p_const {
                        return Err(compose_type_error(&found, &expected, who));
                    }

                    // validate param types
                    if let Some((f_param, e_param)) = Option::zip(f_p_ty.as_ref(), e_p_ty.as_ref())
                    {
                        // contra-variant contained types
                        self.validate_type(e_param, f_param, who)?;
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
                    std::iter::zip(f_variants, e_variants).all(|(f, e)| f.name == e.name);
                if !names_match {
                    return Err(compose_type_error(&found, &expected, who));
                }

                for (f_variant, e_variant) in std::iter::zip(f_variants, e_variants) {
                    // co-variant contained types
                    self.validate_type(&f_variant.ty, &e_variant.ty, who)?;
                }
                Ok(())
            }

            _ => Err(compose_type_error(&found, &expected, who)),
        }
    }

    pub fn finalize_type_vars(&mut self) -> crate::Result<HashMap<pr::Ref, pr::Ty>> {
        let mut known_types = IndexMap::new();
        let mut domains: IndexMap<usize, Vec<pr::TyParamDomain>> = Default::default();
        let mut constraints = {
            let scope = self.scopes.last_mut().unwrap();
            scope.ty_var_constraints.take()
        };

        while !constraints.is_empty() {
            // part 1: consolidate constraints for IsTy and Equals
            while !constraints.is_empty() {
                let mut done_anything = false;
                let mut remaining_constraints = Vec::with_capacity(constraints.len());
                for constraint in constraints {
                    match constraint {
                        scope::TyVarConstraint::IsTy(id, ty) => {
                            self.finalize_var(&mut known_types, id, ty)?;
                            done_anything = true;
                        }
                        scope::TyVarConstraint::Equals(a, b) => {
                            if let Some(ty) = known_types.get(&a).cloned() {
                                self.finalize_var(&mut known_types, b, ty)?;
                                done_anything = true;
                            } else if let Some(ty) = known_types.get(&b).cloned() {
                                self.finalize_var(&mut known_types, a, ty)?;
                                done_anything = true;
                            } else {
                                remaining_constraints.push(constraint);
                            }
                        }

                        scope::TyVarConstraint::InDomain(_, pr::TyParamDomain::Open) => {}
                        scope::TyVarConstraint::InDomain(id, domain) => {
                            let domains = domains.entry(id).or_default();
                            domains.push(domain);
                        }
                    }
                }
                constraints = remaining_constraints;

                if !done_anything {
                    // no constraints were enforced in this loop, error out
                    break;
                }
            }

            // part 2: validate domains of known types
            for (id, ty) in &known_types {
                if let Some(domains) = domains.shift_remove(id) {
                    let var = self.get_ty_var(*id).clone();
                    for domain in domains {
                        self.validate_type_domain(ty, &domain, var.name_hint.as_deref())
                            .with_span_fallback(var.span)?;
                    }
                }
            }

            // validation might have inferred more constraints, which we retrieve here
            let l = constraints.len();
            constraints.extend({
                let scope = self.scopes.last_mut().unwrap();
                scope.ty_var_constraints.take()
            });
            if l < constraints.len() {
                // if any constraints were retrieved, return to step 1
                continue;
            }

            // part 3: consolidate domains into types
            // nothing else worked, so now we try to combine multiple domains into a type
            let mut inferred_anything = false;
            for (id, domains) in &domains {
                // option 1: tuple
                let tuple_len = domains.iter().find_map(|d| match d {
                    TyParamDomain::TupleLen { n } => Some(n),
                    _ => None,
                });
                if let Some(tuple_len) = tuple_len {
                    let mut consolidated = vec![None; *tuple_len];
                    for domain in domains {
                        if let TyParamDomain::TupleHasFields(fields) = domain {
                            for field in fields {
                                match field.location {
                                    Lookup::Position(p) => {
                                        consolidated[p as usize] = Some(field.ty.clone());
                                    }
                                    Lookup::Name(_) => {}
                                }
                            }
                        }
                    }
                    if consolidated.iter().all(Option::is_some) {
                        // success, infer the type
                        let ty = pr::Ty::new(pr::TyKind::Tuple(
                            consolidated
                                .into_iter()
                                .map(|x| pr::TyTupleField {
                                    ty: x.unwrap(),
                                    name: None,
                                    unpack: false,
                                })
                                .collect(),
                        ));

                        tracing::debug!(
                            "consolidated domains to infer {id} is {}",
                            printer::print_ty(&ty)
                        );
                        known_types.insert(*id, ty);
                        inferred_anything = true;
                    }
                }

                // option 2: AnyOf
                // TODO
            }
            if !inferred_anything {
                // nothing worked, stop and raise an error
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
            tracing::debug!("finalized scope {}: {:?}", scope.id, DebugMapping(&mapping));
        }
        Ok(mapping)
    }

    fn finalize_var(
        &mut self,
        known: &mut IndexMap<usize, pr::Ty>,
        id: usize,
        ty: pr::Ty,
    ) -> crate::Result<()> {
        use indexmap::map::Entry;

        let entry = known.entry(id);
        match entry {
            Entry::Occupied(existing) => {
                let ty_var = self.get_ty_var(id);
                let span = ty_var.span;

                self.validate_type(existing.get(), &ty, &|| None)
                    .with_span_fallback(span)?;
            }
            Entry::Vacant(entry) => {
                entry.insert(ty);
            }
        }
        Ok(())
    }

    /// Validates that a type is an a domain of a type param.
    ///
    /// This does two things:
    /// - returns an error if the type is not in the domain,
    /// - infers ty var constraints such that the type is in the domain.
    pub fn validate_type_domain(
        &mut self,
        ty: &Ty,
        domain: &TyParamDomain,
        arg_name: Option<&str>,
    ) -> Result<(), Diagnostic> {
        let ty_ref = self.get_ty_mat(ty)?;
        let ty = match ty_ref {
            TyRef::Ty(cow) => cow,
            TyRef::Param(param_id) => {
                let (found_name, found) = self.get_ty_param(param_id);
                let (found_name, found) = (found_name.clone(), found.clone());
                self.validate_type_domains(&found, domain, &found_name, arg_name)?;
                return Ok(());
            }
            TyRef::Var(_, id) => {
                self.get_ty_var_scope()
                    .infer_type_var_in_domain(id, domain.clone());
                return Ok(());
            }
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
                            msg_restricted_to(arg_name, None),
                            printer::print_ty(ty)
                        ),
                        DiagnosticCode::TYPE_DOMAIN,
                    ));
                }

                Ok(())
            }

            TyParamDomain::TupleHasFields(domain_fields) => {
                let ty = ty.clone();

                for domain_field in domain_fields {
                    let span = domain_field.ty.span.unwrap();
                    let target_ty = self.lookup_in_tuple(&ty, &domain_field.location, span)?;

                    self.validate_type(&target_ty, &domain_field.ty, &|| None)
                        .with_span_fallback(target_ty.span)?;
                    // ok
                }

                // all ok
                Ok(())
            }
            TyParamDomain::TupleLen { n } => {
                fn diagnostic(n: usize, ty: &pr::Ty) -> Diagnostic {
                    Diagnostic::new(
                        format!(
                            "expected a tuple with {n} fields, found {}",
                            printer::print_ty(ty)
                        ),
                        DiagnosticCode::TYPE_DOMAIN,
                    )
                }

                let TyKind::Tuple(fields) = &ty.kind else {
                    return Err(diagnostic(*n, ty));
                };
                if fields.len() != *n {
                    return Err(diagnostic(*n, ty));
                }
                Ok(())
            }

            TyParamDomain::EnumVariants(domain_variants) => {
                let TyKind::Enum(ty_variants) = &ty.kind else {
                    return Err(Diagnostic::new(
                        format!(
                            "{} to enums, found {}",
                            msg_restricted_to(arg_name, None),
                            printer::print_ty(ty)
                        ),
                        DiagnosticCode::TYPE_DOMAIN,
                    ));
                };
                let ty_variants = ty_variants.clone();

                for domain_variant in domain_variants {
                    let (_, variant) =
                        super::pattern::lookup_variant(&ty_variants, &domain_variant.name)?;

                    self.validate_type(&variant.ty, &domain_variant.ty, &|| None)?;
                    // ok
                }

                // all ok
                Ok(())
            }
        }
    }

    /// Validates that found domain is subset of expected domain.
    pub fn validate_type_domains(
        &mut self,
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

            (
                TyParamDomain::TupleHasFields(found_fields),
                TyParamDomain::TupleHasFields(expected_fields),
            ) => {
                for expected_field in expected_fields {
                    let target_ty =
                        Self::lookup_in_tuple_domain(found_fields, &expected_field.location)?;

                    self.validate_type(&target_ty, &expected_field.ty, &|| None)
                        .with_span_fallback(target_ty.span)?;
                }

                // all ok
                Ok(())
            }

            (
                TyParamDomain::EnumVariants(found_variants),
                TyParamDomain::EnumVariants(expected_variants),
            ) => {
                for expected_variant in expected_variants {
                    let (_, found_variant) = super::pattern::lookup_variant_in_domain(
                        found_variants,
                        &expected_variant.name,
                    )?;

                    self.validate_type(&found_variant.ty, &expected_variant.ty, &|| None)?;
                }

                // all ok
                Ok(())
            }

            _ => {
                Err(Diagnostic::new(
                    "incompatible type domain", // TODO: bad error message "
                    DiagnosticCode::TYPE_DOMAIN,
                ))
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

struct DebugMapping<'a>(&'a HashMap<pr::Ref, pr::Ty>);

impl std::fmt::Debug for DebugMapping<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut m = f.debug_map();
        for (key, val) in self.0 {
            let key = match key {
                pr::Ref::Global(pr::AbsoluteRef { to_def, within }) => {
                    if within.is_empty() {
                        format!("{to_def}")
                    } else {
                        format!("{to_def}.{within}")
                    }
                }
                pr::Ref::Local { scope, offset } => format!("{scope}.{offset}"),
            };
            m.entry(&key, &printer::print_ty(val));
        }
        m.finish()
    }
}
