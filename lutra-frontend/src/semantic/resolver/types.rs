use std::borrow::Cow;
use std::collections::HashMap;

use itertools::Itertools;

use crate::diagnostic::{Diagnostic, DiagnosticCode, WithErrorInfo};
use crate::pr::{self, *};
use crate::utils::fold::{self, PrFold};
use crate::Result;
use crate::{decl, printer};

use super::scope::{Named, Scope, ScopedKind, TyArgId, TyRef};
use super::Resolver;

impl Resolver<'_> {
    /// Visit a type in the main resolver pass.
    // This function is named fold_type_actual, because fold_type must be in
    // expr.rs, where we implement PlFold.
    pub fn fold_type_actual(&mut self, ty: Ty) -> Result<Ty> {
        // fold inner containers
        let mut ty = match ty.kind {
            TyKind::Function(ty_func) if self.scopes.is_empty() => {
                let mut scope = Scope::new();
                scope.insert_generics_params(&ty_func.ty_params);
                self.scopes.push(scope);
                let ty_func = fold::fold_ty_func(self, ty_func)?;
                self.scopes.pop();

                Ty {
                    kind: TyKind::Function(ty_func),
                    ..ty
                }
            }
            _ => fold::fold_type(self, ty)?,
        };

        // compute memory layout
        let missing_layout = self.compute_ty_layout(&mut ty)?;
        if missing_layout {
            if self.strict_mode {
                return Err(Diagnostic::new_custom(
                    "type has an infinite size due to recursive type references".to_string(),
                )
                .push_hint("add an array or an enum onto the path of recursion")
                .with_span(ty.span));
            } else {
                self.strict_mode_needed = true;
            }
        }

        Ok(ty)
    }

    /// Resolves if type is an ident. Does not recurse.
    pub fn resolve_ty_ident(&self, ty: Ty) -> Result<TyRef<'_>> {
        let TyKind::Ident(ident) = ty.kind else {
            return Ok(TyRef::Ty(Cow::Owned(ty)));
        };
        let named = self.get_ident(&ident).ok_or_else(|| {
            tracing::debug!("scope: {:?}", self.scopes.last().unwrap());
            Diagnostic::new_assert("cannot find type ident")
                .push_hint(format!("ident={ident}"))
                .with_span(ty.span)
        })?;
        match named {
            Named::Decl(decl) => match &decl.kind {
                decl::DeclKind::Ty(t) => Ok(TyRef::Ty(Cow::Borrowed(t))),
                decl::DeclKind::Unresolved(_) => Err(Diagnostic::new_assert(format!(
                    "Unresolved ident at {:?}: (during eval of {})",
                    ty.span, self.debug_current_decl
                ))),
                _ => Err(Diagnostic::new_assert("expected a type")
                    .push_hint(format!("got {:?}", &decl.kind))
                    .with_span(ty.span)),
            },
            Named::Scoped(scoped) => match scoped {
                ScopedKind::Param { ty, .. } => {
                    Err(Diagnostic::new_assert("expected a type, found a value")
                        .push_hint(format!("got param of type `{}`", printer::print_ty(&ty)))
                        .with_span(ty.span))
                }
                ScopedKind::Type { ty } => self.resolve_ty_ident(ty),
                ScopedKind::TypeParam { .. } => Ok(TyRef::Param(ident.name().to_string())),
                ScopedKind::TypeArg { id } => Ok(TyRef::Arg(id)),
            },
        }
    }

    pub fn infer_type(&mut self, expr: &Expr) -> Result<Ty> {
        if let Some(ty) = &expr.ty {
            return Ok(ty.clone());
        }

        let kind = match &expr.kind {
            ExprKind::Literal(ref literal) => match literal {
                Literal::Integer(_) => TyKind::Primitive(PrimitiveSet::int64),
                Literal::Float(_) => TyKind::Primitive(PrimitiveSet::float64),
                Literal::Boolean(_) => TyKind::Primitive(PrimitiveSet::bool),
                Literal::Text(_) => TyKind::Primitive(PrimitiveSet::text),
                _ => panic!(),
            },

            ExprKind::FString(_) => TyKind::Primitive(PrimitiveSet::text),

            ExprKind::Tuple(fields) => {
                let mut ty_fields: Vec<TyTupleField> = Vec::with_capacity(fields.len());

                for field in fields {
                    let ty = self.infer_type(&field.expr)?;

                    let name = field
                        .name
                        .clone()
                        .or_else(|| self.infer_tuple_field_name(&field.expr));

                    ty_fields.push(TyTupleField { name, ty });
                }
                ty_tuple_kind(ty_fields)
            }
            ExprKind::Array(items) => {
                let mut variants = Vec::with_capacity(items.len());
                for item in items {
                    let item_ty = self.infer_type(item)?;
                    variants.push(item_ty);
                }
                let items_ty = match variants.len() {
                    0 => {
                        // no items, so we must infer the type
                        // TODO
                        let mut ty = Ty::new(PrimitiveSet::int64);
                        ty.layout = ty.kind.get_layout_simple();
                        ty
                    }
                    1 => {
                        // single item, use its type
                        variants.into_iter().exactly_one().unwrap()
                    }
                    2.. => {
                        // ideally, we would enforce that all of items have
                        // the same type, but currently we don't have a good
                        // strategy for dealing with nullable types, which
                        // causes problems here.
                        // HACK: use only the first type
                        variants.into_iter().next().unwrap()
                    }
                };
                TyKind::Array(Box::new(items_ty))
            }

            // ExprKind::All { within, except } => {
            //     let Some(within_ty) = self.infer_type(within)? else {
            //         return Ok(None);
            //     };
            //     let Some(except_ty) = self.infer_type(except)? else {
            //         return Ok(None);
            //     };
            //     self.ty_tuple_exclusion(within_ty, except_ty)?
            // }
            ExprKind::Case(cases) => {
                let case_tys: Vec<Ty> = cases
                    .iter()
                    .map(|c| self.infer_type(&c.value))
                    .try_collect()?;

                let Some(inferred_ty) = case_tys.first() else {
                    return Err(Diagnostic::new_custom(
                        "cannot infer type of any of the branches of this case statement",
                    )
                    .with_span(expr.span));
                };

                return Ok(inferred_ty.clone());
            }

            ExprKind::Func(func) => TyKind::Function(TyFunc {
                params: func.params.iter().map(|p| p.ty.clone()).collect_vec(),
                body: func
                    .return_ty
                    .clone()
                    .or_else(|| func.body.ty.clone())
                    .map(Box::new),
                ty_params: func.ty_params.clone(),
            }),

            ExprKind::Ident(_)
            | ExprKind::FuncCall(_)
            | ExprKind::Indirection { .. }
            | ExprKind::Pipeline(_) // desugar-ed
            | ExprKind::Range(_) // desugar-ed
            | ExprKind::Binary(_) // desugar-ed
            | ExprKind::Unary(_) // desugar-ed
            | ExprKind::Internal => unreachable!(),
        };
        let mut ty = Ty {
            kind,
            name: None,
            span: expr.span,
            layout: None,
        };
        let missing_layout = self.compute_ty_layout(&mut ty)?;
        if missing_layout {
            return Err(Diagnostic::new_assert("missing type layout")
                .push_hint(format!("ty: {}", printer::print_ty(&ty)))
                .with_span(ty.span));
        }
        Ok(ty)
    }

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
        let found_ref = self.resolve_ty_ident(found.clone())?;
        let expected_ref = self.resolve_ty_ident(expected.clone())?;

        let (found, expected) = match (found_ref, expected_ref) {
            // base case: neither found or expected are generic
            (TyRef::Ty(f), TyRef::Ty(e)) => (f.into_owned(), e.into_owned()),

            // type params
            (TyRef::Param(_), TyRef::Ty(expected)) => {
                return Err(compose_type_error(found, &expected, who));
            }
            (TyRef::Param(param_id), TyRef::Arg(arg_id)) => {
                // validate
                let scope = self.scopes.last().unwrap();
                let ty_param = self.get_ty_param(&param_id);
                let ty_arg = scope.get_ty_arg(&arg_id);
                self.validate_type_domains(ty_param, &ty_arg.domain, &param_id, &arg_id.name)?;

                // infer
                let scope = self.scopes.last_mut().unwrap();
                scope.infer_type_arg(&arg_id, found.clone());
                return Ok(());
            }
            (TyRef::Param(found_id), TyRef::Param(expected_id)) => {
                return if found_id == expected_id {
                    Ok(())
                } else {
                    Err(compose_type_error(found, expected, who))
                };
            }

            // I don't know how to construct a test case for this
            (found, TyRef::Param(expected_id)) => todo!("found={found:?} expected={expected_id}"),

            // type args
            (TyRef::Ty(ty), TyRef::Arg(ty_arg_id)) | (TyRef::Arg(ty_arg_id), TyRef::Ty(ty)) => {
                let ty = ty.into_owned();

                // validate
                let scope = self.scopes.last().unwrap();
                let ty_arg = scope.get_ty_arg(&ty_arg_id);
                self.validate_type_domain(&ty, &ty_arg.domain, &ty_arg_id.name)?;

                // infer
                let scope = self.scopes.last_mut().unwrap();
                scope.infer_type_arg(&ty_arg_id, ty);
                return Ok(());
            }
            (TyRef::Arg(a_id), TyRef::Arg(b_id)) => {
                // validate
                let scope = self.scopes.last().unwrap();
                let a = scope.get_ty_arg(&a_id);
                let b = scope.get_ty_arg(&b_id);
                if let Some(a_inferred) = a.inferred.get() {
                    self.validate_type_domain(a_inferred, &b.domain, &a_id.name)?;
                }
                if let Some(b_inferred) = b.inferred.get() {
                    self.validate_type_domain(b_inferred, &a.domain, &b_id.name)?;
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
            (TyKind::Function(f_func), TyKind::Function(e_func))
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

    fn infer_tuple_field_name(&self, field: &Expr) -> Option<String> {
        // at this stage, this expr should already be fully resolved
        // this means that any indirections will be tuple positional
        // so we check for that and pull the name from the type of the base

        let ExprKind::Indirection {
            base,
            field: IndirectionKind::Position(pos),
        } = &field.kind
        else {
            return None;
        };

        let ty = base.ty.as_ref()?;
        self.apply_ty_tuple_indirection(ty, *pos as usize)
    }

    fn apply_ty_tuple_indirection(&self, ty: &Ty, pos: usize) -> Option<String> {
        match &ty.kind {
            TyKind::Tuple(fields) => {
                // this tuple might contain Unpacks (which affect positions of fields after them)
                // so we need to resolve this type full first.

                // unpacks don't interfere with preceding fields
                let field = fields.get(pos)?;

                field.name.clone()
            }

            TyKind::Ident(_fq_ident) => {
                todo!()
            }

            _ => None,
        }
    }

    /// Add type's params into scope as type arguments.
    pub fn introduce_ty_into_scope(&mut self, ty: Ty) -> Ty {
        let TyKind::Function(mut ty_func) = ty.kind else {
            return ty;
        };

        // TODO: recurse? There might be type params deeper in the type.

        if ty_func.ty_params.is_empty() {
            return Ty {
                kind: TyKind::Function(ty_func),
                ..ty
            };
        }

        let expr_id = self.id.gen();
        tracing::debug!(
            "introducing generics with expr_id={expr_id}, ty_func={}",
            crate::printer::print_ty(&Ty::new(ty_func.clone()))
        );

        let mut mapping = HashMap::new();
        let scope = self.scopes.last_mut().unwrap();
        for gtp in ty_func.ty_params.drain(..) {
            mapping.insert(
                Path::new(vec!["scope", gtp.name.as_str()]),
                Ty::new(Path::new(vec![
                    "scope".to_string(),
                    "type_args".to_string(),
                    expr_id.to_string(),
                    gtp.name.clone(),
                ])),
            );

            let type_arg_id = TyArgId {
                expr_id,
                name: gtp.name,
            };
            scope.insert_generic_arg(type_arg_id, gtp.domain);
        }

        let ty = Ty {
            kind: TyKind::Function(ty_func),
            ..ty
        };
        TypeReplacer::on_ty(ty, mapping)
    }
}

pub fn ty_tuple_kind(fields: Vec<TyTupleField>) -> TyKind {
    let mut res: Vec<TyTupleField> = Vec::with_capacity(fields.len());
    for field in fields {
        let TyTupleField { name, .. } = &field;

        // remove names from previous fields with the same name
        if name.is_some() {
            for f in res.iter_mut() {
                if f.name.as_ref() == name.as_ref() {
                    f.name = None;
                }
            }
        }

        res.push(field);
    }
    TyKind::Tuple(res)
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

    if found_ty.kind.is_function() && !expected.kind.is_function() {
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

pub struct TypeReplacer {
    mapping: HashMap<Path, Ty>,
}

impl TypeReplacer {
    #[tracing::instrument(name = "TypeReplacer", skip_all)]
    pub fn on_ty(ty: Ty, mapping: HashMap<Path, Ty>) -> Ty {
        TypeReplacer { mapping }.fold_type(ty).unwrap()
    }

    #[tracing::instrument(name = "TypeReplacer", skip_all)]
    pub fn on_func(func: pr::Func, mapping: HashMap<Path, Ty>) -> pr::Func {
        TypeReplacer { mapping }.fold_func(func).unwrap()
    }
}

impl PrFold for TypeReplacer {
    fn fold_type(&mut self, mut ty: Ty) -> Result<Ty> {
        match ty.kind {
            TyKind::Ident(ident) => {
                if let Some(new_ty) = self.mapping.get(&ident) {
                    let ty = new_ty.clone();
                    self.fold_type(ty)
                } else {
                    ty.kind = TyKind::Ident(ident);
                    Ok(ty)
                }
            }
            _ => fold::fold_type(self, ty),
        }
    }
}

pub struct TypeLayoutResolver<'a, 'b> {
    resolver: &'a mut super::Resolver<'b>,
}

impl<'a, 'b> TypeLayoutResolver<'a, 'b> {
    #[tracing::instrument(name = "TypeLayoutResolver", skip_all)]
    pub fn on_func(func: pr::Func, resolver: &'a mut super::Resolver<'b>) -> Result<pr::Func> {
        TypeLayoutResolver { resolver }.fold_func(func)
    }
}

impl<'a, 'b> PrFold for TypeLayoutResolver<'a, 'b> {
    fn fold_type(&mut self, ty: Ty) -> Result<Ty> {
        // Don't re-resolve idents
        // I do this because I don't want to set up proper scopes.
        // It might be needed.
        if let TyKind::Ident(_) = &ty.kind {
            return Ok(ty);
        }

        let mut ty = fold::fold_type(self, ty)?;
        self.resolver.compute_ty_layout(&mut ty)?;
        Ok(ty)
    }
}
