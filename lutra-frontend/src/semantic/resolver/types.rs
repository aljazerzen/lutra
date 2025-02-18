use std::borrow::Cow;
use std::collections::HashMap;

use itertools::Itertools;

use crate::diagnostic::{Diagnostic, DiagnosticCode, WithErrorInfo};
use crate::pr::{self, *};
use crate::utils::fold::{self, PrFold};
use crate::{decl, Result, Span};

use super::scope::{Named, Scope, ScopedKind, TyArgId, TyRef};
use super::Resolver;

impl Resolver<'_> {
    /// Visit a type in the main resolver pass.
    // This function is named fold_type_actual, because fold_type must be in
    // expr.rs, where we implement PlFold.
    pub fn fold_type_actual(&mut self, ty: Ty) -> Result<Ty> {
        // fold inner containers
        let mut ty = match ty.kind {
            TyKind::Function(Some(ty_func)) if self.scopes.is_empty() => {
                let mut scope = Scope::new();
                scope.insert_generics_params(&ty_func.ty_params);
                self.scopes.push(scope);
                let ty_func = fold::fold_ty_func(self, ty_func)?;
                self.scopes.pop();

                Ty {
                    kind: TyKind::Function(Some(ty_func)),
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
    pub fn resolve_ty_ident<'t>(&'t self, ty: &'t Ty) -> Result<TyRef<'t>> {
        let TyKind::Ident(ident) = &ty.kind else {
            return Ok(TyRef::Ty(Cow::Borrowed(ty)));
        };
        let named = self.get_ident(ident).ok_or_else(|| {
            log::debug!("scope: {:?}", self.scopes.last().unwrap());
            Diagnostic::new_assert("cannot find type ident")
                .push_hint(format!("ident={ident:?}"))
                .with_span(ty.span)
        })?;
        match named {
            Named::Decl(decl) => match &decl.kind {
                decl::DeclKind::Ty(t) => Ok(TyRef::Ty(Cow::Borrowed(t))),
                decl::DeclKind::Unresolved(_) => Err(Diagnostic::new_assert(format!(
                    "Unresolved ident at {:?}: (during eval of {})",
                    ty.span, self.debug_current_decl
                ))),
                _ => Err(Diagnostic::new_assert("expected reference to a type")
                    .push_hint(format!("got {:?}", &decl.kind))
                    .with_span(ty.span)),
            },
            Named::Scoped(scoped) => match scoped {
                ScopedKind::Param { ty, .. } => {
                    Err(Diagnostic::new_assert("expected type found an expression")
                        .push_hint(format!("got {:?}", &ty))
                        .with_span(ty.span))
                }
                ScopedKind::Type { ty } => Ok(TyRef::Ty(Cow::Owned(ty))),
                ScopedKind::TypeParam { .. } => Ok(TyRef::Param(ident.name())),
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

            ExprKind::Func(func) => TyKind::Function(Some(TyFunc {
                params: func.params.iter().map(|p| p.ty.clone()).collect_vec(),
                body: func
                    .return_ty
                    .clone()
                    .or_else(|| func.body.ty.clone())
                    .map(Box::new),
                ty_params: func.ty_params.clone(),
            })),

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
                .push_hint(format!("ty: {ty:?}"))
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

        self.validate_type(found_ty, expected, found.span, who)
    }

    /// Validates that found node has expected type. Returns assumed type of the node.
    pub fn validate_type<F>(
        &mut self,
        found: &Ty,
        expected: &Ty,
        span: Option<Span>,
        who: &F,
    ) -> Result<(), Diagnostic>
    where
        F: Fn() -> Option<String>,
    {
        log::trace!("validate_type, \nf: {found:?}, \ne: {expected:?}");
        let found = self.resolve_ty_ident(found)?;
        let expected = self.resolve_ty_ident(expected)?;

        let (found, expected) = match (found, expected) {
            // base case: neither found or expected are generic
            (TyRef::Ty(f), TyRef::Ty(e)) => (f.into_owned(), e.into_owned()),

            // type params
            (TyRef::Param(param_id), TyRef::Ty(ty)) | (TyRef::Ty(ty), TyRef::Param(param_id)) => {
                let ty_param = self.get_ty_param(param_id);
                self.validate_type_domain(ty.as_ref(), ty_param, param_id)?;

                return Ok(());
            }
            (TyRef::Param(_), TyRef::Param(_)) => {
                return Ok(());
            }
            (TyRef::Param(_), TyRef::Arg(_)) | (TyRef::Arg(_), TyRef::Param(_)) => {
                todo!();
            }

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
            // base case
            (TyKind::Primitive(f), TyKind::Primitive(e)) if e == f => Ok(()),

            // containers: recurse
            (TyKind::Array(found_items), TyKind::Array(expected_items)) => {
                // co-variant contained type
                self.validate_type(&found_items.clone(), &expected_items.clone(), span, who)
            }
            (TyKind::Tuple(found_fields), TyKind::Tuple(expected_fields)) => {
                // here we need to check that found tuple has all fields that are expected.

                // build index of found fields
                let found_types: HashMap<_, _> = found_fields
                    .iter()
                    .filter_map(|e| e.name.as_ref().map(|n| (n, &e.ty)))
                    .collect();

                let mut expected_but_not_found = Vec::new();
                for e_field in expected_fields {
                    if let Some(e_name) = &e_field.name {
                        // when a named field is expected

                        // if it was found
                        if let Some(f_ty) = found_types.get(e_name) {
                            // check its type

                            // co-variant contained type
                            self.validate_type(f_ty, &e_field.ty, span, who)?;
                        } else {
                            expected_but_not_found.push(e_field);
                        }
                    } else {
                        // TODO: positional expected fields
                    }
                }

                if !expected_but_not_found.is_empty() {
                    // not all fields were found
                    return Err(compose_type_error(&found, &expected, who).with_span(span));
                }

                Ok(())
            }
            (TyKind::Function(Some(f_func)), TyKind::Function(Some(e_func)))
                if f_func.params.len() == e_func.params.len() =>
            {
                for (f_arg, e_arg) in itertools::zip_eq(&f_func.params, &e_func.params) {
                    if let Some((f_arg, e_arg)) = Option::zip(f_arg.as_ref(), e_arg.as_ref()) {
                        // contra-variant contained types
                        self.validate_type(e_arg, f_arg, span, who)?;
                    }
                }

                // return types
                if let Some((f_ret, e_ret)) =
                    Option::zip(Option::as_ref(&f_func.body), Option::as_ref(&e_func.body))
                {
                    // co-variant contained type
                    self.validate_type(f_ret, e_ret, span, who)?;
                }
                Ok(())
            }
            _ => Err(compose_type_error(&found, &expected, who).with_span(span)),
        }
    }

    /// Validates that found node has expected type. Returns assumed type of the node.
    pub fn validate_type_domain(
        &self,
        ty: &Ty,
        domain: &TyParamDomain,
        param_name: &str,
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
                            "{param_name} is restricted to one of {possible_tys}, found {ty:?}"
                        ),
                        DiagnosticCode::TYPE_DOMAIN,
                    ));
                }

                Ok(())
            }

            TyParamDomain::TupleFields(domain_fields) => {
                let TyKind::Tuple(ty_fields) = &ty.kind else {
                    return Err(Diagnostic::new(
                        format!("{param_name} is restricted to tuples, found {ty:?}"),
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
                                format!("{param_name} is restricted to tuples with a field named `{name}`"),
                                DiagnosticCode::TYPE_DOMAIN,
                            )
                        })?)
                    } else {
                        // positional
                        (position.to_string(), ty_fields.get(position).ok_or_else(|| {
                            Diagnostic::new(
                                format!("{param_name} is restricted to tuples with at least {num_positional} fields"),
                                DiagnosticCode::TYPE_DOMAIN,
                            )
                        })?)
                    };

                    let TyKind::Primitive(ty_field_ty) = &ty_field.ty.kind else {
                        return Err(Diagnostic::new(
                            format!("{param_name}.{ind_display} is restricted to primitive types"),
                            DiagnosticCode::TYPE_DOMAIN,
                        )
                        .push_hint("This is a temporary restriction. Work in progress."));
                    };

                    if ty_field_ty != &domain_field.ty {
                        return Err(Diagnostic::new(
                            format!(
                                "{param_name}.{ind_display} is restricted to {}",
                                domain_field.ty
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

    // /// Instantiate generic type parameters into generic type arguments.
    // ///
    // /// When resolving a type of reference to a variable, we cannot just use the type
    // /// of the variable as the type of the reference. That's because the variable might contain
    // /// generic type arguments that need to differ between references to the same variable.
    // ///
    // /// For example:
    // /// ```prql
    // /// let plus_one = func <T> x<T> -> <T> x + 1
    // ///
    // /// let a = plus_one 1
    // /// let b = plus_one 1.5
    // /// ```
    // ///
    // /// Here, the first reference to `plus_one` must resolve with T=int and the second with T=float.
    // ///
    // /// This struct makes sure that distinct instanced of T are created from generic type param T.
    // pub fn instantiate_type(&mut self, ty: Ty, id: usize) -> Ty {
    //     let TyKind::Function(Some(ty_func)) = &ty.kind else {
    //         return ty;
    //     };
    //     if ty_func.generic_type_params.is_empty() {
    //         return ty;
    //     }
    //     let prev_scope = Ident::from_path(vec![NS_LOCAL]);
    //     let new_scope = Ident::from_path(vec![NS_GENERIC.to_string(), id.to_string()]);

    //     let mut ident_mapping: HashMap<Ident, Ty> =
    //         HashMap::with_capacity(ty_func.generic_type_params.len());

    //     for gtp in &ty_func.generic_type_params {
    //         let new_ident = new_scope.clone() + Ident::from_name(&gtp.name);

    //         // TODO: this should create GenericArg, not GenericParam
    //         let decl = Decl::from(DeclKind::GenericArg(GenericParam {
    //             domain: gtp.domain,
    //             bounds: Vec::new(),
    //         }));
    //         self.root_mod
    //             .module
    //             .insert(new_ident.clone(), decl)
    //             .unwrap();

    //         ident_mapping.insert(
    //             prev_scope.clone() + Ident::from_name(&gtp.name),
    //             Ty::new(TyKind::Ident(new_ident)),
    //         );
    //     }

    //     TypeReplacer::on_ty(ty, ident_mapping)
    // }

    // pub fn ty_tuple_exclusion(&self, base: Ty, except: Ty) -> Result<TyKind> {
    //     let mask = self.ty_tuple_exclusion_mask(&base, &except)?;

    //     let new_fields = itertools::zip_eq(base.kind.as_tuple().unwrap(), mask)
    //         .filter(|(_, p)| *p)
    //         .map(|(x, _)| x.clone())
    //         .collect();

    //     Ok(TyKind::Tuple(new_fields))
    // }

    // /// Computes the "field mask", which is a vector of booleans indicating if a field of
    // /// base tuple type should appear in the resulting type.
    // ///
    // /// Returns `None` if:
    // /// - base or exclude is a generic type argument, or
    // /// - either of the types contains Unpack.
    // pub fn ty_tuple_exclusion_mask(&self, base: &Ty, except: &Ty) -> Result<Vec<bool>> {
    //     let within_fields = match &base.kind {
    //         TyKind::Tuple(f) => f,

    //         // this is a generic, exclusion cannot be inlined
    //         TyKind::Ident(_) => todo!(),

    //         _ => {
    //             return Err(
    //                 Diagnostic::new_simple("fields can only be excluded from a tuple")
    //                     .with_span(base.span),
    //             )
    //         }
    //     };

    //     let except_fields = match &except.kind {
    //         TyKind::Tuple(f) => f,

    //         // this is a generic, exclusion cannot be inlined
    //         TyKind::Ident(_) => todo!(),

    //         _ => {
    //             return Err(Diagnostic::new_simple("expected excluded fields to be a tuple")
    //                 .with_span(except.span));
    //         }
    //     };

    //     let except_fields: HashSet<&String> = except_fields
    //         .iter()
    //         .map(|field| match &field.name {
    //             Some(name) => Ok(name),
    //             None => Err(Diagnostic::new_simple("excluded fields must be named")),
    //         })
    //         .collect::<Result<_>>()
    //         .with_span(except.span)?;

    //     let mut mask = Vec::new();
    //     for field in within_fields {
    //         if let Some(name) = &field.name {
    //             mask.push(!except_fields.contains(&name));
    //         } else {
    //             mask.push(true);
    //         }
    //     }
    //     Ok(mask)
    // }

    /// Add type's params into scope as type arguments.
    pub fn introduce_ty_into_scope(&mut self, ty: Ty) -> Ty {
        let TyKind::Function(Some(mut ty_func)) = ty.kind else {
            return ty;
        };

        // TODO: recurse? There might be type params deeper in the type.

        if ty_func.ty_params.is_empty() {
            return Ty {
                kind: TyKind::Function(Some(ty_func)),
                ..ty
            };
        }

        let expr_id = self.id.gen();
        log::debug!("introducing generics for {expr_id} {ty_func:?}");

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
            kind: TyKind::Function(Some(ty_func)),
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
        format!("type `{:?}`", ty)
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
            "Type `{expected_name}` expands to `{:?}`",
            expected.kind
        ));
    }
    e
}

pub struct TypeReplacer {
    mapping: HashMap<Path, Ty>,
}

#[allow(dead_code)]
impl TypeReplacer {
    pub fn on_ty(ty: Ty, mapping: HashMap<Path, Ty>) -> Ty {
        TypeReplacer { mapping }.fold_type(ty).unwrap()
    }

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
