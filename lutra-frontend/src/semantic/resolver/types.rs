use std::collections::HashMap;

use itertools::Itertools;

use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::pr::{self, *};
use crate::utils::fold::{self, PrFold};
use crate::{Result, Span};

use super::Resolver;

impl Resolver<'_> {
    /// Visit a type in the main resolver pass.
    // This function is named fold_type_actual, because fold_type must be in
    // expr.rs, where we implement PlFold.
    pub fn fold_type_actual(&mut self, ty: Ty) -> Result<Ty> {
        let mut ty = fold::fold_type(self, ty)?;

        // compute memory layout
        ty.layout = self.compute_ty_layout(&ty)?;
        if ty.layout.is_none() {
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

    pub fn infer_type(&mut self, expr: &Expr) -> Result<Ty> {
        if let Some(ty) = &expr.ty {
            return Ok(ty.clone());
        }

        let kind = match &expr.kind {
            ExprKind::Literal(ref literal) => match literal {
                Literal::Integer(_) => TyKind::Primitive(PrimitiveSet::int),
                Literal::Float(_) => TyKind::Primitive(PrimitiveSet::float),
                Literal::Boolean(_) => TyKind::Primitive(PrimitiveSet::bool),
                Literal::String(_) => TyKind::Primitive(PrimitiveSet::text),
                Literal::RawString(_) => TyKind::Primitive(PrimitiveSet::text),
                _ => panic!(),
            },

            ExprKind::FString(_) => TyKind::Primitive(PrimitiveSet::text),

            ExprKind::Tuple(fields) => {
                let mut ty_fields: Vec<TyTupleField> = Vec::with_capacity(fields.len());

                for field in fields {
                    let ty = self.infer_type(field)?;

                    // if field.flatten {
                    //     let ty = ty.clone();
                    //     match ty.kind {
                    //         TyKind::Tuple(inner_fields) => {
                    //             ty_fields.extend(inner_fields);
                    //         }
                    //         _ => panic!(),
                    //     }

                    //     continue;
                    // }

                    let name = field
                        .alias
                        .clone()
                        .or_else(|| self.infer_tuple_field_name(field));

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
                        todo!()
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
                // generic_type_params: func.generic_type_params.clone(),
            })),

            ExprKind::Ident(_)
            | ExprKind::FuncCall(_)
            | ExprKind::Indirection { .. }
            | ExprKind::Param(_)
            | ExprKind::Pipeline(_) // desugar-ed
            | ExprKind::Range(_) // desugar-ed
            | ExprKind::Binary(_) // desugar-ed
            | ExprKind::Unary(_) // desugar-ed
            | ExprKind::Internal(_) => unreachable!(),
        };
        Ok(Ty {
            kind,
            name: None,
            span: expr.span,
            layout: None,
        })
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
    #[allow(clippy::only_used_in_recursion)]
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
        match (&found.kind, &expected.kind) {
            // base case
            (TyKind::Primitive(f), TyKind::Primitive(e)) if e == f => Ok(()),

            // generics: infer
            (_, TyKind::Ident(_expected_fq)) => {
                // if expected type is a generic, infer that it must be the found type
                todo!();
            }
            (TyKind::Ident(_found_fq), _) => {
                // if found type is a generic, infer that it must be the expected type
                todo!();
            }

            // containers: recurse
            (TyKind::Array(found_items), TyKind::Array(expected_items)) => {
                // co-variant contained type
                self.validate_type(found_items, expected_items, span, who)
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
                    return Err(compose_type_error(found, expected, who).with_span(span));
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
            _ => Err(compose_type_error(found, expected, who).with_span(span)),
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

    let mut e = Diagnostic::new_custom(format!(
        "{who}expected {}, but found {}",
        display_ty(expected),
        display_ty(found_ty)
    ));

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
        ty.kind = match ty.kind {
            TyKind::Ident(ident) => {
                if let Some(new_ty) = self.mapping.get(&ident) {
                    return Ok(new_ty.clone());
                } else {
                    TyKind::Ident(ident)
                }
            }
            _ => return fold::fold_type(self, ty),
        };
        Ok(ty)
    }
}
