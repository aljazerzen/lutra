use crate::diagnostic::{Diagnostic, DiagnosticCode, WithErrorInfo};
use crate::pr::{self, Ty};
use crate::utils::fold::PrFold;
use crate::{Result, Span, printer};

use super::scope;

impl super::TypeResolver<'_> {
    pub fn resolve_tuple_constructor(&mut self, node: pr::Expr) -> Result<pr::Expr> {
        let pr::ExprKind::Tuple(fields_in) = node.kind else {
            unreachable!()
        };

        let mut fields = Vec::with_capacity(fields_in.len());
        let mut ty_fields: Vec<pr::TyTupleField> = Vec::with_capacity(fields_in.len());

        for f in fields_in {
            let name = (f.name.clone()).or_else(|| self.infer_tuple_field_name(&f.expr));

            let expr = self.fold_expr(f.expr)?;
            let ty = expr.ty.clone().unwrap();

            if f.unpack {
                // validate that ty is a tuple
                let ty_ref = self.get_ty_mat(&ty).unwrap();
                match ty_ref {
                    scope::TyRef::Ty(t) => {
                        if !t.kind.is_tuple() {
                            return Err(Diagnostic::new(
                                "only tuples can be unpacked",
                                DiagnosticCode::TYPE,
                            )
                            .with_span(expr.span)
                            .push_hint(format!("got type {}", printer::print_ty(t))));
                        }
                    }
                    scope::TyRef::Param(id) => {
                        let (p_name, domain) = self.get_ty_param(id);
                        match domain {
                            pr::TyParamDomain::Open
                            | pr::TyParamDomain::OneOf(_)
                            | pr::TyParamDomain::EnumVariants(_) => {
                                return Err(Diagnostic::new(
                                    format!("unpack expected a tuple, found {p_name}"),
                                    DiagnosticCode::TYPE,
                                )
                                .push_hint(format!("{p_name} is not constrained to tuples only")));
                            }
                            pr::TyParamDomain::TupleFields(_) => {
                                // ok
                            }
                        }
                    }
                    scope::TyRef::Var(_, o) => {
                        // restrict var to be a tuple

                        let domain = pr::TyParamDomain::TupleFields(vec![]);
                        let scope = self.get_ty_var_scope();
                        scope.infer_type_var_in_domain(o, domain);
                    }
                };
            }

            fields.push(pr::TupleField {
                name: f.name,
                unpack: f.unpack,
                expr,
            });
            ty_fields.push(pr::TyTupleField {
                name,
                unpack: f.unpack,
                ty,
            });
        }
        let kind = pr::ExprKind::Tuple(fields);
        let ty = pr::Ty::new(pr::TyKind::Tuple(ty_fields));
        Ok(pr::Expr {
            kind,
            ty: Some(ty),
            ..node
        })
    }

    fn infer_tuple_field_name(&self, field: &pr::Expr) -> Option<String> {
        // at this stage, this expr should already be fully resolved
        // this means that any lookups will be tuple positional
        // so we check for that and pull the name from the type of the base

        let pr::ExprKind::TupleLookup {
            base,
            lookup: pr::Lookup::Position(pos),
        } = &field.kind
        else {
            return None;
        };

        let ty = base.ty.as_ref()?;
        self.get_ty_tuple_field_name(ty, *pos as usize)
    }

    fn get_ty_tuple_field_name(&self, ty: &Ty, pos: usize) -> Option<String> {
        // SAFETY: get_my_ty will not error, because it has been resolved earlier already
        let mat_ty = self.get_ty_mat(ty).unwrap();

        match mat_ty {
            super::scope::TyRef::Ty(ty) => match &ty.kind {
                pr::TyKind::Tuple(fields) => {
                    // this tuple might contain Unpacks (which affect positions of fields after them)
                    // so we need to resolve this type full first.

                    // unpacks don't interfere with preceding fields
                    let field = fields.get(pos)?;

                    field.name.clone()
                }

                pr::TyKind::Ident(_fq_ident) => unreachable!(),
                _ => None,
            },
            super::scope::TyRef::Param(..) => None,
            super::scope::TyRef::Var(..) => None,
        }
    }

    /// Resolve tuple lookups to their target type.
    /// For example, `base.lookup` where `base` is a tuple type.
    /// If base is a ty var, it infers tuple constraint.
    ///
    /// Span is the span of the lookup node - the thing that invoked the lookup.
    pub fn resolve_tuple_lookup(
        &mut self,
        base: &Ty,
        lookup: &pr::Lookup,
        span: Span,
    ) -> Result<pr::Ty> {
        let base_ref = self.get_ty_mat(base)?;
        let base = match base_ref {
            scope::TyRef::Ty(b) => b,
            scope::TyRef::Param(id) => {
                return self.lookup_name_in_ty_param(lookup, id);
            }
            scope::TyRef::Var(_, o) => {
                // introduce a new type var for the field
                let field_ty = self.introduce_ty_var(pr::TyParamDomain::Open, span);

                // restrict existing ty var to tuples with this field
                let domain = pr::TyParamDomain::TupleFields(vec![pr::TyDomainTupleField {
                    location: lookup.clone(),
                    ty: field_ty.clone(),
                }]);
                let scope = self.get_ty_var_scope();
                scope.infer_type_var_in_domain(o, domain);

                return Ok(field_ty);
            }
        };
        let base = base.clone();
        self.lookup_in_tuple(&base, lookup)
    }

    /// Takes a concrete tuple and finds the field identifier by [pr::Lookup].
    pub fn lookup_in_tuple(&self, base: &Ty, lookup: &pr::Lookup) -> Result<pr::Ty> {
        let pr::TyKind::Tuple(fields) = &base.kind else {
            return Err(Diagnostic::new(
                format!("lookup expected a tuple, found {}", printer::print_ty(base)),
                DiagnosticCode::TYPE,
            ));
        };

        let r = match lookup {
            pr::Lookup::Name(name) => self.lookup_name_in_tuple(fields, name)?,
            pr::Lookup::Position(pos) => self
                .lookup_position_in_tuple(fields, *pos as usize, 0)
                .map(|x| x.ok())?,
        };
        r.ok_or_else(|| {
            Diagnostic::new(
                format!(
                    "field {} does not exist in type {}",
                    print_lookup(lookup),
                    printer::print_ty(base)
                ),
                DiagnosticCode::TYPE,
            )
        })
    }

    /// Takes a concrete tuple and finds the field by name.
    fn lookup_name_in_tuple(
        &self,
        fields: &[pr::TyTupleField],
        name: &str,
    ) -> Result<Option<pr::Ty>> {
        for field in fields {
            if !field.unpack {
                if field.matches_name(name) {
                    return Ok(Some(field.ty.clone()));
                }
            } else {
                // unpack

                // first handle non-concrete types
                let base_ref = self.get_ty_mat(&field.ty)?;
                let base = match base_ref {
                    scope::TyRef::Ty(b) => b,
                    scope::TyRef::Param(id) => {
                        let lookup = pr::Lookup::Name(name.to_string());
                        return self.lookup_name_in_ty_param(&lookup, id).map(Some);
                    }
                    scope::TyRef::Var(_, _) => {
                        return Err(error_lookup_into_unpack_of_ty_var());
                    }
                };
                let pr::TyKind::Tuple(fields) = &base.kind else {
                    // SAFETY: unpack was checked to be a tuple
                    panic!();
                };
                // recurse
                if let Some(target) = self.lookup_name_in_tuple(fields, name)? {
                    return Ok(Some(target));
                }
            }
        }

        Ok(None)
    }

    /// Takes a concrete tuple and finds the field by position.
    ///
    /// Recurses into unpacked fields.
    /// Takes the number of fields that have been already passed in the lookup.
    /// If the field has not been found, it returns the new number of passed fields.
    fn lookup_position_in_tuple(
        &self,
        ty_fields: &[pr::TyTupleField],
        position: usize,
        passed_parent: usize,
    ) -> Result<Result<pr::Ty, usize>> {
        let mut passed = passed_parent;
        for f in ty_fields {
            if !f.unpack {
                if passed == position {
                    return Ok(Ok(f.ty.clone()));
                }
                passed += 1;
            } else {
                // unpack: we need to look into inner type

                // first we need to handle non-concrete cases
                let ty = self.get_ty_mat(&f.ty).unwrap();
                let ty = match ty {
                    scope::TyRef::Ty(t) => t,
                    scope::TyRef::Param(param_id) => {
                        let pos = position - passed;
                        return self.lookup_position_in_ty_param(param_id, pos).map(Ok);
                    }
                    scope::TyRef::Var(_, _) => {
                        return Err(error_lookup_into_unpack_of_ty_var());
                    }
                };
                let pr::TyKind::Tuple(ty_fields) = &ty.kind else {
                    // SAFETY: field unpack was validated to be a tuple
                    panic!()
                };

                // recurse
                match self.lookup_position_in_tuple(ty_fields, position, passed)? {
                    Ok(t) => return Ok(Ok(t)),
                    Err(p) => passed = p,
                }
            }
        }
        Ok(Err(passed))
    }

    fn lookup_position_in_ty_param(&self, param_id: usize, position: usize) -> Result<Ty> {
        let (param_name, param_domain) = self.get_ty_param(param_id);
        let pr::TyParamDomain::TupleFields(fields) = param_domain else {
            return Err(error_lookup_into_unpack_of_ty_param(param_name));
        };
        let lookup = pr::Lookup::Position(position as i64);

        Self::lookup_in_tuple_domain(fields, &lookup).map_err(|_| {
            // TODO: improve this error message
            Diagnostic::new_custom("cannot do positional lookup into unpack of this type param")
        })
    }

    fn lookup_name_in_ty_param(&self, lookup: &pr::Lookup, id: usize) -> Result<pr::Ty> {
        let (param_name, param) = self.get_ty_param(id);
        let pr::TyParamDomain::TupleFields(fields) = param else {
            return Err(error_lookup_into_unpack_of_ty_param(param_name));
        };

        Self::lookup_in_tuple_domain(fields, lookup)
    }

    pub fn lookup_in_tuple_domain(
        fields: &[pr::TyDomainTupleField],
        lookup: &pr::Lookup,
    ) -> Result<pr::Ty> {
        let Some(field) = fields.iter().find(|f| &f.location == lookup) else {
            return Err(Diagnostic::new(
                format!("field {} does not exist", print_lookup(lookup),),
                DiagnosticCode::TYPE,
            ));
        };
        Ok(field.ty.clone())
    }
}

fn error_lookup_into_unpack_of_ty_var() -> Diagnostic {
    Diagnostic::new_custom("ambiguous lookup into unpack of an unknown type")
        .push_hint("consider annotating the unpacked expression")
}

fn error_lookup_into_unpack_of_ty_param(param_name: &str) -> Diagnostic {
    Diagnostic::new_custom(format!(
        "lookup expected a tuple, found type parameter {param_name}"
    ))
    .push_hint(format!(
        "add `{param_name}: {{}}` to constrain it to tuples"
    ))
}

pub fn print_lookup(lookup: &pr::Lookup) -> String {
    match lookup {
        pr::Lookup::Name(n) => format!(".{}", pr::display_ident(n)),
        pr::Lookup::Position(p) => format!(".{p}",),
    }
}
