use crate::Result;
use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::pr;
use crate::resolver::NS_STD;
use crate::utils::IdGenerator;
use crate::utils::fold::{self, PrFold};

use super::Scope;

/// Traverses AST and resolves identifiers.
pub struct NameResolver<'a> {
    pub root: &'a mut pr::ModuleDef,
    pub def_module_path: &'a [String],
    pub scopes: Vec<Scope>,
    pub refs: Vec<pr::Path>,
    pub scope_id_gen: &'a mut IdGenerator<usize>,
}

impl NameResolver<'_> {
    pub fn fold_def_kind(&mut self, def: pr::DefKind) -> Result<pr::DefKind> {
        Ok(match def {
            pr::DefKind::Expr(var_def) => pr::DefKind::Expr(self.fold_var_def(var_def)?),
            pr::DefKind::Ty(ty_def) => pr::DefKind::Ty(self.fold_type_def(ty_def)?),
            pr::DefKind::Import(import_def) => {
                pr::DefKind::Import(self.fold_import_def(import_def)?)
            }
            pr::DefKind::Module(_) => unreachable!(),
            pr::DefKind::Unresolved(_) => unreachable!(),
        })
    }

    pub fn fold_import_def(
        &mut self,
        import_def: pr::ImportDef,
    ) -> Result<pr::ImportDef, Diagnostic> {
        let target = self.resolve_ident(&import_def.target)?;
        let pr::Ref::FullyQualified { to_def, within } = target else {
            panic!()
        };
        if !within.is_empty() {
            panic!();
        }
        Ok(pr::ImportDef { target: to_def })
    }
}

impl fold::PrFold for NameResolver<'_> {
    fn fold_def(&mut self, _def: pr::Def) -> Result<pr::Def> {
        unreachable!()
    }

    fn fold_expr(&mut self, expr: pr::Expr) -> Result<pr::Expr> {
        Ok(match expr.kind {
            pr::ExprKind::Ident(ident) => {
                let target = Some(self.resolve_ident(&ident).with_span(expr.span)?);

                pr::Expr {
                    kind: pr::ExprKind::Ident(ident),
                    target,
                    ..expr
                }
            }
            pr::ExprKind::Indirection { .. } => {
                // special case: indirection might be compiled to a call to std::index,
                // so we add a ref here. This could be conditional.
                self.refs.push(pr::Path::new(vec![NS_STD, "index"]));

                pr::Expr {
                    kind: fold::fold_expr_kind(self, expr.kind)?,
                    ..expr
                }
            }

            pr::ExprKind::Func(func) => {
                let scope_id = self.scope_id_gen.next();
                let scope = Scope::new_of_func(scope_id, &func)?;
                self.scopes.push(scope);
                let func = fold::fold_func(self, *func);
                self.scopes.pop();

                pr::Expr {
                    kind: pr::ExprKind::Func(Box::new(func?)),
                    scope_id: Some(scope_id),
                    ..expr
                }
            }

            pr::ExprKind::Match(match_) => {
                // subject
                let subject = Box::new(self.fold_expr(*match_.subject)?);

                // branches
                let mut branches = Vec::with_capacity(match_.branches.len());
                for branch in match_.branches {
                    let scope_id = self.scope_id_gen.next();
                    let scope = Scope::new_empty(scope_id);
                    self.scopes.push(scope);

                    let pattern = self.fold_pattern(branch.pattern)?;
                    let mut value = Box::new(self.fold_expr(*branch.value)?);

                    self.scopes.pop();
                    value.scope_id = Some(scope_id);

                    branches.push(pr::MatchBranch { pattern, value })
                }

                pr::Expr {
                    kind: pr::ExprKind::Match(pr::Match { subject, branches }),
                    ..expr
                }
            }

            _ => pr::Expr {
                kind: fold::fold_expr_kind(self, expr.kind)?,
                ..expr
            },
        })
    }

    fn fold_pattern(&mut self, mut pattern: pr::Pattern) -> Result<pr::Pattern> {
        match pattern.kind {
            pr::PatternKind::Enum(variant, inner) => {
                // inner
                let inner = if let Some(inner) = inner {
                    Some(Box::new(self.fold_pattern(*inner)?))
                } else {
                    None
                };

                pattern.kind = pr::PatternKind::Enum(variant, inner);
            }
            pr::PatternKind::Bind(ref name) => {
                let scope = self.scopes.last_mut().unwrap();
                scope.insert_local(name);
            }
        }
        Ok(pattern)
    }

    fn fold_type(&mut self, ty: pr::Ty) -> Result<pr::Ty> {
        Ok(match ty.kind {
            pr::TyKind::Ident(ident) => {
                let target = Some(self.resolve_ident(&ident).with_span(ty.span)?);

                pr::Ty {
                    kind: pr::TyKind::Ident(ident),
                    target,
                    ..ty
                }
            }
            pr::TyKind::Func(ty_func) => {
                if self.scopes.is_empty() {
                    let scope_id = self.scope_id_gen.next();
                    let scope = Scope::new_of_ty_func(scope_id, &ty_func)?;
                    self.scopes.push(scope);
                    let r = fold::fold_ty_func(self, ty_func);
                    self.scopes.pop();

                    pr::Ty {
                        kind: pr::TyKind::Func(r?),
                        scope_id: Some(scope_id),
                        ..ty
                    }
                } else {
                    if let Some(param) = ty_func.ty_params.first() {
                        return Err(Diagnostic::new_custom(
                            "generic type parameters are not allowed here",
                        )
                        .with_span(param.span));
                    }
                    let ty = pr::Ty {
                        kind: pr::TyKind::Func(ty_func),
                        ..ty
                    };
                    fold::fold_type(self, ty)?
                }
            }
            _ => fold::fold_type(self, ty)?,
        })
    }
}

impl NameResolver<'_> {
    /// Returns resolved fully-qualified ident
    fn resolve_ident(&mut self, ident: &pr::Path) -> Result<pr::Ref> {
        for scope in self.scopes.iter().rev() {
            if let Some((scope, offset, _)) = scope.get(ident.first()) {
                // match: this ident references a locally-scoped name

                if ident.len() != 1 {
                    return Err(Diagnostic::new_custom(format!(
                        "{} is a param, not a module",
                        ident.first()
                    )));
                }
                return Ok(pr::Ref::Local { scope, offset });
            }
        }

        // find lookup base
        let steps = ident.full_path();
        let (base_path, steps) = match ident.first() {
            "project" => (vec![], &steps[1..]),
            "module" => (self.def_module_path.to_vec(), &steps[1..]),
            "super" => {
                let mut path = self.def_module_path.to_vec();
                path.pop();
                (path, &steps[1..])
            }
            NS_STD => (vec![NS_STD.to_string()], &steps[1..]),
            _ => (self.def_module_path.to_vec(), steps),
        };
        let base_def = self.root.get_submodule(&base_path);

        let res = base_def.map_or(Err(None), |module| module_lookup_steps(module, steps));
        let steps_within = match res {
            Err(err) => {
                tracing::debug!("scopes: {:?}", self.scopes);
                let message = err.unwrap_or_else(|| format!("unknown name {ident}"));
                return Err(Diagnostic::new_custom(message));
            }
            Ok(within) => within,
        };
        let (to_def, within) = steps.split_at(steps.len() - steps_within);

        // prepend the ident with the module path
        // this will make this ident a fully-qualified ident
        let mut fq_ident = pr::Path::new(base_path);
        for step in to_def {
            fq_ident.push(step.clone());
        }

        self.refs.push(fq_ident.clone());

        Ok(pr::Ref::FullyQualified {
            to_def: fq_ident,
            within: pr::Path::new(within),
        })
    }
}

/// Returns true if ok
fn module_lookup_steps(module: &pr::ModuleDef, steps: &[String]) -> Result<usize, Option<String>> {
    if steps.is_empty() {
        // references to modules do not mean anything
        return Err(Some("cannot refer to modules".to_string()));
    }

    let Some(def) = module.defs.get(&steps[0]) else {
        return Err(Some("name does not exist".to_string()));
    };

    match &def.kind {
        // recurse into submodules
        pr::DefKind::Module(sub_module) => module_lookup_steps(sub_module, &steps[1..]),

        // resolve refs into types
        pr::DefKind::Unresolved(Some(def_kind)) => def_lookup_steps(def_kind, &steps[1..]),
        _ => {
            // recursive lookup into self (we take node out of Unresolved during name resolution)

            if steps.len() == 1 {
                return Ok(0);
            }

            // We do not support this, because we want to
            // inline any lookups into types, to keep IR simpler.

            Err(Some("paths into self type are not allowed".into()))
        }
    }
}

fn def_lookup_steps(def: &pr::DefKind, steps: &[String]) -> Result<usize, Option<String>> {
    if steps.is_empty() {
        return Ok(0);
    }

    // into type enum definition (referring to variants)
    match def {
        pr::DefKind::Expr(_) | pr::DefKind::Import(_) => {
            Err(Some("cannot lookup into expressions".to_string()))
        }
        pr::DefKind::Ty(ty_def) => {
            ty_lookup_steps(&ty_def.ty, steps).map_err(|_| None)?;
            Ok(steps.len())
        }
        pr::DefKind::Module(_) | pr::DefKind::Unresolved(_) => unreachable!(),
    }
}

pub fn ty_lookup_steps<'t>(ty: &'t pr::Ty, steps: &[String]) -> Result<&'t pr::Ty, ()> {
    if steps.is_empty() {
        return Ok(ty);
    }
    match &ty.kind {
        pr::TyKind::Enum(variants) => {
            let variant = variants.iter().find(|v| v.name == steps[0]).ok_or(())?;
            ty_lookup_steps(&variant.ty, &steps[1..])
        }
        pr::TyKind::Tuple(fields) => {
            let field = fields
                .iter()
                .find(|f| f.name.as_ref().is_some_and(|n| n == &steps[0]))
                .ok_or(())?;
            ty_lookup_steps(&field.ty, &steps[1..])
        }
        _ => Err(()),
    }
}
