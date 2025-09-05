use crate::Result;
use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::pr;
use crate::resolver::NS_STD;
use crate::utils::IdGenerator;
use crate::utils::fold::{self, PrFold};

use super::Scope;

/// Traverses AST and resolves identifiers.
pub struct NameResolver<'a> {
    pub root: &'a pr::ModuleDef,
    pub def_module_path: &'a [String],
    pub scopes: Vec<Scope>,
    pub refs: Vec<pr::Path>,
    pub scope_id_gen: &'a mut IdGenerator<usize>,
    pub allow_recursive: bool,
}

impl NameResolver<'_> {
    pub fn fold_def_kind(&mut self, def: pr::DefKind) -> Result<pr::DefKind> {
        Ok(match def {
            pr::DefKind::Expr(var_def) => pr::DefKind::Expr(self.fold_expr_def(var_def)?),
            pr::DefKind::Ty(ty_def) => pr::DefKind::Ty(self.fold_type_def(ty_def)?),

            pr::DefKind::Module(_) | pr::DefKind::Import(_) | pr::DefKind::Unresolved(_) => {
                unreachable!()
            }
        })
    }

    pub fn fold_import_def(
        &mut self,
        import_def: pr::ImportDef,
    ) -> Result<pr::ImportDef, Diagnostic> {
        let fq = self.lookup_in_root(self.def_module_path, &import_def.target)?;
        if !fq.within.is_empty() {
            todo!();
        }
        Ok(pr::ImportDef { target: fq.to_def })
    }
}

impl fold::PrFold for NameResolver<'_> {
    fn fold_def(&mut self, _def: pr::Def) -> Result<pr::Def> {
        unreachable!()
    }

    fn fold_expr(&mut self, expr: pr::Expr) -> Result<pr::Expr> {
        Ok(match expr.kind {
            pr::ExprKind::Ident(ident) => {
                let target = Some(self.lookup_ident(&ident).with_span_fallback(expr.span)?);

                pr::Expr {
                    kind: pr::ExprKind::Ident(ident),
                    target,
                    ..expr
                }
            }
            pr::ExprKind::TupleLookup { .. } => pr::Expr {
                kind: fold::fold_expr_kind(self, expr.kind)?,
                ..expr
            },

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

    fn fold_pattern(&mut self, pattern: pr::Pattern) -> Result<pr::Pattern> {
        let binds = collect_pattern_binds(&pattern)?;

        let scope = self.scopes.last_mut().unwrap();
        for b in binds {
            scope.insert_local(b);
        }

        Ok(pattern)
    }

    fn fold_type(&mut self, ty: pr::Ty) -> Result<pr::Ty> {
        Ok(match ty.kind {
            pr::TyKind::Ident(ident) => {
                let target = Some(self.lookup_ident(&ident).with_span_fallback(ty.span)?);

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
            pr::TyKind::TupleComprehension(ref comp) => {
                // validate that variable name is equal to body name
                // This restricts the generality of comprehension
                // (variable name cannot be used in some inner comprehension)
                // but for now, that generality is not needed.
                if comp
                    .body_name
                    .as_ref()
                    .is_some_and(|b| b != &comp.variable_name)
                {
                    return Err(Diagnostic::new_custom(format!(
                        "expected field to be named {}",
                        comp.variable_name
                    )));
                }

                // new scope that for comp.variable_ty
                let scope_id = self.scope_id_gen.next();
                let mut scope = Scope::new_empty(scope_id);
                scope.insert_ty_local(&comp.variable_ty);
                self.scopes.push(scope);

                // fold
                let mut ty = fold::fold_type(self, ty)?;
                ty.scope_id = Some(scope_id);

                self.scopes.pop();

                ty
            }
            _ => fold::fold_type(self, ty)?,
        })
    }
}

impl NameResolver<'_> {
    /// Returns resolved fully-qualified ident
    fn lookup_ident(&mut self, ident: &pr::Path) -> Result<pr::Ref> {
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

        let fq = (self.lookup_in_root(self.def_module_path, ident)).inspect_err(|_| {
            tracing::debug!("scopes: {:?}", self.scopes);
        })?;
        self.refs.push(fq.to_def.clone());

        Ok(pr::Ref::FullyQualified {
            to_def: fq.to_def,
            within: fq.within,
        })
    }

    #[tracing::instrument("lookup", skip_all, fields(from = pr::Path::new(def_mod_fq).to_string()))]
    fn lookup_in_root(&self, def_mod_fq: &[String], ident: &pr::Path) -> Result<FullyQualified> {
        tracing::debug!("lookup for {ident}");

        // find lookup base
        let steps = ident.as_steps();
        let (base_path, relative) = match ident.first() {
            "project" => (pr::Path::empty(), pr::Path::new(&steps[1..])),
            "module" => (pr::Path::new(def_mod_fq), pr::Path::new(&steps[1..])),
            "super" => {
                let mut path = pr::Path::new(def_mod_fq);
                path.pop();
                (path, pr::Path::new(&steps[1..]))
            }
            NS_STD => (pr::Path::from_name(NS_STD), pr::Path::new(&steps[1..])),
            _ => (pr::Path::new(def_mod_fq), pr::Path::new(steps)),
        };

        let base_def = (self.root.get_submodule(base_path.as_steps()))
            .ok_or_else(|| Diagnostic::new_custom("unknown name"))?;

        self.lookup_in_module(base_def, base_path, relative)
    }

    fn lookup_in_module(
        &self,
        base_mod: &pr::ModuleDef,
        mut base_fq: pr::Path,
        mut steps: pr::Path,
    ) -> Result<FullyQualified> {
        let Some(first) = steps.pop_first() else {
            return Ok(FullyQualified {
                to_def: base_fq,
                within: pr::Path::empty(),
            });
        };

        let Some(def) = base_mod.defs.get(&first) else {
            return Err(Diagnostic::new_custom("name does not exist".to_string())
                .push_hint(format!("base_fq: {base_fq}"))
                .push_hint(format!("first: {first}"))
                .push_hint(format!("steps: {steps}")));
        };
        base_fq.push(first);

        match &def.kind {
            // recurse into submodules
            pr::DefKind::Module(sub_module) => self.lookup_in_module(sub_module, base_fq, steps),

            // resolved imports
            pr::DefKind::Import(import) => {
                // use resolved fq ident and extend it with remaining steps
                let mut new_path = import.target.clone();
                new_path.extend(steps);
                self.lookup_in_root(&[], &new_path)
            }

            // unresolved imports
            pr::DefKind::Unresolved(Some(def_kind)) if def_kind.is_import() => {
                let import = def_kind.as_import().unwrap();

                // resolve import target
                let import_fq = self
                    .lookup_in_root(base_fq.parent(), &import.target)
                    .with_span_fallback(def.span)?;

                tracing::debug!("resolved import to {import_fq:?}, steps={steps}");

                // combine import target with remaining steps and resolve again
                let mut new_path = import_fq.to_def;
                new_path.extend(import_fq.within);
                new_path.extend(steps);
                self.lookup_in_root(&[], &new_path)
            }

            // recursive lookup into self (we take node out of Unresolved during name resolution)
            pr::DefKind::Unresolved(None) => {
                if steps.is_empty() {
                    if self.allow_recursive {
                        Ok(FullyQualified {
                            to_def: base_fq,
                            within: pr::Path::empty(),
                        })
                    } else {
                        Err(Diagnostic::new_custom("recursive path"))
                    }
                } else {
                    // recursive, with remaining steps into self

                    // We do not support this, because we want to
                    // inline any lookups into types, to keep IR simpler.
                    Err(Diagnostic::new_custom(
                        "recursive paths into self are not allowed",
                    ))
                }
            }

            // resolve refs into types
            pr::DefKind::Unresolved(Some(def_kind)) => lookup_in_def(def_kind, steps.as_steps())
                .map(|_| FullyQualified {
                    to_def: base_fq,
                    within: steps,
                })
                .map_err(|msg| {
                    Diagnostic::new_custom(msg.unwrap_or_else(|| "unknown name".into()))
                }),

            // resolve refs into already resolved stuff
            def_kind => lookup_in_def(def_kind, steps.as_steps())
                .map(|_| FullyQualified {
                    to_def: base_fq,
                    within: steps,
                })
                .map_err(|msg| {
                    Diagnostic::new_custom(msg.unwrap_or_else(|| "unknown name".into()))
                }),
        }
    }
}

#[derive(Debug)]
struct FullyQualified {
    to_def: pr::Path,
    within: pr::Path,
}

fn lookup_in_def(def: &pr::DefKind, steps: &[String]) -> Result<(), Option<String>> {
    if steps.is_empty() {
        return Ok(());
    }

    match def {
        pr::DefKind::Ty(ty_def) => {
            // into type enum definition (referring to enum variants or tuple fields)
            lookup_in_ty(&ty_def.ty, steps).map_err(|_| None)?;
            Ok(())
        }

        pr::DefKind::Expr(_) => Err(Some("cannot lookup into expressions".to_string())),

        pr::DefKind::Module(_) | pr::DefKind::Import(_) | pr::DefKind::Unresolved(_) => {
            unreachable!()
        }
    }
}

pub fn lookup_in_ty<'t>(ty: &'t pr::Ty, steps: &[String]) -> Result<&'t pr::Ty, ()> {
    if steps.is_empty() {
        return Ok(ty);
    }
    match &ty.kind {
        pr::TyKind::Enum(variants) => {
            let variant = variants.iter().find(|v| v.name == steps[0]).ok_or(())?;
            lookup_in_ty(&variant.ty, &steps[1..])
        }
        pr::TyKind::Tuple(fields) => {
            let field = fields
                .iter()
                .find(|f| f.name.as_ref().is_some_and(|n| n == &steps[0]))
                .ok_or(())?;
            lookup_in_ty(&field.ty, &steps[1..])
        }
        _ => Err(()),
    }
}

/// Traverses a pattern and returns the set of all bound names.
/// For example `.Cat(name) | .Dog(name)` would return `[name]`.
fn collect_pattern_binds(pattern: &pr::Pattern) -> Result<Vec<String>> {
    match &pattern.kind {
        pr::PatternKind::Literal(_) => Ok(vec![]),
        pr::PatternKind::Bind(name) => Ok(vec![name.clone()]),

        pr::PatternKind::Enum(_, inner) => Ok(inner
            .as_ref()
            .map(|p| collect_pattern_binds(p))
            .transpose()?
            .unwrap_or_default()),
        pr::PatternKind::AnyOf(branches) => {
            assert!(branches.len() >= 2);

            let mut result = None;
            for b in branches {
                let binds = collect_pattern_binds(b)?;

                if let Some(result) = &result {
                    // in subsequent branches, validate that binds match previous binds
                    if &binds != result {
                        return Err(Diagnostic::new_custom(
                            "patterns introduce different variable names",
                        )
                        .with_span(Some(b.span)));
                    }
                } else {
                    // in the first branch, use collected binds
                    result = Some(binds);
                }
            }

            Ok(result.unwrap())
        }
    }
}
