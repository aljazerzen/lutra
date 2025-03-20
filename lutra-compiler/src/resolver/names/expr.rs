use crate::decl;
use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::pr;
use crate::resolver::NS_STD;
use crate::utils::fold::{self, PrFold};
use crate::Result;

use super::{Scope, ScopedKind};

/// Traverses AST and resolves identifiers.
pub struct NameResolver<'a> {
    pub root: &'a mut decl::RootModule,
    pub decl_module_path: &'a [String],
    pub scopes: Vec<Scope>,
    pub refs: Vec<pr::Path>,
}

impl NameResolver<'_> {
    pub fn fold_stmt_kind(&mut self, stmt: pr::StmtKind) -> Result<pr::StmtKind> {
        Ok(match stmt {
            pr::StmtKind::VarDef(var_def) => pr::StmtKind::VarDef(self.fold_var_def(var_def)?),
            pr::StmtKind::TypeDef(ty_def) => pr::StmtKind::TypeDef(self.fold_type_def(ty_def)?),
            pr::StmtKind::ImportDef(import_def) => {
                pr::StmtKind::ImportDef(self.fold_import_def(import_def)?)
            }
            pr::StmtKind::ModuleDef(_) => unreachable!(),
        })
    }

    pub fn fold_import_def(
        &mut self,
        import_def: pr::ImportDef,
    ) -> Result<pr::ImportDef, Diagnostic> {
        let fq_ident = self.resolve_ident(import_def.name)?;
        Ok(pr::ImportDef {
            name: fq_ident,
            alias: import_def.alias,
        })
    }
}

impl fold::PrFold for NameResolver<'_> {
    fn fold_stmt(&mut self, _stmt: pr::Stmt) -> Result<pr::Stmt> {
        unreachable!()
    }
    fn fold_stmts(&mut self, _stmts: Vec<pr::Stmt>) -> Result<Vec<pr::Stmt>> {
        unreachable!()
    }

    fn fold_expr(&mut self, expr: pr::Expr) -> Result<pr::Expr> {
        Ok(match expr.kind {
            pr::ExprKind::Ident(ident) => {
                // TODO: can this ident have length 0?

                let ident = self.resolve_ident(ident).with_span(expr.span)?;

                let kind = pr::ExprKind::Ident(ident);
                pr::Expr { kind, ..expr }
            }
            pr::ExprKind::Indirection { .. } => {
                // special case: indirection might be compiled to a call to std::index,
                // so we add a ref here. This could be conditional.
                self.refs.push(pr::Path::new(vec!["std", "index"]));

                pr::Expr {
                    kind: fold::fold_expr_kind(self, expr.kind)?,
                    ..expr
                }
            }
            _ => pr::Expr {
                kind: fold::fold_expr_kind(self, expr.kind)?,
                ..expr
            },
        })
    }

    fn fold_type(&mut self, ty: pr::Ty) -> Result<pr::Ty> {
        Ok(match ty.kind {
            pr::TyKind::Ident(ident) => {
                let ident = self.resolve_ident(ident).with_span(ty.span)?;

                pr::Ty {
                    kind: pr::TyKind::Ident(ident),
                    ..ty
                }
            }
            pr::TyKind::Function(ty_func) => {
                if self.scopes.is_empty() {
                    let scope = Scope::new_of_ty_func(&ty_func)?;
                    self.scopes.push(scope);
                    let r = fold::fold_ty_func(self, ty_func);
                    self.scopes.pop();

                    pr::Ty {
                        kind: pr::TyKind::Function(r?),
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
                        kind: pr::TyKind::Function(ty_func),
                        ..ty
                    };
                    fold::fold_type(self, ty)?
                }
            }
            _ => fold::fold_type(self, ty)?,
        })
    }

    fn fold_func(&mut self, func: pr::Func) -> Result<pr::Func> {
        let scope = Scope::new_of_func(&func)?;
        self.scopes.push(scope);
        let r = fold::fold_func(self, func);
        self.scopes.pop();
        r
    }
}

impl NameResolver<'_> {
    /// Returns resolved fully-qualified ident
    fn resolve_ident(&mut self, ident: pr::Path) -> Result<pr::Path> {
        for (up, scope) in self.scopes.iter().rev().enumerate() {
            if let Some((position, scoped)) = scope.get(ident.first()) {
                // match: this ident is a param ref

                if ident.len() != 1 {
                    return Err(Diagnostic::new_custom(format!(
                        "{} is a param, not a module",
                        ident.first()
                    )));
                }

                let mut fq_ident = pr::Path::new(vec!["scope"]);
                for _ in 0..up {
                    fq_ident.push("up".to_string())
                }
                fq_ident.push(match &scoped {
                    ScopedKind::Param => position.to_string(),
                    ScopedKind::Generic => ident.first().to_string(),
                });
                return Ok(fq_ident);
            }
        }

        // find lookup base
        let steps = ident.full_path();
        let (base_path, steps) = match ident.first() {
            "project" => (vec![], &steps[1..]),
            "module" => (self.decl_module_path.to_vec(), &steps[1..]),
            "super" => {
                let mut path = self.decl_module_path.to_vec();
                path.pop();
                (path, &steps[1..])
            }
            NS_STD => (vec![NS_STD.to_string()], &steps[1..]),
            _ => (self.decl_module_path.to_vec(), steps),
        };
        let base_decl = self.root.module.get_submodule(&base_path);

        let found = base_decl.map_or(false, |module| module_lookup_steps(module, steps));
        if !found {
            tracing::debug!("scopes: {:?}", self.scopes);
            return Err(Diagnostic::new_custom(format!("unknown name {ident}")));
        }

        // prepend the ident with the module path
        // this will make this ident a fully-qualified ident
        let mut fq_ident = pr::Path::new(base_path);
        for step in steps {
            fq_ident.push(step.clone());
        }

        self.refs.push(fq_ident.clone());

        Ok(fq_ident)
    }
}

/// Returns true if ok
fn module_lookup_steps(module: &decl::Module, steps: &[String]) -> bool {
    if steps.is_empty() {
        // references to modules do not mean anything
        // TODO: a better err message
        return false;
    }

    let Some(decl) = module.names.get(&steps[0]) else {
        // name does not exist
        return false;
    };

    match &decl.kind {
        // recurse into submodules
        decl::DeclKind::Module(sub_module) => module_lookup_steps(sub_module, &steps[1..]),

        // resolve refs into types
        decl::DeclKind::Unresolved(Some(stmt)) => stmt_lookup_steps(stmt, &steps[1..]),
        decl::DeclKind::Unresolved(None) => {
            // recursive lookup into self (we take node out of Unresolved during name resolution)

            if steps.len() == 1 {
                return true;
            } else {
                // ???
                todo!()
            }
        }
        _ => unreachable!(),
    }
}

fn stmt_lookup_steps(stmt: &pr::StmtKind, steps: &[String]) -> bool {
    if steps.is_empty() {
        return true;
    }

    // a single step is allowed
    if steps.len() > 1 {
        return false;
    }

    // into type enum declarations (referring to variants)
    match stmt {
        pr::StmtKind::VarDef(_) => false,
        pr::StmtKind::TypeDef(ty_def) => match &ty_def.ty.kind {
            pr::TyKind::Enum(variants) => variants.iter().any(|v| v.name == steps[0]),
            _ => false,
        },
        pr::StmtKind::ModuleDef(_) => unreachable!(),
        pr::StmtKind::ImportDef(_) => false,
    }
}
