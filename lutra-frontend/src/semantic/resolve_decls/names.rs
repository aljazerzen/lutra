use indexmap::IndexMap;
use itertools::Itertools;

use crate::decl;
use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::semantic::NS_STD;
use crate::utils::fold::{self, PrFold};
use crate::Result;
use crate::{pr, utils};

/// Runs name resolution for global names - names that refer to declarations.
///
/// Keeps track of all inter-declaration references.
/// Returns a resolution order.
pub fn resolve_decl_refs(root: &mut decl::RootModule) -> Result<Vec<Vec<pr::Path>>> {
    // resolve inter-declaration references
    let (refs_tys, refs_vars) = {
        let mut r = ModuleRefResolver {
            root,
            refs_tys: Default::default(),
            refs_vars: Default::default(),
            current_path: Vec::new(),
        };
        r.resolve_refs()?;
        (r.refs_tys, r.refs_vars)
    };

    // toposort tys
    let order_tys = utils::toposort::<pr::Path>(&refs_tys);

    // toposort vars
    let order_vars = utils::toposort::<pr::Path>(&refs_vars);
    let has_var_cycles = order_vars.iter().any(|scc| scc.len() != 1);

    if has_var_cycles {
        return Err(Diagnostic::new_custom(
            "unimplemented cyclic references between expressions",
        ));
    }

    Ok(itertools::chain(order_tys, order_vars)
        .map(|tree| tree.iter().map(|p| (*p).clone()).collect_vec())
        .collect_vec())
}

/// Traverses module tree and runs name resolution on each of the declarations.
/// Collects references of each declaration.
struct ModuleRefResolver<'a> {
    root: &'a mut decl::RootModule,
    current_path: Vec<String>,

    refs_tys: Vec<(pr::Path, Vec<pr::Path>)>,
    refs_vars: Vec<(pr::Path, Vec<pr::Path>)>,
}

impl ModuleRefResolver<'_> {
    fn resolve_refs(&mut self) -> Result<()> {
        let path = &mut self.current_path;
        let module = self.root.module.get_submodule_mut(path).unwrap();

        let mut submodules = Vec::new();
        let mut unresolved_decls = Vec::new();
        for (name, decl) in &module.names {
            match &decl.kind {
                decl::DeclKind::Module(_) => {
                    submodules.push(name.clone());
                }
                decl::DeclKind::Unresolved(Some(_)) => {
                    unresolved_decls.push(name.clone());
                }
                decl::DeclKind::Unresolved(None) => panic!(),
                _ => {}
            }
        }

        for name in unresolved_decls {
            path.push(name);
            let fq_ident = pr::Path::new(path.clone());

            // take the decl out of the module tree
            let (stmt, span) = self.root.module.take_unresolved(&fq_ident);

            // resolve the decl
            let mut r = NameResolver {
                root: self.root,
                decl_module_path: &path[0..(path.len() - 1)],
                scopes: Vec::new(),
                refs: Vec::new(),
            };

            let stmt = r.fold_stmt_kind(stmt).with_span_fallback(span)?;

            // filter out self-references
            r.refs.retain(|r| r != &fq_ident);

            match &stmt {
                pr::StmtKind::VarDef(_) => {
                    self.refs_vars.push((fq_ident.clone(), r.refs));
                }
                pr::StmtKind::TypeDef(_) => {
                    self.refs_tys.push((fq_ident.clone(), r.refs));
                }
                pr::StmtKind::ModuleDef(_) | pr::StmtKind::ImportDef(_) => {}
            }

            path.pop();

            // put the decl back in
            {
                let decl = self.root.module.get_mut(&fq_ident).unwrap();
                *decl.kind.as_unresolved_mut().unwrap() = Some(stmt);
            };
        }

        for name in submodules {
            self.current_path.push(name);
            self.resolve_refs()?;
            self.current_path.pop();
        }
        Ok(())
    }
}

/// Traverses AST and resolves all global (non-local) identifiers.
struct NameResolver<'a> {
    root: &'a mut decl::RootModule,
    decl_module_path: &'a [String],
    scopes: Vec<Scope>,
    refs: Vec<pr::Path>,
}

impl NameResolver<'_> {
    fn fold_stmt_kind(&mut self, stmt: pr::StmtKind) -> Result<pr::StmtKind> {
        Ok(match stmt {
            pr::StmtKind::VarDef(var_def) => pr::StmtKind::VarDef(self.fold_var_def(var_def)?),
            pr::StmtKind::TypeDef(ty_def) => pr::StmtKind::TypeDef(self.fold_type_def(ty_def)?),
            pr::StmtKind::ImportDef(import_def) => {
                pr::StmtKind::ImportDef(self.fold_import_def(import_def)?)
            }
            pr::StmtKind::ModuleDef(_) => unreachable!(),
        })
    }

    fn fold_import_def(&mut self, import_def: pr::ImportDef) -> Result<pr::ImportDef, Diagnostic> {
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
            pr::TyKind::Function(Some(ty_func)) => {
                if self.scopes.is_empty() {
                    let scope = Scope::new_of_ty_func(&ty_func)?;
                    self.scopes.push(scope);
                    let r = fold::fold_ty_func(self, ty_func);
                    self.scopes.pop();

                    pr::Ty {
                        kind: pr::TyKind::Function(Some(r?)),
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
                        kind: pr::TyKind::Function(Some(ty_func)),
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
    fn resolve_ident(&mut self, mut ident: pr::Path) -> Result<pr::Path> {
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

        // find module
        let mod_fq_path = match ident.first() {
            "project" => {
                ident.pop_front();
                vec![]
            }
            "module" => {
                ident.pop_front();
                self.decl_module_path.to_vec()
            }
            "super" => {
                ident.pop_front();

                let mut path = self.decl_module_path.to_vec();
                path.pop();
                path
            }

            "int" | "float" | "text" | "bool" => vec![],

            NS_STD => {
                ident.pop_front();
                vec![NS_STD.to_string()]
            }

            _ => self.decl_module_path.to_vec(),
        };
        let mod_decl = self.root.module.get_submodule_mut(&mod_fq_path);

        let decl = mod_decl.and_then(|module| module.get(&ident));
        if decl.is_none() {
            log::debug!("scopes: {:?}", self.scopes);
            return Err(Diagnostic::new_custom(format!("unknown name {ident}")));
        }

        // prepend the ident with the module path
        // this will make this ident a fully-qualified ident
        let mut fq_ident = mod_fq_path;
        fq_ident.extend(ident);
        let fq_ident = pr::Path::new(fq_ident);

        self.refs.push(fq_ident.clone());

        Ok(fq_ident)
    }
}

#[derive(Debug)]
pub struct Scope {
    names: IndexMap<String, ScopedKind>,
}

#[derive(Debug, Clone, enum_as_inner::EnumAsInner)]
pub enum ScopedKind {
    Param,
    Generic,
}

impl Scope {
    pub fn new_of_func(func: &pr::Func) -> crate::Result<Self> {
        let mut scope = Self {
            names: IndexMap::new(),
        };
        scope.insert_params(func)?;
        scope.insert_generics(&func.ty_params)?;
        Ok(scope)
    }

    pub fn new_of_ty_func(func: &pr::TyFunc) -> crate::Result<Self> {
        let mut scope = Self {
            names: IndexMap::new(),
        };
        scope.insert_generics(&func.ty_params)?;
        Ok(scope)
    }

    pub fn insert_generics(&mut self, type_params: &[pr::TyParam]) -> crate::Result<()> {
        for param in type_params {
            let scoped = ScopedKind::Generic;
            self.names.insert(param.name.clone(), scoped);
        }
        Ok(())
    }

    pub fn insert_params(&mut self, func: &pr::Func) -> crate::Result<()> {
        for param in func.params.iter() {
            self.names.insert(param.name.clone(), ScopedKind::Param);
        }
        Ok(())
    }

    pub fn get(&self, name: &str) -> Option<(usize, &ScopedKind)> {
        let (position, _, scoped) = self.names.get_full(name)?;
        Some((position, scoped))
    }
}
