use itertools::Itertools;

use crate::error::{Diagnostic, WithErrorInfo};
use crate::ir::decl;
use crate::ir::fold::{self, PrFold};
use crate::semantic::NS_STD;
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
        return Err(Diagnostic::new_simple(
            "unimplemented cyclic references between expressions",
        ));
    }

    Ok(order_tys
        .iter()
        .chain(order_vars.iter())
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
        let (fq_ident, indirections) = self.resolve_ident(import_def.name)?;
        if !indirections.is_empty() {
            return Err(Diagnostic::new_simple(
                "Import can only reference modules and declarations",
            ));
        }
        if fq_ident.is_empty() {
            log::debug!("resolved type ident to : {fq_ident:?} + {indirections:?}");
            return Err(Diagnostic::new_simple("invalid type name"));
        }
        Ok(pr::ImportDef {
            name: pr::Path::new(fq_ident),
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
                let (ident, indirections) = self.resolve_ident(ident).with_span(expr.span)?;
                // TODO: can this ident have length 0?

                let mut kind = pr::ExprKind::Ident(pr::Path::new(ident));
                for indirection in indirections {
                    let mut e = pr::Expr::new(kind);
                    e.span = expr.span;
                    kind = pr::ExprKind::Indirection {
                        base: Box::new(e),
                        field: pr::IndirectionKind::Name(indirection),
                    };
                }

                pr::Expr { kind, ..expr }
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
                let (ident, indirections) = self.resolve_ident(ident).with_span(ty.span)?;

                if !indirections.is_empty() {
                    log::debug!("resolved type ident to : {ident:?} + {indirections:?}");
                    return Err(Diagnostic::new_simple("types are not allowed indirections")
                        .with_span(ty.span));
                }

                if ident.is_empty() {
                    log::debug!("resolved type ident to : {ident:?} + {indirections:?}");
                    return Err(Diagnostic::new_simple("invalid type name").with_span(ty.span));
                }

                pr::Ty {
                    kind: pr::TyKind::Ident(pr::Path::new(ident)),
                    ..ty
                }
            }
            _ => fold::fold_type(self, ty)?,
        })
    }
}

impl NameResolver<'_> {
    /// Returns resolved fully-qualified ident and a list of indirections
    fn resolve_ident(&mut self, mut ident: pr::Path) -> Result<(Vec<String>, Vec<String>)> {
        // this is the name we are looking for
        let first = ident.iter().next().unwrap();
        let mod_path = match first.as_str() {
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

            NS_STD => {
                ident.pop_front();
                vec![NS_STD.to_string()]
            }

            _ => self.decl_module_path.to_vec(),
        };
        let mod_decl = self.root.module.get_submodule_mut(&mod_path);

        Ok(if let Some(module) = mod_decl {
            // module found

            // now find the decl within that module
            let (path, indirections) = module_lookup(module, ident)?;

            // prepend the ident with the module path
            // this will make this ident a fully-qualified ident
            let mut fq_ident = mod_path;
            fq_ident.extend(path);

            self.refs.push(pr::Path::new(fq_ident.clone()));

            (fq_ident, indirections)
        } else {
            // cannot find module, so this must be a ref to a local var + indirections
            let mut steps = ident.into_iter();
            let first = steps.next().unwrap();
            let indirections = steps.collect_vec();
            (vec![first], indirections)
        })
    }
}

fn module_lookup(
    module: &mut decl::Module,
    ident_within: pr::Path,
) -> Result<(Vec<String>, Vec<String>)> {
    let mut steps = ident_within.into_iter().collect_vec();

    let mut module = module;
    for i in 0..steps.len() {
        let decl = module_lookup_step(module, &steps[i])?;
        if let decl::DeclKind::Module(inner) = &mut decl.kind {
            module = inner;
            continue;
        } else {
            // we've found a declaration that is not a module:
            // this and preceding steps are identifier, steps following are indirections
            let indirections = steps.drain((i + 1)..).collect_vec();
            return Ok((steps, indirections));
        }
    }

    Err(Diagnostic::new_simple(
        "direct references modules not allowed",
    ))
}

fn module_lookup_step<'m>(module: &'m mut decl::Module, step: &str) -> Result<&'m mut decl::Decl> {
    if module.names.contains_key(step) {
        return Ok(module.names.get_mut(step).unwrap());
    }

    Err(Diagnostic::new_simple(format!("Name not found: {step}")))
}
