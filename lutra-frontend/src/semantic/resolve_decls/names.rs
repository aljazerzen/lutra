use itertools::Itertools;

use crate::ir::decl;
use crate::ir::fold::{self, PrFold};
use crate::semantic::NS_STD;
use crate::{pr, utils};
use crate::{Error, Result, WithErrorInfo};

/// Runs name resolution for global names - names that refer to declarations.
///
/// Keeps track of all inter-declaration references.
/// Returns a resolution order.
pub fn resolve_decl_refs(root: &mut decl::RootModule) -> Result<Vec<pr::Path>> {
    // resolve inter-declaration references
    let refs = {
        let mut r = ModuleRefResolver {
            root,
            refs: Default::default(),
            current_path: Vec::new(),
        };
        r.resolve_refs()?;
        r.refs
    };

    // HACK: put std.* declarations first
    // this is needed because during compilation of transforms, we inject refs to "std.lte" and a few others
    // sorting here makes std decls appear first in the final ordering
    let mut refs = refs;
    refs.sort_by_key(|(a, _)| !a.path().first().map_or(false, |p| p == "std"));

    // toposort the declarations
    // TODO: we might not need to compile all declarations if they are not used
    //   to prevent that, this start should be something else than None
    //   a list of all public declarations?
    // let main = pr::Ident::from_name("main");
    let order = utils::toposort::<pr::Path>(&refs, None);

    if let Some(order) = order {
        Ok(order.into_iter().cloned().collect_vec())
    } else {
        todo!("error for a cyclic references between expressions")
    }
}

/// Traverses module tree and runs name resolution on each of the declarations.
/// Collects references of each declaration.
struct ModuleRefResolver<'a> {
    root: &'a mut decl::RootModule,
    current_path: Vec<String>,

    // TODO: maybe make these ids, instead of Ident?
    refs: Vec<(pr::Path, Vec<pr::Path>)>,
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
                decl::DeclKind::Unresolved(_) => {
                    unresolved_decls.push(name.clone());
                }
                _ => {}
            }
        }

        for name in unresolved_decls {
            // take the decl out of the module tree
            let (stmt, declared_at) = {
                let submodule = self.root.module.get_submodule_mut(path).unwrap();
                let decl = submodule.names.get_mut(&name).unwrap();
                let unresolved = decl.kind.as_unresolved_mut().unwrap();

                (unresolved.take().unwrap(), decl.declared_at)
            };
            let span = declared_at
                .and_then(|x| self.root.span_map.get(&x))
                .cloned();

            // resolve the decl
            path.push(name);
            let mut r = NameResolver {
                root: self.root,
                decl_module_path: &path[0..(path.len() - 1)],
                refs: Vec::new(),
            };

            let stmt = r.fold_stmt_kind(stmt).with_span_fallback(span)?;

            // filter out self-references
            r.refs.retain(|r| r.full_path() != path);

            let decl_ident = pr::Path::from_path(path.clone());
            self.refs.push((decl_ident, r.refs));

            let name = path.pop().unwrap();

            // put the decl back in
            {
                let submodule = self.root.module.get_submodule_mut(path).unwrap();
                let decl = submodule.names.get_mut(&name).unwrap();
                let unresolved = decl.kind.as_unresolved_mut().unwrap();
                *unresolved = Some(stmt);
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

    fn fold_import_def(&mut self, import_def: pr::ImportDef) -> Result<pr::ImportDef, Error> {
        let (fq_ident, indirections) = self.resolve_ident(import_def.name)?;
        if !indirections.is_empty() {
            return Err(Error::new_simple(
                "Import can only reference modules and declarations",
            ));
        }
        if fq_ident.is_empty() {
            log::debug!("resolved type ident to : {fq_ident:?} + {indirections:?}");
            return Err(Error::new_simple("invalid type name"));
        }
        Ok(pr::ImportDef {
            name: pr::Path::from_path(fq_ident),
            alias: import_def.alias,
        })
    }
}

impl fold::PrFold for NameResolver<'_> {
    fn fold_expr(&mut self, expr: pr::Expr) -> Result<pr::Expr> {
        Ok(match expr.kind {
            pr::ExprKind::Ident(ident) => {
                let (ident, indirections) = self.resolve_ident(ident).with_span(expr.span)?;
                // TODO: can this ident have length 0?

                let mut kind = pr::ExprKind::Ident(pr::Path::from_path(ident));
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
                    return Err(
                        Error::new_simple("types are not allowed indirections").with_span(ty.span)
                    );
                }

                if ident.is_empty() {
                    log::debug!("resolved type ident to : {ident:?} + {indirections:?}");
                    return Err(Error::new_simple("invalid type name").with_span(ty.span));
                }

                pr::Ty {
                    kind: pr::TyKind::Ident(pr::Path::from_path(ident)),
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

            self.refs.push(pr::Path::from_path(fq_ident.clone()));

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

    Err(Error::new_simple("direct references modules not allowed"))
}

fn module_lookup_step<'m>(module: &'m mut decl::Module, step: &str) -> Result<&'m mut decl::Decl> {
    if module.names.contains_key(step) {
        return Ok(module.names.get_mut(step).unwrap());
    }

    Err(Error::new_simple(format!("Name not found: {step}")))
}
