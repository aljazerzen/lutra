use crate::decl;
use crate::diagnostic::WithErrorInfo;
use crate::pr;
use crate::Result;

/// Traverses module tree and runs name resolution on each of the declarations.
/// Collects references of each declaration.
pub struct ModuleRefResolver<'a> {
    pub root: &'a mut decl::RootModule,
    pub current_path: Vec<String>,

    pub refs_tys: Vec<(pr::Path, Vec<pr::Path>)>,
    pub refs_vars: Vec<(pr::Path, Vec<pr::Path>)>,
}

impl ModuleRefResolver<'_> {
    pub fn resolve_refs(&mut self) -> Result<()> {
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
            let mut r = super::expr::NameResolver {
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
