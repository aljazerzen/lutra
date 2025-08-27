use crate::Result;
use crate::diagnostic::WithErrorInfo;
use crate::pr;
use crate::utils::IdGenerator;

/// Traverses module tree and runs name resolution on each of the definitions.
/// Collects references of each definition.
pub struct ModuleRefResolver<'a> {
    pub root: &'a mut pr::ModuleDef,
    pub current_path: pr::Path,

    pub refs_tys: Vec<(pr::Path, Vec<pr::Path>)>,
    pub refs_vars: Vec<(pr::Path, Vec<pr::Path>)>,

    pub scope_id_gen: IdGenerator<usize>,
}

impl ModuleRefResolver<'_> {
    /// Entry point
    pub fn run(&mut self) -> Result<()> {
        self.current_path = pr::Path::empty();
        self.resolve_imports()?;
        assert_eq!(self.current_path.len(), 0);
        self.resolve_refs()?;
        assert_eq!(self.current_path.len(), 0);
        Ok(())
    }

    #[tracing::instrument("i", skip_all, fields(p = self.current_path.to_string()))]
    fn resolve_imports(&mut self) -> Result<()> {
        let path = &mut self.current_path;
        let module = self.root.get_module_mut(path.as_steps()).unwrap();

        let mut submodules = Vec::new();
        let mut imports = Vec::new();
        for (name, def) in &module.defs {
            match &def.kind {
                pr::DefKind::Module(_) => {
                    submodules.push(name.clone());
                }
                pr::DefKind::Unresolved(Some(d)) if d.is_import() => {
                    imports.push(name.clone());
                }
                pr::DefKind::Unresolved(None) => panic!(),
                _ => {}
            }
        }

        for name in imports {
            tracing::trace!("import {name}");
            path.push(name);

            // take the def out of the module tree
            let (def, span) = self.root.take_unresolved(path);

            // resolve the def
            let mut r = super::expr::NameResolver {
                root: self.root,
                def_module_path: path.parent(),
                scopes: Vec::new(),
                refs: Vec::new(),
                scope_id_gen: &mut self.scope_id_gen,
                allow_recursive: false,
            };

            let pr::DefKind::Import(import) = def else {
                unreachable!()
            };
            let import = r.fold_import_def(import).with_span_fallback(span)?;

            // put the def back in
            let def_loc = self.root.get_mut(path).unwrap();
            def_loc.kind = pr::DefKind::Import(import);

            path.pop();
        }

        for name in submodules {
            self.current_path.push(name);
            self.resolve_imports()?;
            self.current_path.pop();
        }
        Ok(())
    }

    #[tracing::instrument("r", skip_all, fields(p = self.current_path.to_string()))]
    fn resolve_refs(&mut self) -> Result<()> {
        let path = &mut self.current_path;
        let module = self.root.get_module_mut(path.as_steps()).unwrap();

        let mut submodules = Vec::new();
        let mut unresolved_defs = Vec::new();
        for (name, def) in &module.defs {
            match &def.kind {
                pr::DefKind::Module(_) => {
                    submodules.push(name.clone());
                }
                pr::DefKind::Unresolved(Some(_)) => {
                    unresolved_defs.push(name.clone());
                }
                pr::DefKind::Unresolved(None) => panic!(),
                _ => {}
            }
        }

        for name in unresolved_defs {
            tracing::trace!("def {name}");
            path.push(name);

            // take the def out of the module tree
            let (def, span) = self.root.take_unresolved(path);

            // resolve the def
            let mut r = super::expr::NameResolver {
                root: self.root,
                def_module_path: path.parent(),
                scopes: Vec::new(),
                refs: Vec::new(),
                scope_id_gen: &mut self.scope_id_gen,
                allow_recursive: true,
            };

            let def = r.fold_def_kind(def).with_span_fallback(span)?;

            // filter out self-references
            r.refs.retain(|r| r != path);

            match &def {
                pr::DefKind::Expr(_) => {
                    self.refs_vars.push((path.clone(), r.refs));
                }
                pr::DefKind::Ty(_) => {
                    self.refs_tys.push((path.clone(), r.refs));
                }
                pr::DefKind::Module(_) | pr::DefKind::Import(_) | pr::DefKind::Unresolved(_) => {
                    unreachable!()
                }
            }

            // put the def back in
            self.root.insert_unresolved(path, def);

            path.pop();
        }

        for name in submodules {
            self.current_path.push(name);
            self.resolve_refs()?;
            self.current_path.pop();
        }
        Ok(())
    }
}
