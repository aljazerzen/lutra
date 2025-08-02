use crate::Result;
use crate::diagnostic::WithErrorInfo;
use crate::pr;
use crate::utils::IdGenerator;

/// Traverses module tree and runs name resolution on each of the definitions.
/// Collects references of each definition.
pub struct ModuleRefResolver<'a> {
    pub root: &'a mut pr::ModuleDef,
    pub current_path: Vec<String>,

    pub refs_tys: Vec<(pr::Path, Vec<pr::Path>)>,
    pub refs_vars: Vec<(pr::Path, Vec<pr::Path>)>,

    pub scope_id_gen: IdGenerator<usize>,
}

impl ModuleRefResolver<'_> {
    pub fn resolve_refs(&mut self) -> Result<()> {
        let path = &mut self.current_path;
        let module = self.root.get_submodule_mut(path).unwrap();

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
            path.push(name);
            let fq_ident = pr::Path::new(path.clone());

            // take the def out of the module tree
            let (def, span) = self.root.take_unresolved(&fq_ident);

            // resolve the def
            let mut r = super::expr::NameResolver {
                root: self.root,
                def_module_path: &path[0..(path.len() - 1)],
                scopes: Vec::new(),
                refs: Vec::new(),
                scope_id_gen: &mut self.scope_id_gen,
            };

            let def = r.fold_def_kind(def).with_span_fallback(span)?;

            // filter out self-references
            r.refs.retain(|r| r != &fq_ident);

            match &def {
                pr::DefKind::Expr(_) => {
                    self.refs_vars.push((fq_ident.clone(), r.refs));
                }
                pr::DefKind::Ty(_) => {
                    self.refs_tys.push((fq_ident.clone(), r.refs));
                }
                pr::DefKind::Module(_) | pr::DefKind::Import(_) | pr::DefKind::Unresolved(_) => {}
            }

            path.pop();

            // put the def back in
            self.root.insert_unresolved(&fq_ident, def);
        }

        for name in submodules {
            self.current_path.push(name);
            self.resolve_refs()?;
            self.current_path.pop();
        }
        Ok(())
    }
}
