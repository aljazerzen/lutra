use std::collections::HashSet;

use crate::Result;
use crate::diagnostic::{Diagnostic, DiagnosticCode, WithErrorInfo};
use crate::pr;
use crate::utils::IdGenerator;
use crate::utils::fold::PrFold;

/// Traverses module tree and runs name resolution on each of the definitions.
/// Collects references of each definition.
pub struct DefNameResolver<'a> {
    pub root: &'a mut pr::ModuleDef,
    /// Path of the current def being resolved
    pub current: pr::Path,
    /// Paths of definitions that still need name resolution
    pub unresolved: HashSet<pr::Path>,
    /// Generator for scope ids
    pub scope_id_gen: IdGenerator<usize>,

    // -- output --
    /// Accumulated references between types
    pub refs_tys: Vec<(pr::Path, Vec<pr::Path>)>,
    /// Accumulated references between expressions
    pub refs_vars: Vec<(pr::Path, Vec<pr::Path>)>,
    /// Accumulates mapping from span to target Ref
    pub target_spans: Vec<(crate::Span, crate::project::TargetSpan)>,
}

impl<'a> DefNameResolver<'a> {
    /// Entry point
    pub fn run(&mut self) -> Result<()> {
        self.current = pr::Path::empty();
        self.resolve_imports()?;
        assert_eq!(self.current.len(), 0);
        self.resolve_import_stars()?;
        assert_eq!(self.current.len(), 0);
        self.resolve_defs()?;
        assert_eq!(self.current.len(), 0);
        self.resolve_root_anno()?;
        Ok(())
    }

    fn init_expr_resolver<'b>(
        &'b mut self,
        allow_recursive: bool,
    ) -> super::expr::NameResolver<'b> {
        super::expr::NameResolver {
            // -- inherited --
            root: self.root,
            unresolved: &self.unresolved,
            scope_id_gen: &mut self.scope_id_gen,
            target_spans: &mut self.target_spans,
            // -- config --
            current: &self.current,
            allow_recursive,
            // -- state --
            scopes: Vec::new(),
            refs: Vec::new(),
        }
    }

    fn resolve_imports(&mut self) -> Result<()> {
        // collect module structure
        let module = self.root.get_module(self.current.as_steps()).unwrap();
        let mut submodules = Vec::new();
        let mut imports = Vec::new();
        for (name, def) in &module.defs {
            match &def.kind {
                pr::DefKind::Module(_) => {
                    submodules.push(name.clone());
                }
                pr::DefKind::Import(_) => {
                    self.current.push(name.clone());
                    let unresolved = self.unresolved.contains(&self.current);
                    let name = self.current.pop().unwrap();
                    if unresolved {
                        imports.push(name);
                    }
                }
                _ => {}
            }
        }
        let _ = module;

        // resolve each import
        let trace_span = tracing::span!(
            tracing::Level::DEBUG,
            "imports",
            module = self.current.to_string()
        );
        let _trace_enter = trace_span.enter();
        for name in imports {
            tracing::trace!("import {name}");
            self.current.push(name);

            // clone the import info
            let (import, span) = {
                let def = self.root.get(&self.current).unwrap();
                (def.kind.as_import().unwrap().clone(), def.span)
            };

            // resolve the def
            let r = self.init_expr_resolver(false);
            let target = import.kind.as_simple().unwrap();
            let target_fq = r.resolve_path(target).with_span_fallback(span)?;

            // put the def back in
            let def_loc = self.root.get_mut(&self.current).unwrap();
            def_loc.kind = pr::DefKind::Import(pr::ImportDef::new_simple(target_fq, import.span));

            // mark as resolved
            self.unresolved.remove(&self.current);
            self.current.pop();
        }
        drop(_trace_enter);

        // recurse into submodules
        for name in submodules {
            self.current.push(name);
            self.resolve_imports()?;
            self.current.pop();
        }
        Ok(())
    }

    fn resolve_import_stars(&mut self) -> Result<()> {
        let module = self.root.get_module_mut(self.current.as_steps()).unwrap();

        let mut submodules = Vec::new();
        for (name, def) in &module.defs {
            if let pr::DefKind::Module(_) = &def.kind {
                submodules.push(name.clone());
            }
        }

        let trace_span = tracing::span!(
            tracing::Level::DEBUG,
            "import stars",
            module = self.current.to_string()
        );
        let _trace_enter = trace_span.enter();

        let imports = std::mem::take(&mut module.imports);
        for def in imports {
            let pr::DefKind::Import(import) = def.kind else {
                unreachable!()
            };
            let pr::ImportKind::Star(target) = import.kind else {
                unreachable!()
            };
            tracing::trace!("{}", target);

            // resolve the def
            let r = self.init_expr_resolver(false);
            let target_fq = r
                .resolve_path(&target)
                .with_span_fallback(Some(import.span))?;

            // find target module and named of contents
            let target_mod = (self.root.get_module(target_fq.as_steps()))
                .ok_or_else(|| Diagnostic::new("expected module", DiagnosticCode::NAME_KIND))?;
            let names: Vec<_> = target_mod.defs.keys().cloned().collect();

            // construct simple imports for each name, but don't overwrite
            // explicit definitions that already exist in the module.
            let module = self.root.get_module_mut(self.current.as_steps()).unwrap();
            for name in names {
                if module.defs.contains_key(&name) {
                    continue;
                }
                let mut def_fq = target_fq.clone();
                def_fq.push(name.clone());
                let def = pr::Def::new(pr::ImportDef::new_simple(def_fq, import.span));
                module.defs.insert(name, def);
            }
        }
        drop(_trace_enter);

        // recurse into submodules
        for name in submodules {
            self.current.push(name);
            self.resolve_import_stars()?;
            self.current.pop();
        }
        Ok(())
    }

    fn resolve_defs(&mut self) -> Result<()> {
        // collect module structure
        let module = self.root.get_module(self.current.as_steps()).unwrap();
        let mut submodules = Vec::new();
        let mut defs = Vec::new();
        for (name, def) in &module.defs {
            match &def.kind {
                pr::DefKind::Module(_) => {
                    submodules.push(name.clone());
                }
                pr::DefKind::Import(_) => {}
                _ => {
                    self.current.push(name.clone());
                    let unresolved = self.unresolved.contains(&self.current);
                    let name = self.current.pop().unwrap();
                    if unresolved {
                        defs.push(name);
                    }
                }
            }
        }
        let _ = module;

        let trace_span = tracing::span!(
            tracing::Level::DEBUG,
            "names",
            module = self.current.to_string()
        );
        let _trace_enter = trace_span.enter();

        // resolve each def
        for name in defs {
            tracing::trace!("def {name}");
            self.current.push(name);

            // take the def kind out (replace with a placeholder)
            let mut def = {
                let def_slot = self.root.get_mut(&self.current).unwrap();
                std::mem::replace(def_slot, pr::Def::dummy())
            };

            // resolve the def
            let mut r = self.init_expr_resolver(true);
            def.kind = r.fold_def_kind(def.kind).with_span_fallback(def.span)?;

            let mut refs = r.refs;
            refs.retain(|p| p != r.current); // filter out self-references
            match &def.kind {
                pr::DefKind::Expr(_) | pr::DefKind::External(_) => {
                    self.refs_vars.push((self.current.clone(), refs));
                }
                pr::DefKind::Ty(_) | pr::DefKind::Anno(_) => {
                    self.refs_tys.push((self.current.clone(), refs));
                }
                pr::DefKind::Module(_) | pr::DefKind::Import(_) => {
                    unreachable!()
                }
            }

            // resolve annotations
            let mut r = self.init_expr_resolver(true);
            let annotations = std::mem::take(&mut def.annotations);
            for ann in annotations {
                let expr = Box::new(r.fold_expr(*ann.expr)?);
                def.annotations.push(pr::Anno { expr });
            }

            // put the resolved def back in
            let def_slot = self.root.get_mut(&self.current).unwrap();
            *def_slot = def;

            // mark as resolved
            self.unresolved.remove(&self.current);

            self.current.pop();
        }
        drop(_trace_enter);

        // recurse into submodules
        for name in submodules {
            self.current.push(name);
            self.resolve_defs()?;
            self.current.pop();
        }
        Ok(())
    }

    fn resolve_root_anno(&mut self) -> Result<()> {
        let annotations = std::mem::take(&mut self.root.annotations);

        let mut r = self.init_expr_resolver(false);
        let mut resolved = Vec::with_capacity(annotations.len());
        for a in annotations {
            let expr = Box::new(r.fold_expr(*a.expr)?);
            resolved.push(pr::Anno { expr });
        }
        self.root.annotations = resolved;
        Ok(())
    }
}
