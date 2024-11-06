use indexmap::IndexMap;

use crate::ir::decl::{Decl, DeclKind, Module, RootModule};
use crate::pr;
use crate::{Result, Span};

use super::{NS_MAIN, NS_STD};

impl Module {
    pub(crate) fn new_root() -> Module {
        // Each module starts with a default namespace that contains a wildcard
        // and the standard library.
        Module {
            names: IndexMap::from([(NS_STD.to_string(), Decl::new(Module::default()))]),
        }
    }

    pub fn insert(&mut self, fq_ident: pr::Path, decl: Decl) -> Option<Decl> {
        let module = self.get_submodule_mut(fq_ident.path())?;
        module.names.insert(fq_ident.name().to_string(), decl)
    }

    pub fn get_mut(&mut self, ident: &pr::Path) -> Option<&mut Decl> {
        let module = self.get_submodule_mut(ident.path())?;
        module.names.get_mut(ident.name())
    }

    /// Get namespace entry using a fully qualified ident.
    pub fn get(&self, ident: &pr::Path) -> Option<&Decl> {
        let module = self.get_submodule(ident.path())?;
        module.names.get(ident.name())
    }

    pub fn get_submodule(&self, path: &[String]) -> Option<&Module> {
        let mut curr_mod = self;
        for step in path {
            let decl = curr_mod.names.get(step)?;
            curr_mod = decl.kind.as_module()?;
        }
        Some(curr_mod)
    }

    pub fn get_submodule_mut(&mut self, path: &[String]) -> Option<&mut Module> {
        let mut curr_mod = self;
        for step in path {
            let decl = curr_mod.names.get_mut(step)?;
            curr_mod = decl.kind.as_module_mut()?;
        }
        Some(curr_mod)
    }

    pub fn get_module_path(&self, path: &[String]) -> Option<Vec<&Module>> {
        let mut res = vec![self];
        for step in path {
            let decl = res.last().unwrap().names.get(step)?;
            let module = decl.kind.as_module()?;
            res.push(module);
        }

        Some(res)
    }

    pub fn get_decls(&self) -> Vec<(&String, &Decl)> {
        self.names.iter().collect()
    }

    pub fn as_flat_decls(&self) -> Vec<(pr::Path, &Decl)> {
        let mut r = Vec::new();
        for (name, decl) in &self.names {
            match &decl.kind {
                DeclKind::Module(module) => r.extend(
                    module
                        .as_flat_decls()
                        .into_iter()
                        .map(|(inner, decl)| (pr::Path::from_name(name) + inner, decl)),
                ),
                _ => r.push((pr::Path::from_name(name), decl)),
            }
        }
        r
    }

    /// Recursively finds all declarations that end in suffix.
    pub fn find_by_suffix(&self, suffix: &str) -> Vec<pr::Path> {
        let mut res = Vec::new();

        for (name, decl) in &self.names {
            if let DeclKind::Module(module) = &decl.kind {
                let nested = module.find_by_suffix(suffix);
                res.extend(nested.into_iter().map(|x| x.prepend(vec![name.clone()])));
                continue;
            }

            if name == suffix {
                res.push(pr::Path::from_name(name));
            }
        }

        res
    }

    /// Recursively finds all declarations with an annotation that has a specific name.
    pub fn find_by_annotation_name(&self, annotation_name: &pr::Path) -> Vec<pr::Path> {
        let mut res = Vec::new();

        for (name, decl) in &self.names {
            if let DeclKind::Module(module) = &decl.kind {
                let nested = module.find_by_annotation_name(annotation_name);
                res.extend(nested.into_iter().map(|x| x.prepend(vec![name.clone()])));
            }

            let has_annotation = decl_has_annotation(decl, annotation_name);
            if has_annotation {
                res.push(pr::Path::from_name(name));
            }
        }
        res
    }

    pub fn take_unresolved(&mut self, ident: &pr::Path) -> (pr::StmtKind, Option<Span>) {
        let decl = self.get_mut(ident).unwrap();
        let unresolved = decl.kind.as_unresolved_mut().unwrap();
        (unresolved.take().unwrap(), decl.span)
    }
}

fn decl_has_annotation(decl: &Decl, annotation_name: &pr::Path) -> bool {
    for ann in &decl.annotations {
        if super::is_ident_or_func_call(&ann.expr, annotation_name) {
            return true;
        }
    }
    false
}

type HintAndSpan = (Option<String>, Option<Span>);

impl RootModule {
    /// Finds that main pipeline given a path to either main itself or its parent module.
    /// Returns main expr and fq ident of the decl.
    pub fn find_main_rel(&self, path: &[String]) -> Result<(&pr::Expr, pr::Path), HintAndSpan> {
        let (decl, ident) = self.find_main(path).map_err(|x| (x, None))?;

        let span = decl.span.clone();

        let decl = (decl.kind.as_expr())
            .filter(|e| e.ty.as_ref().unwrap().is_relation())
            .ok_or((Some(format!("{ident} is not a relational variable")), span))?;

        Ok((decl.as_ref(), ident))
    }

    pub fn find_main(&self, path: &[String]) -> Result<(&Decl, pr::Path), Option<String>> {
        let mut tried_idents = Vec::new();

        // is path referencing the relational var directly?
        if !path.is_empty() {
            let ident = pr::Path::new(path.to_vec());
            let decl = self.module.get(&ident);

            if let Some(decl) = decl {
                return Ok((decl, ident));
            } else {
                tried_idents.push(ident.to_string());
            }
        }

        // is path referencing the parent module?
        {
            let mut path = path.to_vec();
            path.push(NS_MAIN.to_string());

            let ident = pr::Path::new(path);
            let decl = self.module.get(&ident);

            if let Some(decl) = decl {
                return Ok((decl, ident));
            } else {
                tried_idents.push(ident.to_string());
            }
        }

        Err(Some(format!(
            "Expected a declaration at {}",
            tried_idents.join(" or ")
        )))
    }

    /// Finds all main pipelines.
    pub fn find_mains(&self) -> Vec<pr::Path> {
        self.module.find_by_suffix(NS_MAIN)
    }

    /// Finds declarations that are annotated with a specific name.
    pub fn find_by_annotation_name(&self, annotation_name: &pr::Path) -> Vec<pr::Path> {
        self.module.find_by_annotation_name(annotation_name)
    }
}
