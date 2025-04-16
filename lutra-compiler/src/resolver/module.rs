use enum_as_inner::EnumAsInner;
use indexmap::IndexMap;
use itertools::Itertools;

use crate::decl;
use crate::pr;
use crate::Span;

use super::NS_STD;

pub fn init_root(root_module_def: pr::ModuleDef) -> decl::RootModule {
    let mut root = decl::Module {
        names: IndexMap::from([(NS_STD.to_string(), decl::Decl::new(decl::Module::default()))]),
    };

    root.populate_module(root_module_def.stmts);

    decl::RootModule {
        module: root,
        ordering: Vec::new(), // computed later
    }
}

#[derive(Debug, EnumAsInner)]
pub enum ExprOrTy<'a> {
    Expr(&'a pr::Expr),
    Ty(&'a pr::Ty),
}

impl decl::Module {
    /// Get declaration by fully qualified ident.
    pub fn get(&self, fq_ident: &pr::Path) -> Option<ExprOrTy> {
        let sub_module = self.get_submodule(fq_ident.path())?;
        let decl = sub_module.names.get(fq_ident.name())?;
        match &decl.kind {
            decl::DeclKind::Expr(expr) => Some(ExprOrTy::Expr(expr)),
            decl::DeclKind::Ty(ty) => Some(ExprOrTy::Ty(ty)),
            decl::DeclKind::Unresolved(_) => {
                panic!("unresolved")
            }
            decl::DeclKind::Module(_) | decl::DeclKind::Import(_) => None,
        }
    }

    /// Get declaration by fully qualified ident and return remaining steps into the decl.
    pub fn try_get<'a, 's>(
        &'a self,
        steps: &'s [String],
    ) -> Option<(&'a decl::Decl, &'s [String])> {
        let mut curr_mod = self;
        for (index, step) in steps.iter().enumerate() {
            let decl = curr_mod.names.get(step)?;
            if let decl::DeclKind::Module(sub_module) = &decl.kind {
                curr_mod = sub_module;
            } else {
                return Some((decl, &steps[(index + 1)..]));
            }
        }
        None
    }

    /// Get an exclusive reference to declaration by fully qualified ident.
    pub fn get_mut(&mut self, ident: &pr::Path) -> Option<&mut decl::Decl> {
        let module = self.get_submodule_mut(ident.path())?;
        module.names.get_mut(ident.name())
    }

    pub fn get_submodule(&self, path: &[String]) -> Option<&decl::Module> {
        let mut curr_mod = self;
        for step in path {
            let decl = curr_mod.names.get(step)?;
            curr_mod = decl.kind.as_module()?;
        }
        Some(curr_mod)
    }

    pub fn get_submodule_mut(&mut self, path: &[String]) -> Option<&mut decl::Module> {
        let mut curr_mod = self;
        for step in path {
            let decl = curr_mod.names.get_mut(step)?;
            curr_mod = decl.kind.as_module_mut()?;
        }
        Some(curr_mod)
    }

    pub fn iter_decls(&self) -> impl Iterator<Item = (&String, &decl::Decl)> {
        self.names.iter()
    }

    pub fn iter_decls_re(&self) -> impl Iterator<Item = (pr::Path, &decl::Decl)> {
        let non_modules = (self.names.iter())
            .filter(|(_, d)| !d.kind.is_module())
            .map(|(name, d)| (pr::Path::from_name(name), d));

        let sub_decls = (self.names.iter())
            .filter(|(_, d)| d.kind.is_module())
            .flat_map(|(name, d)| {
                let sub_module = d.kind.as_module().unwrap();
                sub_module
                    .iter_decls_re()
                    .map(|(p, d)| (p.prepend(vec![name.clone()]), d))
                    .collect_vec()
            });

        non_modules.chain(sub_decls)
    }

    pub(super) fn take_unresolved(&mut self, ident: &pr::Path) -> (pr::StmtKind, Option<Span>) {
        let decl = self.get_mut(ident).unwrap();
        let unresolved = decl.kind.as_unresolved_mut().unwrap();
        (unresolved.take().unwrap(), decl.span)
    }

    pub(super) fn populate_module(&mut self, stmts: Vec<pr::Stmt>) {
        for stmt in stmts {
            let name = get_stmt_name(&stmt).to_string();

            let kind = match stmt.kind {
                pr::StmtKind::ModuleDef(module_def) => {
                    // init new module and recurse
                    let mut new_mod = decl::Module::default();
                    new_mod.populate_module(module_def.stmts);

                    decl::DeclKind::Module(new_mod)
                }
                kind => {
                    // insert "DeclKind::Unresolved"
                    decl::DeclKind::Unresolved(Some(kind))
                }
            };
            let decl = decl::Decl {
                span: stmt.span,
                kind,
                annotations: stmt.annotations,
            };
            self.names.insert(name, decl);
        }
    }
}

fn get_stmt_name(stmt: &pr::Stmt) -> &str {
    match &stmt.kind {
        pr::StmtKind::VarDef(pr::VarDef { name, .. }) => name,
        pr::StmtKind::TypeDef(pr::TypeDef { name, .. }) => name,
        pr::StmtKind::ModuleDef(pr::ModuleDef { name, .. }) => name,
        pr::StmtKind::ImportDef(pr::ImportDef { name, alias }) => {
            alias.as_deref().unwrap_or(name.name())
        }
    }
}
