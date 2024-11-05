use std::collections::HashMap;

use crate::ir::decl;
use crate::pr;
use crate::utils::IdGenerator;
use crate::Span;

pub fn init_module_tree(root_module_def: pr::ModuleDef) -> decl::RootModule {
    let mut root = decl::Module::new_root();

    let mut ctx = Context {
        span_map: Default::default(),
        id: IdGenerator::new(),
    };

    ctx.populate_module(&mut root, root_module_def.stmts);

    decl::RootModule {
        module: root,
        ordering: Vec::new(), // computed later
        span_map: ctx.span_map,
    }
}

struct Context {
    span_map: HashMap<usize, Span>,
    id: IdGenerator<usize>,
}

impl Context {
    fn populate_module(&mut self, module: &mut decl::Module, stmts: Vec<pr::Stmt>) {
        for (index, stmt) in stmts.into_iter().enumerate() {
            let id = self.id.gen();
            if let Some(span) = stmt.span {
                self.span_map.insert(id, span);
            }

            let name = get_stmt_name(&stmt).to_string();

            let kind = match stmt.kind {
                pr::StmtKind::ModuleDef(module_def) => {
                    // init new module and recurse
                    let mut new_mod = decl::Module::default();
                    self.populate_module(&mut new_mod, module_def.stmts);

                    decl::DeclKind::Module(new_mod)
                }
                kind => {
                    // insert "DeclKind::Unresolved"
                    decl::DeclKind::Unresolved(Some(kind))
                }
            };
            let decl = decl::Decl {
                declared_at: Some(id),
                kind,
                order: index + 1,
                annotations: stmt.annotations,
            };
            module.names.insert(name, decl);
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
