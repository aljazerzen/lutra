use std::collections::HashMap;

use crate::ir::decl;
use crate::ir::pl;
use crate::utils::IdGenerator;
use crate::Span;

pub fn init_module_tree(root_module_def: pl::ModuleDef) -> decl::RootModule {
    let mut root = decl::Module::new_root();

    let mut ctx = Context {
        span_map: Default::default(),
        id: IdGenerator::new(),
    };

    ctx.populate_module(&mut root, root_module_def.stmts);

    decl::RootModule {
        module: root,
        span_map: ctx.span_map,
    }
}

struct Context {
    span_map: HashMap<usize, Span>,
    id: IdGenerator<usize>,
}

impl Context {
    fn populate_module(&mut self, module: &mut decl::Module, stmts: Vec<pl::Stmt>) {
        for (index, stmt) in stmts.into_iter().enumerate() {
            let id = self.id.gen();
            if let Some(span) = stmt.span {
                self.span_map.insert(id, span);
            }

            let name = stmt.name().to_string();

            let kind = match stmt.kind {
                pl::StmtKind::ModuleDef(module_def) => {
                    // init new module and recurse
                    let mut new_mod = decl::Module::default();
                    self.populate_module(&mut new_mod, module_def.stmts);

                    decl::DeclKind::Module(new_mod)
                }
                kind => {
                    // insert "DeclKind::Unresolved"
                    decl::DeclKind::Unresolved(kind)
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
