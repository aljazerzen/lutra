use crate::ir::decl;
use crate::pr;

pub fn init_module_tree(root_module_def: pr::ModuleDef) -> decl::RootModule {
    let mut root = decl::Module::new_root();

    populate_module(&mut root, root_module_def.stmts);

    decl::RootModule {
        module: root,
        ordering: Vec::new(), // computed later
    }
}

fn populate_module(module: &mut decl::Module, stmts: Vec<pr::Stmt>) {
    for stmt in stmts {
        let name = get_stmt_name(&stmt).to_string();

        let kind = match stmt.kind {
            pr::StmtKind::ModuleDef(module_def) => {
                // init new module and recurse
                let mut new_mod = decl::Module::default();
                populate_module(&mut new_mod, module_def.stmts);

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
        module.names.insert(name, decl);
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
