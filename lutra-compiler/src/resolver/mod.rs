//! Resolver (name resolution, type checking)

mod const_eval;
mod desugar;
mod module;
mod names;
mod types;

use std::str::FromStr;

use crate::Result;
use crate::decl;
use crate::diagnostic::Diagnostic;
use crate::pr;

pub const NS_STD: &str = "std";

/// Runs semantic analysis on a project.
pub fn resolve(module_tree: pr::ModuleDef) -> Result<decl::RootModule, Vec<Diagnostic>> {
    // desugar
    let module_tree = desugar::run(module_tree).map_err(|d| vec![d])?;

    // init the module structure
    let mut root_module = module::init_root(module_tree)?;

    // resolve names
    let resolution_order = names::run(&mut root_module).map_err(|d| vec![d])?;

    tracing::debug!("{:#?}", root_module);

    // resolve types
    types::run(&mut root_module, &resolution_order)?;

    root_module.ordering = resolution_order;

    // resolve types
    const_eval::run(&root_module)?;

    Ok(root_module)
}

/// Runs semantic analysis of an expression within an already resolved project.
pub fn resolve_overlay_expr(
    root_module: &decl::RootModule,
    expr: pr::Expr,
) -> Result<pr::Expr, Vec<Diagnostic>> {
    // desugar
    let expr = desugar::run_expr(expr).map_err(|d| vec![d])?;

    let mut root_module = root_module.clone();

    // insert into the module tree
    let var_name = "_";
    root_module.module.names.insert(
        var_name.to_string(),
        decl::Decl::new(decl::DeclKind::Unresolved(Some(pr::StmtKind::VarDef(
            pr::VarDef {
                name: var_name.to_string(),
                value: Some(Box::new(expr)),
                ty: None,
            },
        )))),
    );

    // resolve names
    let resolution_order = names::run(&mut root_module).map_err(|d| vec![d])?;
    assert_eq!(resolution_order.len(), 1);
    assert_eq!(resolution_order[0].len(), 1);

    // resolve types
    types::run(&mut root_module, &resolution_order)?;

    let decl = root_module.module.names.swap_remove(var_name).unwrap();
    let expr = *decl.kind.into_expr().unwrap();

    Ok(expr)
}

/// Preferred way of injecting std module.
pub fn load_std_lib(source: &mut crate::SourceTree) {
    use crate::project::SourceProvider;

    let path = std::path::PathBuf::from_str("std.lt").unwrap();
    if source.get_source(&path).is_none() {
        source.insert(path, include_str!("std.lt").to_string());
    }
}
