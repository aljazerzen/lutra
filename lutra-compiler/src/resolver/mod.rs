//! Resolver (name resolution, type checking)

mod const_eval;
mod desugar;
mod module;
mod names;
mod types;

use std::str::FromStr;

use crate::Project;
use crate::Result;
use crate::SourceTree;
use crate::diagnostic::Diagnostic;
use crate::pr;

pub const NS_STD: &str = "std";

/// Runs semantic analysis on a project.
pub fn resolve(module_tree: pr::ModuleDef) -> Result<Project, Vec<Diagnostic>> {
    // desugar
    let module_tree = desugar::run(module_tree).map_err(|d| vec![d])?;

    tracing::debug!("{:#?}", module_tree);

    // init the module structure
    let mut root_module = module::init_root(module_tree)?;

    // resolve names
    let resolution_order = names::run(&mut root_module).map_err(|d| vec![d])?;

    // tracing::debug!("{:#?}", root_module);

    // resolve types
    types::run(&mut root_module, &resolution_order)?;

    let project = Project {
        source: SourceTree::empty(),
        root_module,
        ordering: resolution_order,
    };

    Ok(project)
}

/// Runs semantic analysis of an expression within an already resolved project.
pub fn resolve_overlay_expr(
    root_module: &pr::ModuleDef,
    expr: pr::Expr,
) -> Result<pr::Expr, Vec<Diagnostic>> {
    // desugar
    let expr = desugar::run_expr(expr).map_err(|d| vec![d])?;

    let mut root_module = root_module.clone();

    // insert into the module tree
    let var_name = "_";
    root_module.defs.insert(
        var_name.to_string(),
        pr::Def::new(pr::DefKind::Unresolved(Some(Box::new(pr::DefKind::Expr(
            pr::ExprDef {
                value: Some(Box::new(expr)),
                ty: None,
                constant: false,
            },
        ))))),
    );

    // resolve names
    let resolution_order = names::run(&mut root_module).map_err(|d| vec![d])?;
    assert_eq!(resolution_order.len(), 1);
    assert_eq!(resolution_order[0].len(), 1);

    // resolve types
    types::run(&mut root_module, &resolution_order)?;

    let def = root_module.defs.swap_remove(var_name).unwrap();
    let expr = *def.kind.into_expr().unwrap().value.unwrap();

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
