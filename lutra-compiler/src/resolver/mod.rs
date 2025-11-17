//! Resolver (name resolution, type checking)

mod const_eval;
mod desugar;
mod module;
mod names;
mod prefixer;
mod types;

use crate::Project;
use crate::Result;
use crate::SourceTree;
use crate::diagnostic::Diagnostic;
use crate::pr;
use crate::project::Dependency;

pub const NS_STD: &str = "std";

/// Runs semantic analysis on a project.
pub fn resolve(
    module_tree: pr::ModuleDef,
    dependencies: Vec<Dependency>,
) -> Result<Project, Vec<Diagnostic>> {
    // desugar
    let module_tree = desugar::run(module_tree).map_err(|d| vec![d])?;

    tracing::debug!("{:#?}", module_tree);

    // init the module structure
    let mut root_module = module::init_root(module_tree)?;

    // inject dependencies
    for dep in &dependencies {
        let dep_module = dep.inner.root_module.clone();
        let dep_module = prefixer::prefix(dep_module, dep.name.clone());
        let def = pr::Def::new(dep_module);
        root_module.defs.insert(dep.name.clone(), def);
    }

    // resolve names
    let resolution_order = names::run(&mut root_module).map_err(|d| vec![d])?;

    // tracing::debug!("{:#?}", root_module);

    // resolve types
    types::run(&mut root_module, &resolution_order)?;

    let project = Project {
        source: SourceTree::empty(),
        root_module,
        ordering: resolution_order,
        dependencies,
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
