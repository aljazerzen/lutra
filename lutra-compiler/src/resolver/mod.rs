//! Resolver (name resolution, type checking)

mod const_eval;
mod desugar;
mod names;
mod prefixer;
mod recursive;
mod types;

use crate::Project;
use crate::Result;
use crate::SourceTree;
use crate::diagnostic::Diagnostic;
use crate::pr;
use crate::project::Dependency;

pub const NS_STD: &str = "std";
pub(crate) use desugar::{TY_DOMAIN_NUMBER, TY_DOMAIN_PRIMITIVE};

/// Runs semantic analysis on a project.
pub fn resolve(
    module_tree: pr::ModuleDef,
    dependencies: Vec<Dependency>,
    is_std: bool,
) -> Result<Project, Vec<Diagnostic>> {
    // desugar
    let mut root_module = desugar::run(module_tree).map_err(|d| vec![d])?;

    tracing::trace!("{:#?}", root_module);
    let unresolved = root_module.collect_unresolved();

    // inject dependencies
    for dep in &dependencies {
        let dep_module = dep.inner.root_module.clone();
        let dep_module = prefixer::prefix(dep_module, dep.name.clone());
        let def = pr::Def::new(dep_module);
        root_module.defs.insert(dep.name.clone(), def);
    }

    // resolve names
    let (resolution_order, target_spans) =
        names::run(&mut root_module, unresolved, is_std).map_err(|d| vec![d])?;

    // resolve types
    types::run(&mut root_module, &resolution_order, is_std)?;

    let target_map = crate::project::TargetMap::build(target_spans);

    let project = Project {
        source: SourceTree::empty(),
        root_module,
        ordering: resolution_order,
        dependencies,
        target_map,
    };

    // resolve layout
    let project = recursive::check(project)?;

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
        pr::Def::new(pr::DefKind::Expr(pr::ExprDef {
            value: Box::new(expr),
            ty: None,
            constant: false,
        })),
    );

    // resolve names
    let unresolved = std::collections::HashSet::from([pr::Path::from_name(var_name)]);
    let (resolution_order, _) =
        names::run(&mut root_module, unresolved, false).map_err(|d| vec![d])?;
    assert_eq!(resolution_order.len(), 1);
    assert_eq!(resolution_order[0].len(), 1);

    // resolve types
    types::run(&mut root_module, &resolution_order, false)?;

    let def = root_module.defs.swap_remove(var_name).unwrap();
    Ok(*def.kind.into_expr().unwrap().value)
}
