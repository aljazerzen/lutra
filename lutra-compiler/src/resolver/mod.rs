//! Resolver (name resolution, type checking)

mod desugar;
mod module;
mod names;
mod types;

use std::str::FromStr;

use crate::decl;
use crate::diagnostic::Diagnostic;
use crate::pr;
use crate::Result;

pub const NS_STD: &str = "std";

/// Runs semantic analysis on the query.
pub fn resolve(module_tree: pr::ModuleDef) -> Result<decl::RootModule, Vec<Diagnostic>> {
    // desugar
    let module_tree = desugar::run(module_tree).map_err(|d| vec![d])?;

    // init the module structure
    let mut root_module = module::init_root(module_tree);

    // resolve names
    let resolution_order = names::resolve_names(&mut root_module).map_err(|d| vec![d])?;

    // resolve types
    types::resolve_types(&mut root_module, &resolution_order)?;

    root_module.ordering = resolution_order;
    Ok(root_module)
}

/// Preferred way of injecting std module.
pub fn load_std_lib(source: &mut crate::SourceTree) {
    let path = std::path::PathBuf::from_str("std.lt").unwrap();
    if source.get_source(&path).is_none() {
        source.insert(path, include_str!("std.lt").to_string());
    }
}
