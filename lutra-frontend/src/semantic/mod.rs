//! Semantic resolver (name resolution, type checking and lowering to RQ)

pub mod desugar;
mod module;
mod resolve_decls;
mod resolver;

use std::str::FromStr;

use self::resolver::Resolver;

use crate::decl::RootModule;
use crate::diagnostic::Diagnostic;
use crate::pr;
use crate::Result;

/// Runs semantic analysis on the query.
pub fn resolve(module_tree: pr::ModuleDef) -> Result<RootModule, Vec<Diagnostic>> {
    // expand AST into PL
    let root_module_def = desugar::run(module_tree).map_err(|d| vec![d])?;

    // init the module structure
    let mut root_module = resolve_decls::init_module_tree(root_module_def);

    // resolve name references between declarations
    let resolution_order =
        resolve_decls::resolve_decl_refs(&mut root_module).map_err(|d| vec![d])?;

    // resolve
    let mut resolver = Resolver::new(&mut root_module);
    resolver
        .resolve_decls(resolution_order.as_slice())
        .map_err(|d| vec![d])?;

    root_module.ordering = resolution_order;
    Ok(root_module)
}

/// Preferred way of injecting std module.
#[allow(dead_code)]
pub fn load_std_lib(source: &mut crate::SourceTree) {
    let path = std::path::PathBuf::from_str("std.lt").unwrap();
    if source.get_source(&path).is_none() {
        source.insert(path, include_str!("std.lt").to_string());
    }
}

pub fn is_ident_or_func_call(expr: &pr::Expr, name: &pr::Path) -> bool {
    match &expr.kind {
        pr::ExprKind::Ident(i) if i == name => true,
        pr::ExprKind::FuncCall(pr::FuncCall { name: n_expr, .. })
            if n_expr.kind.as_ident().map_or(false, |i| i == name) =>
        {
            true
        }
        _ => false,
    }
}

pub const NS_STD: &str = "std";
pub const NS_THIS: &str = "this";
pub const NS_MAIN: &str = "main";
