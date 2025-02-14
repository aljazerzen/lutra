//! Semantic resolver (name resolution, type checking and lowering to RQ)

pub mod desugar;
mod module;
mod resolve_decls;
mod resolver;

use std::str::FromStr;

use self::resolver::Resolver;

use crate::decl;
use crate::diagnostic::Diagnostic;
use crate::pr;
use crate::Result;

/// Runs semantic analysis on the query.
pub fn resolve(module_tree: pr::ModuleDef) -> Result<decl::RootModule, Vec<Diagnostic>> {
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
pub fn load_std_lib(source: &mut crate::SourceTree) {
    let path = std::path::PathBuf::from_str("std.lt").unwrap();
    if source.get_source(&path).is_none() {
        source.insert(path, include_str!("std.lt").to_string());
    }
}

pub fn decl_get_annotation<'e>(
    decl: &'e decl::Decl,
    annotation_name: &pr::Path,
) -> Option<&'e [pr::Expr]> {
    for ann in &decl.annotations {
        if let Some(args) = as_ident_or_func_call(&ann.expr, annotation_name) {
            return Some(args);
        }
    }
    None
}

pub fn as_ident_or_func_call<'e>(expr: &'e pr::Expr, name: &pr::Path) -> Option<&'e [pr::Expr]> {
    match &expr.kind {
        pr::ExprKind::Ident(i) if i == name => Some(&[]),
        pr::ExprKind::FuncCall(pr::FuncCall { func: n_expr, args })
            if n_expr.kind.as_ident().map_or(false, |i| i == name) =>
        {
            Some(args.as_slice())
        }
        _ => None,
    }
}

pub const NS_STD: &str = "std";
pub const NS_MAIN: &str = "main";
