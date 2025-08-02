mod def;
mod expr;
mod functions;
mod inference;
mod pattern;
mod scope;
mod tuple;
mod validation;

use crate::pr;

pub fn run(
    root_module: &mut pr::ModuleDef,
    resolution_order: &[Vec<pr::Path>],
) -> Result<(), Vec<crate::diagnostic::Diagnostic>> {
    let mut resolver = TypeResolver::new(root_module);
    resolver
        .resolve_defs(resolution_order)
        .map_err(|d| vec![d])?;

    Ok(())
}

/// Can fold (walk) over AST and for each function call or variable find what they are referencing.
struct TypeResolver<'a> {
    root_mod: &'a mut pr::ModuleDef,

    debug_current_def: crate::pr::Path,

    scopes: Vec<scope::Scope>,
}

impl TypeResolver<'_> {
    fn new(root_mod: &mut pr::ModuleDef) -> TypeResolver {
        TypeResolver {
            root_mod,
            debug_current_def: crate::pr::Path::from_name("?"),
            scopes: Vec::new(),
        }
    }
}
