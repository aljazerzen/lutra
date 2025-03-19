mod expr;
mod functions;
mod inference;
mod scope;
mod stmt;
mod tuple;
mod validation;

use crate::decl::RootModule;
use crate::pr;
use crate::utils::IdGenerator;

pub fn run(
    root_module: &mut RootModule,
    resolution_order: &[Vec<pr::Path>],
) -> Result<(), Vec<crate::diagnostic::Diagnostic>> {
    let mut resolver = TypeResolver::new(root_module);
    resolver
        .resolve_decls(resolution_order)
        .map_err(|d| vec![d])?;

    Ok(())
}

/// Can fold (walk) over AST and for each function call or variable find what they are referencing.
struct TypeResolver<'a> {
    root_mod: &'a mut RootModule,

    debug_current_decl: crate::pr::Path,

    id: IdGenerator<usize>,

    scopes: Vec<scope::Scope>,
}

impl TypeResolver<'_> {
    fn new(root_mod: &mut RootModule) -> TypeResolver {
        TypeResolver {
            root_mod,
            debug_current_decl: crate::pr::Path::from_name("?"),
            id: IdGenerator::new(),
            scopes: Vec::new(),
        }
    }
}
