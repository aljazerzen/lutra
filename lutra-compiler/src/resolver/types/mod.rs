mod expr;
mod functions;
mod inference;
mod layout;
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

    /// For recursive stmt references (recursive types and functions),
    /// we do two passes:
    /// - non-strict mode, where we try to resolve as much types as possible and don't fail on
    ///   unresolved references.
    ///   After this pass, all top-level types should be resolved.
    /// - strict mode, where we require all types to be resolved.
    strict_mode: bool,
    strict_mode_needed: bool,

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
            strict_mode: false,
            strict_mode_needed: false,
        }
    }
}
