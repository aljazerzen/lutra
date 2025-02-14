use crate::decl::RootModule;
use crate::utils::IdGenerator;

mod expr;
mod functions;
mod layout;
mod scope;
mod stmt;
mod tuple;
mod types;

/// Can fold (walk) over AST and for each function call or variable find what they are referencing.
pub struct Resolver<'a> {
    root_mod: &'a mut RootModule,

    /// For recursive stmt references (recursive types and functions),
    /// we do two passes:
    /// - non-strict mode, where we try to resolve as much types as possible and don't fail on
    ///   unresolved references.
    ///   After this pass, all top-level types should be resolved.
    /// - strict mode, where we require all types to be resolved.
    pub strict_mode: bool,
    pub strict_mode_needed: bool,

    pub debug_current_decl: crate::pr::Path,

    pub id: IdGenerator<usize>,

    scopes: Vec<scope::Scope>,
}

#[derive(Default, Clone)]
pub struct ResolverOptions {}

impl Resolver<'_> {
    pub fn new(root_mod: &mut RootModule) -> Resolver {
        Resolver {
            root_mod,
            debug_current_decl: crate::pr::Path::from_name("?"),
            id: IdGenerator::new(),
            scopes: Vec::new(),
            strict_mode: false,
            strict_mode_needed: false,
        }
    }

    #[allow(dead_code)]
    fn scope_mut(&mut self) -> &mut scope::Scope {
        if self.scopes.is_empty() {
            self.scopes.push(scope::Scope::new());
        }
        self.scopes.last_mut().unwrap()
    }
}
