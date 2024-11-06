use crate::ir::decl::RootModule;
use crate::utils::IdGenerator;

mod expr;
mod functions;
mod scope;
mod stmt;
mod tuple;
mod types;

/// Can fold (walk) over AST and for each function call or variable find what they are referencing.
pub struct Resolver<'a> {
    root_mod: &'a mut RootModule,

    pub debug_current_decl: crate::pr::Path,

    /// Sometimes ident closures must be resolved and sometimes not. See [test::test_func_call_resolve].
    in_func_call_name: bool,

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
            in_func_call_name: false,
            id: IdGenerator::new(),
            scopes: Vec::new(),
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
