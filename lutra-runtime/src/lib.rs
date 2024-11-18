mod interpreter;
mod native;
mod test;

pub use interpreter::{evaluate, Cell, Interpreter};

pub trait NativeModule: Sync {
    fn lookup_native_symbol(&self, id: &str) -> interpreter::Cell;
}

pub static BUILTIN_MODULES: &[(&str, &dyn NativeModule)] = &[
    ("core_int", &native::core::int::MODULE),
    ("core_array", &native::core::array::MODULE),
    ("interpreter", &native::interpreter::MODULE),
];

#[test]
fn interpreter_layout() {
    // TODO: when we have a bench, see if boxes would yield any speed up
    insta::assert_snapshot!(std::mem::size_of::<interpreter::Cell>(), @"32");
}
