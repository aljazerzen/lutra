mod interpreter;
mod native;
mod test;

pub use interpreter::{evaluate, Cell, Interpreter};

#[test]
fn interpreter_layout() {
    insta::assert_snapshot!(std::mem::size_of::<interpreter::Cell>(), @"24");
}

pub trait NativeModule: Sync {
    fn lookup_native_symbol(&self, id: &str) -> interpreter::Cell;
}

pub static BUILTIN_MODULES: &[(&str, &dyn NativeModule)] = &[
    ("core_int", &native::core::int::MODULE),
    ("core_array", &native::core::array::MODULE),
    ("interpreter", &native::interpreter::MODULE),
];
