mod arrow;
mod interpreter;
mod native;
mod test;

pub use interpreter::{Cell, EvalError, Interpreter, NativeFunction, evaluate};

pub trait NativeModule: Sync {
    fn lookup_native_symbol(&self, id: &str) -> Option<interpreter::NativeFunction>;
}

pub static BUILTIN_MODULES: &[(&str, &dyn NativeModule)] = &[
    ("std", &native::std::MODULE),
    ("std::text_ops", &native::std_text_ops::MODULE),
    ("std::fs", &native::std_fs::MODULE),
    ("interpreter", &native::interpreter::MODULE),
];

pub struct InterpreterRunner<'a> {
    pub modules: &'a [(&'a str, &'a dyn NativeModule)],
}

impl<'a> lutra_runner::Run for InterpreterRunner<'a> {
    type Error = EvalError;
    type Prepared = lutra_bin::rr::Program;

    async fn prepare(
        &self,
        program: lutra_bin::rr::Program,
    ) -> Result<Self::Prepared, Self::Error> {
        Ok(program)
    }

    async fn execute(
        &self,
        program: &lutra_bin::rr::Program,
        input: &[u8],
    ) -> Result<std::vec::Vec<u8>, Self::Error> {
        let program = program.as_bytecode_lt().unwrap();

        // TODO: figure out how to remove this clone
        evaluate(program, input.to_vec(), self.modules)
    }
}

impl<'a> Default for InterpreterRunner<'a> {
    fn default() -> Self {
        Self {
            modules: BUILTIN_MODULES,
        }
    }
}

#[test]
fn interpreter_layout() {
    // TODO: when we have a bench, see if boxes would yield any speed up
    insta::assert_snapshot!(std::mem::size_of::<interpreter::Cell>(), @"32");
}
