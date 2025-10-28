mod arrow;
mod data;
mod interpreter;
mod native;
mod writer;

pub use data::Data;
pub use interpreter::{Cell, EvalError, Interpreter, NativeFunction, evaluate};
pub use writer::{ArrayWriter, EnumWriter, TupleWriter};

pub trait NativeModule: Sync {
    fn lookup_native_symbol(&self, id: &str) -> Option<interpreter::NativeFunction>;
}

pub static BUILTIN_MODULES: &[(&str, &dyn NativeModule)] = &[
    ("std", &native::std::Module),
    ("std::text", &native::std_text::Module),
    ("std::math", &native::std_math::Module),
    ("std::fs", &native::std_fs::Module),
    ("interpreter", &native::interpreter::Module),
];

pub struct InterpreterRunner<'a> {
    modules: &'a [(&'a str, &'a dyn NativeModule)],

    file_system: Option<std::path::PathBuf>,
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

        evaluate(
            program,
            input.to_vec(), // TODO: figure out how to remove this clone
            self.modules,
            self.file_system.clone(),
        )
    }
}

impl<'a> InterpreterRunner<'a> {
    pub fn with_file_system(mut self, file_system: Option<std::path::PathBuf>) -> Self {
        self.file_system = file_system;
        self
    }
}

impl<'a> Default for InterpreterRunner<'a> {
    fn default() -> Self {
        Self {
            modules: BUILTIN_MODULES,
            file_system: None,
        }
    }
}
