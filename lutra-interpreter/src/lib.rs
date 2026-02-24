mod data;
mod interpreter;
mod native;
mod writer;

pub use data::Data;
pub use interpreter::{Cell, EvalError, Interpreter, NativeFunction, evaluate};
pub use writer::{ArrayWriter, EnumWriter, TupleWriter};

pub use lutra_runner::{Run, RunSync};

use lutra_runner::proto;

pub trait NativeModule: Sync {
    fn lookup_native_symbol(&self, id: &str) -> Option<interpreter::NativeFunction>;
}

pub static BUILTIN_MODULES: &[(&str, &dyn NativeModule)] = &[
    ("std", &native::std::Module),
    ("std::text", &native::std_text::Module),
    ("std::math", &native::std_math::Module),
    ("std::fs", &native::std_fs::Module),
    ("std::date", &native::std_date::Module),
    ("interpreter", &native::interpreter::Module),
];

pub struct InterpreterRunner<'a> {
    modules: &'a [(&'a str, &'a dyn NativeModule)],
    file_system: Option<std::path::PathBuf>,

    next_program_id: u32,
    programs: std::collections::HashMap<u32, lutra_bin::rr::Program>,
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
            next_program_id: 0,
            programs: std::collections::HashMap::new(),
        }
    }
}

impl<'a> lutra_runner::RunSync for InterpreterRunner<'a> {
    fn prepare_sync(&mut self, program: lutra_bin::rr::Program) -> Result<u32, proto::Error> {
        let program_id = self.next_program_id;
        self.next_program_id += 1;
        self.programs.insert(program_id, program);
        Ok(program_id)
    }

    fn execute_sync(
        &mut self,
        program_id: u32,
        input: &[u8],
    ) -> Result<std::vec::Vec<u8>, proto::Error> {
        let program = (self.programs.get(&program_id))
            .ok_or_else(|| proto::Error::program_not_found(program_id))?;
        let program = program.as_bytecode_lt().unwrap();

        evaluate(
            program,
            input.to_vec(),
            self.modules,
            self.file_system.clone(),
        )
        .map_err(|e| proto::Error {
            display: format!("{}", e),
            code: None,
        })
    }

    fn release_sync(&mut self, program_id: u32) -> Result<(), proto::Error> {
        self.programs.remove(&program_id);
        Ok(())
    }

    fn pull_schema_sync(&mut self) -> Result<std::string::String, proto::Error> {
        let Some(fs) = &self.file_system else {
            return Ok(String::new());
        };

        // TODO: error
        Ok(lutra_arrow::pull_schema(fs).unwrap())
    }
}
