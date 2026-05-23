mod python;
mod rust;
mod sql;

pub use lutra_compiler::ProgramRepr;

use std::borrow::Cow;
use std::{fs, path};

use lutra_bin::{Encode, ir};
use lutra_compiler::DiscoverParams;

/// Wrapper for [generate], designed to play well with Cargo's `build.rs`.
///
/// Writes into `OUT_DIR/lutra.rs`.
/// Prints cargo::rerun-if-changed directives.
/// Panics on error.
#[track_caller]
pub fn check_and_generate(project_dir: impl AsRef<path::Path>, options: GenerateOptions) {
    let out_dir = path::PathBuf::from(std::env::var_os("OUT_DIR").unwrap());
    let out_path = out_dir.join("lutra.rs");

    // tracing_subscriber::fmt::Subscriber::builder()
    //     .without_time()
    //     .with_target(false)
    //     .with_max_level(tracing::Level::DEBUG)
    //     .with_writer(std::io::stderr)
    //     .init();

    // discover
    let source = lutra_compiler::discover(DiscoverParams {
        project: Some(project_dir.as_ref().to_path_buf()),
    });
    let source = match source {
        Ok(s) => s,
        Err(e) => panic!("{e}"),
    };

    // compile
    let project = lutra_compiler::check(source, Default::default());
    let project = match project {
        Ok(p) => p,
        Err(e) => panic!("{e}"),
    };

    // generate
    let code = rust::run(&project, &options, out_dir).unwrap();
    fs::write(out_path, code).unwrap();

    // print directives with absolute paths for reliability
    for (p, _) in project.source.get_sources() {
        let abs_path = project.source.get_absolute_path(p);
        println!("cargo::rerun-if-changed={}", abs_path.display());
    }
}

#[track_caller]
pub fn generate(
    project: &lutra_compiler::Project,
    target: Target,
    out_path: impl AsRef<path::Path>,
    options: GenerateOptions,
) -> Result<(), lutra_compiler::error::Error> {
    let out_path = out_path.as_ref();
    let out_dir = out_path.parent().unwrap().to_path_buf();

    // generate
    let code = match target {
        Target::Rust => rust::run(project, &options, out_dir).unwrap(),
        Target::Python => python::run(project, &options, out_dir).unwrap(),
        Target::Sql => sql::run(project, &options, out_dir).unwrap(),
    };

    fs::write(out_path, code)?;

    Ok(())
}

pub enum Target {
    Rust,
    Python,
    Sql,
}

#[track_caller]
pub fn generate_program_bytecode(project_dir: &path::Path, program: &str, out_file: &path::Path) {
    // discover the project
    let source = lutra_compiler::discover(DiscoverParams {
        project: Some(project_dir.into()),
    })
    .unwrap();

    // compile
    let project =
        lutra_compiler::check(source, Default::default()).unwrap_or_else(|e| panic!("{e}"));

    // lower & bytecode
    let params =
        lutra_compiler::CompileParams::new(program, lutra_compiler::ProgramRepr::BytecodeLt);
    let (program, _ty) = lutra_compiler::compile(&project, &params).unwrap();

    let buf = program.into_bytecode_lt().unwrap().encode();
    std::fs::write(out_file, buf).unwrap();
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "clap", derive(clap::Args))]
pub struct GenerateOptions {
    #[cfg_attr(feature = "clap", arg(long = "no-types", default_value_t = false))]
    pub no_types: bool,

    #[cfg_attr(
        feature = "clap",
        arg(long = "no-encode-decode", default_value_t = false)
    )]
    pub no_encode_decode: bool,

    #[cfg_attr(
        feature = "clap",
        arg(
            long = "no-function-traits",
            default_value_t = true,
            num_args = 0..=1,
            default_missing_value = "true"
        )
    )]
    pub no_function_traits: bool,

    #[cfg_attr(feature = "clap", arg(long))]
    pub client: bool,

    #[cfg_attr(feature = "clap", arg(long))]
    pub programs_bytecode_lt: Vec<String>,

    #[cfg_attr(feature = "clap", arg(long))]
    pub programs_sql_pg: Vec<String>,

    #[cfg_attr(feature = "clap", arg(long))]
    pub programs_sql_duckdb: Vec<String>,

    #[cfg_attr(feature = "clap", arg(long, default_value = "::lutra_bin"))]
    pub lutra_bin_path: String,

    #[cfg_attr(feature = "clap", arg(long, default_value = "::lutra_runner"))]
    pub lutra_runner_path: String,
}

impl Default for GenerateOptions {
    fn default() -> Self {
        Self {
            no_types: false,
            no_encode_decode: false,
            no_function_traits: true,
            client: false,
            programs_bytecode_lt: Vec::new(),
            programs_sql_pg: Vec::new(),
            programs_sql_duckdb: Vec::new(),
            lutra_bin_path: "::lutra_bin".into(),
            lutra_runner_path: "::lutra_runner".into(),
        }
    }
}

impl GenerateOptions {
    /// Do not generate type definitions
    pub fn no_generate_types(mut self) -> Self {
        self.no_types = true;
        self
    }

    /// Do not generate [lutra_bin::Encode] and [lutra_bin::Decode] implementations
    pub fn no_generate_encode_decode(mut self) -> Self {
        self.no_encode_decode = true;
        self
    }

    /// Do not generate traits for functions
    pub fn generate_function_traits(mut self) -> Self {
        self.no_function_traits = false;
        self
    }

    /// Generate module client wrappers for generated programs
    pub fn generate_client(mut self) -> Self {
        self.client = true;
        self
    }

    /// Set path to [lutra_bin] dependency
    pub fn with_lutra_bin_path(mut self, path: String) -> Self {
        self.lutra_bin_path = path;
        self
    }

    /// Set path to [lutra_runner] dependency
    pub fn with_lutra_runner_path(mut self, path: String) -> Self {
        self.lutra_runner_path = path;
        self
    }

    /// Generates programs for all functions in a module
    pub fn generate_programs(mut self, module_path: impl ToString, fmt: ProgramRepr) -> Self {
        let module_path = module_path.to_string();
        match fmt {
            ProgramRepr::BytecodeLt => self.programs_bytecode_lt.push(module_path),
            ProgramRepr::SqlPg => self.programs_sql_pg.push(module_path),
            ProgramRepr::SqlDuckdb => self.programs_sql_duckdb.push(module_path),
        }
        self
    }

    pub(crate) fn generates_types(&self) -> bool {
        !self.no_types
    }

    pub(crate) fn generates_encode_decode(&self) -> bool {
        !self.no_encode_decode
    }

    pub(crate) fn generates_function_traits(&self) -> bool {
        !self.no_function_traits
    }

    pub(crate) fn generates_client(&self) -> bool {
        self.client
    }

    pub(crate) fn included_program_repr(&self, module_path: &str) -> Option<ProgramRepr> {
        if self.programs_bytecode_lt.iter().any(|p| p == module_path) {
            Some(ProgramRepr::BytecodeLt)
        } else if self.programs_sql_pg.iter().any(|p| p == module_path) {
            Some(ProgramRepr::SqlPg)
        } else if self.programs_sql_duckdb.iter().any(|p| p == module_path) {
            Some(ProgramRepr::SqlDuckdb)
        } else {
            None
        }
    }

    pub(crate) fn has_programs_in_subtree(&self, module_path: &str) -> bool {
        self.programs_bytecode_lt
            .iter()
            .chain(self.programs_sql_pg.iter())
            .chain(self.programs_sql_duckdb.iter())
            .any(|p| {
                p == module_path
                    || (!module_path.is_empty()
                        && p.starts_with(module_path)
                        && p[module_path.len()..].starts_with("::"))
                    || (module_path.is_empty() && !p.is_empty())
            })
    }
}

/// Types might not have names, because they are defined inline.
/// This function traverses a type definition and generates names for all of the types.
fn infer_names(def_name: &str, ty: &mut ir::Ty) {
    if ty.name.is_none() {
        ty.name = Some(def_name.to_string());
    }

    let mut name_prefix = Vec::new();
    infer_names_re(ty, &mut name_prefix);
}

fn infer_names_of_program_ty(ty: &mut lutra_bin::rr::ProgramType, program_name: &str) {
    let mut name_camel = vec![snake_to_sentence(program_name)];
    {
        name_camel.push("Input".into());
        infer_names_re(&mut ty.input, &mut name_camel);
        name_camel.pop();
    }
    {
        name_camel.push("Output".into());
        infer_names_re(&mut ty.output, &mut name_camel);
        name_camel.pop();
    }
}

fn infer_names_re(ty: &mut ir::Ty, name_prefix: &mut Vec<String>) {
    if ty.name.is_none() {
        ty.name = Some(name_prefix.concat());
    } else {
        name_prefix.push(ty.name.clone().unwrap());
    }

    match &mut ty.kind {
        ir::TyKind::Primitive(_) | ir::TyKind::Ident(_) => {}

        ir::TyKind::Tuple(fields) => {
            for (index, field) in fields.iter_mut().enumerate() {
                let name = tuple_field_name(&field.name, index);
                name_prefix.push(name.into_owned());

                infer_names_re(&mut field.ty, name_prefix);
                name_prefix.pop();
            }
        }

        ir::TyKind::Array(items_ty) => {
            name_prefix.push("Items".to_string());
            infer_names_re(items_ty, name_prefix);
            name_prefix.pop();
        }

        ir::TyKind::Enum(variants) => {
            for v in variants {
                name_prefix.push(v.name.clone());
                infer_names_re(&mut v.ty, name_prefix);
                name_prefix.pop();
            }
        }

        ir::TyKind::Function(func) => {
            for param in &mut func.params {
                infer_names_re(param, name_prefix);
            }
            infer_names_re(&mut func.body, name_prefix);
        }
    }
}

pub fn tuple_field_name(name: &Option<String>, index: usize) -> Cow<'_, str> {
    (name.as_ref())
        .map(|x| Cow::Borrowed(x.as_str()))
        .unwrap_or_else(|| format!("field{index}").into())
}

fn camel_to_snake(camel: &str) -> String {
    let mut snake = String::with_capacity(camel.len());
    for current in camel.chars() {
        if current.is_uppercase() {
            if !snake.is_empty() && snake.ends_with('_') {
                snake.push('_');
            }
            snake.push(current.to_lowercase().next().unwrap());
        } else {
            snake.push(current);
        }
    }

    snake
}

fn snake_to_sentence(snake: &str) -> String {
    let mut sentence = String::with_capacity(snake.len());
    let mut next_upper = true;
    for current in snake.chars() {
        if current == '_' {
            next_upper = true;
            continue;
        }

        if next_upper {
            sentence.push(current.to_uppercase().next().unwrap());
        } else {
            sentence.push(current.to_lowercase().next().unwrap());
        }
        next_upper = false;
    }

    sentence
}
