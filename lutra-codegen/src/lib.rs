mod cpp;
mod python;
mod rust;
mod sql;

pub use lutra_compiler::ProgramRepr;

use std::borrow::Cow;
use std::collections::HashMap;
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
    let code = rust::run(&project, &options, out_dir);
    let code = match code {
        Ok(s) => s,
        Err(e) => panic!("{e}"),
    };
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
) -> Result<(), Error> {
    let out_path = out_path.as_ref();
    let out_dir = out_path
        .parent()
        .unwrap_or_else(|| path::Path::new("."))
        .to_path_buf();

    // generate
    let code = match target {
        Target::Cpp => cpp::run(project, &options, out_dir)?,
        Target::Rust => rust::run(project, &options, out_dir)?,
        Target::Python => python::run(project, &options, out_dir)?,
        Target::Sql => sql::run(project, &options, out_dir)?,
    };

    fs::write(out_path, code)?;

    Ok(())
}

pub enum Target {
    Cpp,
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

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{0}")]
    Fmt(#[from] std::fmt::Error),
    #[error("{0}")]
    Io(#[from] std::io::Error),
    #[error("{0}")]
    Compile(#[from] lutra_compiler::Error),
    #[error("{0}")]
    Unsupported(String),
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
            long = "function-traits",
            visible_alias = "no-function-traits",
            default_value_t = false,
            num_args = 0..=1,
            default_missing_value = "true"
        )
    )]
    pub function_traits: bool,

    #[cfg_attr(feature = "clap", arg(long))]
    pub client: bool,

    #[cfg_attr(feature = "clap", arg(long = "no-std", default_value_t = false))]
    pub no_std: bool,

    /// Modules for which programs should be generated, together with their
    /// representation and additional externals.
    #[cfg_attr(feature = "clap", arg(skip))]
    pub programs: HashMap<String, (ProgramRepr, Vec<String>)>,

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
            function_traits: false,
            client: false,
            no_std: false,
            programs: HashMap::new(),
            lutra_bin_path: "::lutra_bin".into(),
            lutra_runner_path: "::lutra_runner".into(),
        }
    }
}

impl GenerateOptions {
    /// Do not generate type definitions
    pub fn no_types(mut self) -> Self {
        self.no_types = true;
        self
    }

    /// Do not generate [lutra_bin::Encode] and [lutra_bin::Decode] implementations
    pub fn no_encode_decode(mut self) -> Self {
        self.no_encode_decode = true;
        self
    }

    /// Generate traits for functions (Rust only).
    pub fn function_traits(mut self) -> Self {
        self.function_traits = true;
        self
    }

    /// Generate module client wrappers for generated programs
    pub fn generate_client(mut self) -> Self {
        self.client = true;
        self
    }

    /// Emit `no_std`-safe, fully-qualified references for `Vec`, `String`, and
    /// `Box` (via the `lutra_bin` re-exports) instead of the prelude names.
    pub fn no_std(mut self) -> Self {
        self.no_std = true;
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

    /// Generates programs for all functions in a module.
    ///
    /// This will compile each function from the module into the target program repr.
    /// It will check that the program uses only externals provided by the repr or
    /// explicitly allowed by `suppoerted_externals`.
    pub fn generate_programs(
        mut self,
        module_path: impl ToString,
        repr: ProgramRepr,
        supported_externals: impl IntoIterator<Item = String>,
    ) -> Self {
        let m = module_path.to_string();
        let e = self.programs.entry(m).or_insert_with(|| (repr, Vec::new()));
        e.0 = repr;
        e.1.extend(supported_externals);
        self
    }

    /// Return compile parameters for a generated function definition.
    ///
    /// `programs` is keyed by module path, while `path` is the fully-qualified
    /// definition path. Resolve the module first so nested modules do not get
    /// confused with their definitions.
    pub(crate) fn get_program_params(&self, path: &str) -> Option<lutra_compiler::CompileParams> {
        let module_path = path.rsplit_once("::").map_or("", |(module, _)| module);
        let (repr, externals) = self.programs.get(module_path)?;
        Some(
            lutra_compiler::CompileParams::new(path, *repr)
                .with_externals(externals.iter().cloned()),
        )
    }

    pub(crate) fn generates_programs_for_module(&self, module_path: &str) -> bool {
        self.programs.contains_key(module_path)
    }

    pub(crate) fn generates_types(&self) -> bool {
        !self.no_types
    }

    pub(crate) fn generates_encode_decode(&self) -> bool {
        !self.no_encode_decode
    }

    pub(crate) fn generates_function_traits(&self) -> bool {
        self.function_traits
    }

    pub(crate) fn generates_client(&self) -> bool {
        self.client
    }

    pub(crate) fn has_programs_in_subtree(&self, module_path: &str) -> bool {
        self.programs.keys().any(|p| {
            p == module_path
                || (!module_path.is_empty()
                    && p.starts_with(module_path)
                    && p[module_path.len()..].starts_with("::"))
                || (module_path.is_empty() && !p.is_empty())
        })
    }

    /// Path to use for `Vec` in generated code.
    pub(crate) fn vec_path(&self) -> Cow<'static, str> {
        if self.no_std {
            Cow::Owned(format!("{}::vec::Vec", self.lutra_bin_path))
        } else {
            Cow::Borrowed("Vec")
        }
    }

    /// Path to use for `String` in generated code.
    pub(crate) fn string_path(&self) -> Cow<'static, str> {
        if self.no_std {
            Cow::Owned(format!("{}::string::String", self.lutra_bin_path))
        } else {
            Cow::Borrowed("String")
        }
    }

    /// Path to use for `Box` in generated code.
    pub(crate) fn box_path(&self) -> Cow<'static, str> {
        if self.no_std {
            Cow::Owned(format!("{}::boxed::Box", self.lutra_bin_path))
        } else {
            Cow::Borrowed("Box")
        }
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
