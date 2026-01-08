mod python;
mod rust;

pub use lutra_compiler::ProgramFormat;

use std::borrow::Cow;
use std::{fs, path};

use lutra_bin::{Encode, ir};
use lutra_compiler::DiscoverParams;

#[track_caller]
pub fn generate(
    project_dir: impl AsRef<path::Path>,
    target: Target,
    out_file: impl AsRef<path::Path>,
    options: GenerateOptions,
) -> Vec<path::PathBuf> {
    // tracing_subscriber::fmt::Subscriber::builder()
    //     .without_time()
    //     .with_target(false)
    //     .with_max_level(tracing::Level::DEBUG)
    //     .with_writer(std::io::stderr)
    //     .init();

    // discover the project
    let source = lutra_compiler::discover(DiscoverParams {
        project: Some(project_dir.as_ref().to_path_buf()),
    })
    .unwrap();

    // compile
    let project =
        lutra_compiler::check(source, Default::default()).unwrap_or_else(|e| panic!("{e}"));

    // generate
    let mut file = fs::File::create(&out_file).unwrap();
    let out_dir = out_file.as_ref().parent().unwrap().to_path_buf();

    let generated = match target {
        Target::Rust => rust::run(&project, &options, out_dir).unwrap(),
        Target::Python => python::run(&project, &options, out_dir).unwrap(),
    };

    std::io::Write::write_all(&mut file, generated.as_bytes()).unwrap();

    // return vec of input files
    project
        .source
        .get_source_display_paths()
        .map(|p| p.to_path_buf())
        .collect()
}

pub enum Target {
    Rust,
    Python,
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
    let (program, _ty) = lutra_compiler::compile(
        &project,
        program,
        None,
        lutra_compiler::ProgramFormat::BytecodeLt,
    )
    .unwrap();

    let buf = program.into_bytecode_lt().unwrap().encode();
    std::fs::write(out_file, buf).unwrap();
}

#[derive(Debug, Clone)]
pub struct GenerateOptions {
    generate_types: bool,
    generate_encode_decode: bool,
    generate_function_traits: bool,

    include_programs: Vec<(String, ProgramFormat)>,

    lutra_bin_path: String,
}

impl Default for GenerateOptions {
    fn default() -> Self {
        Self {
            generate_types: true,
            generate_encode_decode: true,
            generate_function_traits: false,
            include_programs: Vec::new(),
            lutra_bin_path: "::lutra_bin".into(),
        }
    }
}

impl GenerateOptions {
    /// Do not generate type definitions
    pub fn no_generate_types(mut self) -> Self {
        self.generate_types = false;
        self
    }

    /// Do not generate [lutra_bin::Encode] and [lutra_bin::Decode] implementations
    pub fn no_generate_encode_decode(mut self) -> Self {
        self.generate_encode_decode = false;
        self
    }

    /// Do not generate traits for functions
    pub fn generate_function_traits(mut self) -> Self {
        self.generate_function_traits = true;
        self
    }

    /// Set path to [lutra_bin] dependency
    pub fn with_lutra_bin_path(mut self, path: String) -> Self {
        self.lutra_bin_path = path;
        self
    }

    /// Generates programs for all functions in a module
    pub fn generate_programs(mut self, module_path: impl ToString, fmt: ProgramFormat) -> Self {
        self.include_programs.push((module_path.to_string(), fmt));
        self
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
