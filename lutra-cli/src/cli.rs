mod language_server;

use std::collections::HashMap;
use std::path;

use clap::{Args, Parser, Subcommand};

use lutra_bin::{Encode, typed_data};
use lutra_codegen::ProgramFormat;
use lutra_compiler::{CheckParams, DiscoverParams, codespan};
use lutra_runner::Run;

use tokio::fs;
use tokio::io::{self, AsyncWriteExt};

fn main() {
    let action = Command::parse();

    if action.verbose {
        tracing_subscriber::fmt::Subscriber::builder()
            .without_time()
            .with_target(false)
            .with_max_level(tracing::Level::DEBUG)
            .with_writer(std::io::stderr)
            .init();
    }

    let res = match action.command {
        Action::Discover(cmd) => discover(cmd),
        Action::Check(cmd) => check(cmd),
        Action::Compile(cmd) => compile(cmd),
        Action::Run(cmd) => run(cmd),
        Action::Pull(cmd) => pull_interface(cmd),
        Action::Codegen(cmd) => codegen(cmd),
        Action::Format(cmd) => format(cmd),
        Action::LanguageServer(cmd) => {
            language_server::run(cmd);
            Ok(())
        }
    };

    match res {
        Ok(_) => {}
        Err(err) => {
            println!("[Error]:");
            println!("{err}");
            std::process::exit(1);
        }
    }
}

#[derive(Parser)]
pub struct Command {
    #[clap(short, long)]
    pub verbose: bool,

    #[clap(subcommand)]
    pub command: Action,
}

#[derive(Subcommand)]
pub enum Action {
    /// Read the project
    Discover(DiscoverCommand),

    /// Validate the project
    Check(CheckCommand),

    /// Compile a program
    Compile(CompileCommand),

    /// Compile a program and run it
    Run(RunCommand),

    /// Pull interface from the runner
    Pull(PullSchemaPostgresCommand),

    /// Compile the project and generate bindings code
    Codegen(CodegenCommand),

    /// Format source files
    #[clap(alias = "fmt")]
    Format(FormatCommand),

    /// Start language server (LSP)
    LanguageServer(language_server::Command),
}

#[derive(Args)]
#[group(required = true, multiple = false)]
struct RunnerParams {
    /// Use interpreter runner
    #[arg(long, short)]
    interpreter: bool,

    /// Use PostgreSQL runner. Requires a postgres:// URL or a libpq-style connection config.
    #[arg(long, name = "DSN")]
    postgres: Option<String>,
}

impl RunnerParams {
    /// Returns the program format needed for this runner
    fn get_program_format(&self) -> lutra_compiler::ProgramFormat {
        if self.interpreter {
            lutra_compiler::ProgramFormat::BytecodeLt
        } else if self.postgres.is_some() {
            lutra_compiler::ProgramFormat::SqlPg
        } else {
            unreachable!()
        }
    }
}

#[derive(clap::Parser)]
pub struct DiscoverCommand {
    #[clap(flatten)]
    discover: DiscoverParams,
}

pub fn discover(cmd: DiscoverCommand) -> anyhow::Result<()> {
    let project = lutra_compiler::discover(cmd.discover)?;

    println!("{project}");
    Ok(())
}

#[derive(clap::Parser)]
pub struct CheckCommand {
    #[clap(flatten)]
    discover: DiscoverParams,

    #[clap(flatten)]
    check: CheckParams,

    /// Prints debug information about compiled project
    #[clap(long, default_value = "false")]
    print_project: bool,

    /// Lutra program expression to be compiled
    #[clap(long)]
    program: Option<String>,

    /// Prints the Intermediate Representation
    #[clap(long, default_value = "false")]
    print_ir: bool,
}

pub fn check(cmd: CheckCommand) -> anyhow::Result<()> {
    let project = lutra_compiler::discover(cmd.discover)?;

    let project = lutra_compiler::check(project, cmd.check)?;

    if cmd.print_project {
        println!("------ PROJECT ------");
        println!("{project:#?}");
        println!("---------------------");
    }

    if let Some(program) = &cmd.program {
        let expr = lutra_compiler::check_overlay(&project, program, Some("--program"))?;
        let program = lutra_compiler::lower_expr(&project, &expr);
        let program = lutra_compiler::inline(program);
        let program = lutra_compiler::layouter::on_program(program);

        if cmd.print_ir {
            let program_source = lutra_bin::ir::print(&program);
            println!("------ IR ------");
            println!("{program_source}");
            println!("----------------");
        }
    }

    if !cmd.print_project {
        println!("All good.")
    }

    Ok(())
}

#[derive(clap::Parser)]
pub struct CompileCommand {
    #[clap(flatten)]
    discover: DiscoverParams,

    #[clap(flatten)]
    check: CheckParams,

    /// Program to execute.
    ///
    /// Usually this is a path to an expression in the project, but it can be
    /// any lutra expression.
    ///
    /// When --input is supplied, the program is wrapped into `func (input) -> ...`.
    #[clap(long, default_value = "main")]
    program: String,

    #[clap(long, default_value = "bytecode-lt")]
    format: lutra_compiler::ProgramFormat,
}

pub fn compile(cmd: CompileCommand) -> anyhow::Result<()> {
    let project_dir = cmd.discover.project.clone();

    // compile
    let project = lutra_compiler::discover(cmd.discover)?;

    let project = lutra_compiler::check(project, cmd.check)?;

    let (program, ty) =
        lutra_compiler::compile(&project, &cmd.program, Some("--program"), cmd.format)?;

    println!("Compiled.");

    println!();
    println!("Input:  {}", lutra_bin::ir::print_ty(&ty.input));
    println!("Output: {}", lutra_bin::ir::print_ty(&ty.output));
    if !ty.defs.is_empty() {
        println!("Type definitions:");
        for def in &ty.defs {
            println!(
                "  {}: {}",
                def.name.0.join("::"),
                lutra_bin::ir::print_ty(&def.ty)
            );
        }
    }

    let project_dir = project_dir.unwrap_or_else(|| std::env::current_dir().unwrap());
    let out_dir = project_dir.join("program.rr.lb");

    let program_lt = program.encode();
    std::fs::write(&out_dir, &program_lt)?;

    println!();
    println!(
        "Program written to {} ({} bytes)",
        out_dir.display(),
        program_lt.len()
    );
    Ok(())
}

#[derive(clap::Parser)]
pub struct RunCommand {
    #[clap(flatten)]
    discover: DiscoverParams,

    #[clap(flatten)]
    check: CheckParams,

    #[clap(flatten)]
    runner: RunnerParams,

    /// Read input from `.lb` file.
    ///
    /// When supplied, the program is wrapped into `func (input) -> ...`.
    #[clap(long)]
    input: Option<String>,

    /// Program to execute.
    ///
    /// Usually this is a path to an expression in the project, but it can be
    /// any lutra expression.
    ///
    /// When --input is supplied, the program is wrapped into `func (input) -> ...`.
    #[clap(long, default_value = "main")]
    program: String,

    /// Write output to `.lb` file.
    ///
    /// When omitted, output is written to stdout in Lutra source format.
    #[clap(long)]
    output: Option<String>,

    /// Output file format
    #[clap(long, default_value = "ld")]
    output_format: FileFormat,
}

#[derive(clap::ValueEnum, Clone, strum::AsRefStr)]
enum FileFormat {
    /// Lutra data (binary)
    Ld,
    /// Lutra typed data (binary)
    Ltd,
}

#[tokio::main(flavor = "current_thread")]
pub async fn run(cmd: RunCommand) -> anyhow::Result<()> {
    // compile
    println!("Compiling...");
    let project = lutra_compiler::discover(cmd.discover.clone())?;

    let project = lutra_compiler::check(project, cmd.check)?;

    let mut program = cmd.program;
    if cmd.input.is_some() {
        program = format!("func (input) -> {program}");
    }

    let (program, ty) = lutra_compiler::compile(
        &project,
        &program,
        Some("--program"),
        cmd.runner.get_program_format(),
    )?;

    // read input
    println!("Reading input...");
    let input = if let Some(input_file) = cmd.input {
        let input_file = project.source.root.as_path().join(input_file);
        fs::read(input_file).await?
    } else {
        if !ty.input.is_unit() {
            return Err(anyhow::anyhow!(
                "Missing --input. Expected type {}",
                lutra_bin::ir::print_ty(&ty.input)
            ));
        }

        Vec::new()
    };

    // execute
    println!("Executing...");
    let output = if cmd.runner.interpreter {
        let runner =
            lutra_interpreter::InterpreterRunner::default().with_file_system(cmd.discover.project);

        let handle = runner.prepare(program).await?;
        runner.execute(&handle, &input).await?
    } else if let Some(pg_url) = cmd.runner.postgres {
        let runner = init_runner_postgres(&pg_url).await?;
        let handle = runner.prepare(program).await?;
        runner.execute(&handle, &input).await?
    } else {
        unreachable!()
    };

    // handle output
    if let Some(output_file) = cmd.output {
        let output_file = project.source.root.as_path().join(output_file);
        let format = cmd.output_format;

        // write to file
        let mut writer = io::BufWriter::new(fs::File::create(&output_file).await?);

        let size: usize;
        match format {
            FileFormat::Ld => {
                writer.write_all(&output).await?;
                size = output.len();
            }
            FileFormat::Ltd => {
                let mut buf = lutra_bin::bytes::BytesMut::new();
                typed_data::encode(&mut buf, &output, &ty.output, &ty.defs)?;
                writer.write_all(&buf).await?;
                size = buf.len();
            }
        }

        writer.write_all(&output).await?;

        writer.flush().await?;
        println!(
            "Output written to {} ({}, {size} bytes)",
            output_file.display(),
            format.as_ref(),
        );
    } else {
        // print to stdout
        println!(
            "{}",
            lutra_bin::print_source(&output, &ty.output, &ty.defs).unwrap()
        );
    }

    Ok(())
}

async fn init_runner_postgres(url: &str) -> anyhow::Result<lutra_runner_postgres::RunnerAsync> {
    use tokio_postgres::{NoTls, connect};

    let (client, connection) = connect(url, NoTls).await.unwrap();

    tokio::spawn(async move {
        if let Err(e) = connection.await {
            eprintln!("connection error: {e}");
        }
    });

    Ok(lutra_runner_postgres::RunnerAsync::new(client))
}

#[derive(clap::Parser)]
pub struct PullSchemaPostgresCommand {
    #[clap(flatten)]
    runner: RunnerParams,
}

#[tokio::main(flavor = "current_thread")]
pub async fn pull_interface(cmd: PullSchemaPostgresCommand) -> anyhow::Result<()> {
    let interface = if cmd.runner.interpreter {
        let runner = lutra_interpreter::InterpreterRunner::default();
        runner.get_interface().await?
    } else if let Some(pg_url) = cmd.runner.postgres {
        let runner = init_runner_postgres(&pg_url).await?;
        runner.get_interface().await?
    } else {
        unreachable!()
    };

    println!(
        "# Generated by Lutra CLI {}\n",
        std::env::var("CARGO_PKG_VERSION").unwrap_or_default()
    );
    println!("{interface}");
    Ok(())
}

#[derive(clap::Parser)]
pub struct CodegenCommand {
    project_dir: std::path::PathBuf,
    output_file: std::path::PathBuf,

    #[arg(long)]
    no_types: bool,

    #[arg(long)]
    no_encode_decode: bool,

    #[arg(long)]
    no_function_traits: bool,

    #[arg(long)]
    programs_bytecode_lt: Vec<String>,
    #[arg(long)]
    programs_sql_pg: Vec<String>,

    #[arg(long)]
    lutra_bin_path: Option<String>,
}

pub fn codegen(cmd: CodegenCommand) -> anyhow::Result<()> {
    let mut opts = lutra_codegen::GenerateOptions::default();

    if cmd.no_types {
        opts = opts.no_generate_types();
    }
    if cmd.no_encode_decode {
        opts = opts.no_generate_encode_decode();
    }
    if cmd.no_function_traits {
        opts = opts.generate_function_traits();
    }
    for mod_name in cmd.programs_bytecode_lt {
        opts = opts.generate_programs(mod_name, ProgramFormat::BytecodeLt);
    }
    for mod_name in cmd.programs_sql_pg {
        opts = opts.generate_programs(mod_name, ProgramFormat::SqlPg);
    }
    if let Some(lutra_bin_path) = cmd.lutra_bin_path {
        opts = opts.with_lutra_bin_path(lutra_bin_path);
    }

    let input_files = if cmd.output_file.extension().is_some_and(|x| x == "py") {
        lutra_codegen::generate_python(&cmd.project_dir, &cmd.output_file, opts)
    } else {
        lutra_codegen::generate(&cmd.project_dir, &cmd.output_file, opts)
    };

    println!("Used files:");
    for input_file in input_files {
        println!("- {}", input_file.display());
    }
    println!("Output written to {}", cmd.output_file.display());
    println!("Done.");

    Ok(())
}

#[derive(clap::Parser)]
pub struct FormatCommand {
    #[clap(flatten)]
    discover: lutra_compiler::DiscoverParams,
}

pub fn format(cmd: FormatCommand) -> anyhow::Result<()> {
    let source_tree = lutra_compiler::discover(cmd.discover)?;

    let (err, edits) = lutra_compiler::format(&source_tree);

    if let Some(err) = err {
        println!("[Error]:");
        println!("{err}");
    }

    let mut edits_by_source: HashMap<&path::Path, Vec<_>> = HashMap::new();
    for edit in edits {
        let Some(path) = source_tree.get_path(edit.span.source_id) else {
            continue;
        };
        let vec = edits_by_source.entry(path).or_default();
        vec.push(edit);
    }

    for (path, content) in source_tree.get_sources() {
        println!("-- {} --", path.display());

        if let Some(edits) = edits_by_source.get(path.as_path()) {
            let formatted = codespan::apply_text_edits(content, edits);
            println!("{formatted}");
        } else {
            println!("  <unchanged>");
        }
    }
    Ok(())
}
