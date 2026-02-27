mod io;
mod language_server;
mod runners;
mod schema;

use std::collections::HashMap;
use std::path;

use clap::{Parser, Subcommand};

use lutra_bin::Encode;
use lutra_codegen::ProgramFormat;
use lutra_compiler::{CheckParams, DiscoverParams, codespan};
use lutra_runner::RunSync;

use crate::runners::RunnerParams;

fn main() {
    let action = Command::parse();

    if action.verbose {
        tracing_subscriber::fmt::Subscriber::builder()
            .without_time()
            .with_target(false)
            .with_ansi(true)
            .with_max_level(tracing::Level::DEBUG)
            .with_writer(std::io::stderr)
            .init();
    }

    let res = match action.command {
        Action::Discover(cmd) => discover(cmd),
        Action::Check(cmd) => check(cmd),
        Action::Compile(cmd) => compile(cmd),
        Action::Run(cmd) => run(cmd),
        Action::Interactive(cmd) => interactive(cmd),
        Action::Pull(cmd) => pull_schema(cmd),
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

    /// Interactive project environment with live recompilation
    Interactive(InteractiveCommand),

    /// Pull interface from the runner
    Pull(PullSchemaCommand),

    /// Compile the project and generate bindings code
    Codegen(CodegenCommand),

    /// Format source files
    #[clap(alias = "fmt")]
    Format(FormatCommand),

    /// Start language server (LSP)
    LanguageServer(language_server::Command),
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
        lutra_compiler::check_overlay(&project, program, Some("--program"))?;
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
    #[clap(long, default_value = "main")]
    program: String,

    /// Write compiled program to a `.rr.ld` file.
    /// Relative to project root.
    #[clap(long)]
    output: Option<path::PathBuf>,

    #[clap(long, default_value = "bytecode-lt")]
    format: lutra_compiler::ProgramFormat,
}

pub fn compile(cmd: CompileCommand) -> anyhow::Result<()> {
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

    if let Some(out) = &cmd.output {
        let out_file = project.source.get_project_dir().join(out);

        let program_lt = program.encode();
        println!();
        println!(
            "Writing program to {} ({} bytes)",
            out_file.display(),
            program_lt.len()
        );
        std::fs::write(&out_file, &program_lt)?;
    }
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
    #[clap(long)]
    input: Option<String>,

    /// Output file format.
    /// If omitted, it is inferred from input file extension.
    /// Falls back to `lb`.
    #[clap(long)]
    input_format: Option<DataFormat>,

    /// Program to execute.
    ///
    /// Usually this is a path to an expression in the project, but it can be
    /// any lutra expression.
    #[clap(long, default_value = "main")]
    program: String,

    /// Write output to a file.
    ///
    /// When omitted, output is written to stdout in Lutra source format.
    #[clap(long)]
    output: Option<String>,

    /// Output file format.
    /// If omitted, it is inferred from output file extension.
    /// Falls back to `lt`.
    #[clap(long)]
    output_format: Option<DataFormat>,
}

#[derive(Clone, Copy, clap::ValueEnum, strum::AsRefStr, strum::EnumString)]
#[strum(serialize_all = "snake_case")]
enum DataFormat {
    /// Lutra source
    Lt,
    /// Lutra data (binary)
    Ld,
    /// Lutra typed data (binary)
    Ltd,
    /// Comma Separated Values
    Csv,
    /// Parquet
    Parquet,
    /// ASCII table
    Table,
}

pub fn run(cmd: RunCommand) -> anyhow::Result<()> {
    use lutra_runner::RunSync;

    // compile
    eprintln!("Compiling...");
    let project = lutra_compiler::discover(cmd.discover.clone())?;

    let project = lutra_compiler::check(project, cmd.check)?;
    let to_project_path = |p: &str| project.source.get_absolute_path(p);

    let (program, ty) = lutra_compiler::compile(
        &project,
        &cmd.program,
        Some("--program"),
        cmd.runner.get_program_format(),
    )?;

    // read input
    let input_path = cmd.input.as_deref().map(to_project_path);
    let input = io::read_input(input_path, cmd.input_format, &ty)?;

    // execute
    eprintln!("Executing...");

    let (mut runner, _) = crate::runners::init(cmd.runner, &project.source)?;

    let handle = runner.prepare_sync(program)?;
    let output = runner.execute_sync(handle, &input)?;

    // handle output
    let output_path = cmd.output.as_deref().map(to_project_path);
    io::write_output(&output, output_path, cmd.output_format, &ty)?;

    Ok(())
}

#[derive(clap::Parser)]
pub struct InteractiveCommand {
    #[clap(flatten)]
    discover: DiscoverParams,

    #[clap(flatten)]
    runner: Option<RunnerParams>,
}

pub fn interactive(cmd: InteractiveCommand) -> anyhow::Result<()> {
    // open launcher TUI, if needed
    let (project, runner_params) = if cmd.discover.project.is_none() || cmd.runner.is_none() {
        let runner = cmd.runner.map(RunnerParams::into_launcher);
        let result = lutra_tui::launcher::run(cmd.discover.project, runner)?;

        let Some(result) = result else { return Ok(()) };

        let runner = RunnerParams::from_launcher(result.runner_params);
        (result.project_path, runner)
    } else {
        (cmd.discover.project.unwrap(), cmd.runner.unwrap())
    };

    // runner cfg
    let cfg = lutra_tui::RunnerConfig {
        format: runner_params.get_program_format(),
    };

    // init runner
    let source = lutra_compiler::discover(DiscoverParams {
        project: Some(project.clone()),
    })?;
    let (runner, runner_thread) = crate::runners::init(runner_params, &source)?;

    // open interactive TUI
    lutra_tui::run_interactive(project, cfg, runner, runner_thread)
}

#[derive(clap::Parser)]
pub struct PullSchemaCommand {
    #[clap(flatten)]
    discover: DiscoverParams,

    #[clap(flatten)]
    check: CheckParams,

    #[clap(flatten)]
    runner: RunnerParams,
}

pub fn pull_schema(cmd: PullSchemaCommand) -> anyhow::Result<()> {
    // discover and check
    let source_tree = lutra_compiler::discover(cmd.discover.clone())?;
    let project = lutra_compiler::check(source_tree, cmd.check)?;

    // pull
    let (mut runner, _) = crate::runners::init(cmd.runner, &project.source)?;
    let schema = runner.pull_schema_sync()?;

    // find @schema annotation
    let target = crate::schema::find_schema_module_def(&project).map_err(anyhow::Error::msg)?;

    match target {
        None => {
            // No @schema annotation found: print to stdout.
            println!(
                "# Generated by Lutra CLI{}\n",
                std::env::var("CARGO_PKG_VERSION")
                    .map(|v| format!(" {v}"))
                    .unwrap_or_default()
            );
            println!("{schema}");
        }

        Some((_def_path, def)) => {
            // Rewrite a project file

            let rel_path = schema::rewrite_module_contents(&project, def, schema)?;

            let display = project.source.get_display_path(rel_path).unwrap();
            eprintln!("Written: {}", display.display());
        }
    }

    Ok(())
}

#[derive(clap::Parser)]
pub struct CodegenCommand {
    #[clap(flatten)]
    discover: lutra_compiler::DiscoverParams,

    #[clap(flatten)]
    check: CheckParams,

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
    if cmd.discover.project.is_none() {
        return Err(anyhow::anyhow!("--project is required for codegen"));
    }

    let project = lutra_compiler::discover(cmd.discover.clone())?;
    let project = lutra_compiler::check(project, cmd.check)?;

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

    let out_ext = cmd.output_file.extension();
    let target = if out_ext.is_some_and(|x| x == "py") {
        lutra_codegen::Target::Python
    } else if out_ext.is_some_and(|e| e == "sql") {
        lutra_codegen::Target::Sql
    } else {
        lutra_codegen::Target::Rust
    };

    lutra_codegen::generate(&project, target, &cmd.output_file, opts)?;

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
        let Some((path, _content)) = source_tree.get_by_id(edit.span.source_id) else {
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
