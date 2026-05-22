mod io;
mod language_server;
mod runners;

use std::collections::HashMap;
use std::path;

use clap::{Parser, Subcommand};

use lutra_bin::Encode;
use lutra_compiler::{CheckParams, CompileParams, DiscoverParams, codespan};
use lutra_runner::RunSync;

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
        Action::GenCode(cmd) => gen_code(cmd),
        Action::GenDocs(cmd) => gen_docs(cmd),
        Action::Format(cmd) => format(cmd),
        Action::Serve(cmd) => serve(cmd),
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
    /// Interactive REPL shell
    #[clap(alias = "i")]
    Interactive(InteractiveCommand),

    /// Read a project
    Discover(DiscoverCommand),

    /// Validate a project
    Check(CheckCommand),

    /// Compile a program
    Compile(CompileCommand),

    /// Compile a program and run it
    Run(RunCommand),

    /// Pull runner interface into a project
    Pull(PullSchemaCommand),

    /// Format source files
    #[clap(alias = "fmt")]
    Format(FormatCommand),

    /// Generate bindings code from a project
    GenCode(GenCodeCommand),

    /// Generate Markdown reference from a project
    GenDocs(GenDocsCommand),

    /// Start language server (LSP)
    LanguageServer(language_server::Command),

    /// Serve a runner proxy over TCP binary protocol
    Serve(ServeCommand),
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

    #[clap(flatten)]
    compile: CompileParams,

    /// Write compiled program to a `.rr.ld` file.
    /// Relative to project root.
    #[clap(long)]
    output: Option<path::PathBuf>,
}

pub fn compile(cmd: CompileCommand) -> anyhow::Result<()> {
    // compile
    let project = lutra_compiler::discover(cmd.discover)?;

    let project = lutra_compiler::check(project, cmd.check)?;

    let (program, ty) =
        lutra_compiler::compile(&project, &cmd.compile.with_program_name_hint("--program"))?;

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
    runner: runners::RunnerParams,

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

    // check
    eprintln!("Compiling...");
    let project = lutra_compiler::discover(cmd.discover.clone())?;

    let project = lutra_compiler::check(project, cmd.check)?;
    let to_project_path = |p: &str| project.source.get_absolute_path(p);

    // init runner
    let runner = cmd.runner.into_spec(project.get_runner())?;
    let mut runner = runners::AnyRunnerSync::connect(&runner)?;

    // compile
    let params =
        lutra_compiler::CompileParams::from_externals(&cmd.program, &runner.get_externals_sync()?)
            .map_err(|e| anyhow::anyhow!("{e}"))?
            .with_program_name_hint("--program");
    let (program, ty) = lutra_compiler::compile(&project, &params)?;

    // read input
    let input_path = cmd.input.as_deref().map(to_project_path);
    let input = io::read_input(input_path, cmd.input_format, &ty)?;

    // execute
    eprintln!("Executing...");

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
    runner: runners::RunnerParams,
}

pub fn interactive(cmd: InteractiveCommand) -> anyhow::Result<()> {
    // check (needed for runner & project dir)
    let source = lutra_compiler::discover(cmd.discover.clone())?;
    let project = lutra_compiler::check(source.clone(), Default::default())?;

    // init runner
    let runner_params = cmd.runner.into_spec(project.get_runner())?;
    let (mut runner, _) = runners::spawn(runner_params)?;

    // derive repr from runner externals
    let externals = runner.get_externals_sync()?;
    let repr = lutra_compiler::ProgramRepr::from_externals(&externals)
        .ok_or_else(|| anyhow::anyhow!("runner did not provide a repr: tag in get_externals()"))?;

    // open interactive TUI
    let project_path =
        (cmd.discover.project.is_some()).then_some(project.source.get_root().to_path_buf());
    lutra_tui::run_shell(project_path, repr, runner)
}

#[derive(clap::Parser)]
pub struct PullSchemaCommand {
    #[clap(flatten)]
    discover: DiscoverParams,

    #[clap(flatten)]
    check: CheckParams,

    #[clap(flatten)]
    runner: runners::RunnerParams,
}

pub fn pull_schema(cmd: PullSchemaCommand) -> anyhow::Result<()> {
    // discover and check
    let source_tree = lutra_compiler::discover(cmd.discover.clone())?;
    let project = lutra_compiler::check(source_tree, cmd.check)?;

    // pull
    let spec = cmd.runner.into_spec(project.get_runner())?;
    let mut runner = runners::AnyRunnerSync::connect(&spec)?;
    let schema = runner.pull_schema_sync()?;

    // try update the @schema module
    if let Some(path) = lutra_project_tools::update_schema(&project, &schema)? {
        let display = project.source.get_display_path(&path).unwrap();
        eprintln!("Written: {}", display.display());
    }

    // fallback: print the schema
    println!(
        "# Generated by Lutra CLI{}\n",
        std::env::var("CARGO_PKG_VERSION")
            .map(|v| format!(" {v}"))
            .unwrap_or_default()
    );
    println!("{schema}");

    Ok(())
}

#[derive(clap::Parser)]
pub struct GenCodeCommand {
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
    client: bool,

    #[arg(long)]
    programs_bytecode_lt: Vec<String>,
    #[arg(long)]
    programs_sql_pg: Vec<String>,

    #[arg(long)]
    lutra_bin_path: Option<String>,
}

pub fn gen_code(cmd: GenCodeCommand) -> anyhow::Result<()> {
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
    if cmd.client {
        opts = opts.generate_client();
    }
    for mod_name in cmd.programs_bytecode_lt {
        opts = opts.generate_programs(mod_name, lutra_codegen::ProgramRepr::BytecodeLt);
    }
    for mod_name in cmd.programs_sql_pg {
        opts = opts.generate_programs(mod_name, lutra_codegen::ProgramRepr::SqlPg);
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
pub struct GenDocsCommand {
    /// Path to a file in a Lutra project.
    #[clap(flatten)]
    discover: DiscoverParams,

    #[clap(flatten)]
    check: CheckParams,

    /// Output directory for generated Markdown pages.
    output_dir: std::path::PathBuf,
}

pub fn gen_docs(cmd: GenDocsCommand) -> anyhow::Result<()> {
    let project = if cmd.discover.project.as_ref().is_some_and(|p| p == ":std:") {
        let source = lutra_compiler::std_source();
        let mut params = cmd.check;
        params.no_std = true;
        lutra_compiler::check(source, params)?
    } else {
        let source_tree = lutra_compiler::discover(cmd.discover.clone())?;
        lutra_compiler::check(source_tree, cmd.check)?
    };

    use lutra_project_tools::docs;
    let pages = docs::generate_md_pages(&project)?;
    docs::write_md_pages(&cmd.output_dir, &pages)?;

    eprintln!(
        "Written {} page(s) to {}",
        pages.len(),
        cmd.output_dir.display()
    );
    Ok(())
}

#[derive(clap::Parser)]
pub struct ServeCommand {
    /// Address to bind the TCP server on.
    #[arg(long, default_value = "127.0.0.1:7373")]
    at: String,

    #[clap(flatten)]
    runner: runners::RunnerParams,
}

#[tokio::main(flavor = "current_thread")]
pub async fn serve(cmd: ServeCommand) -> anyhow::Result<()> {
    let spec = cmd.runner.into_spec(None)?;
    let runner = runners::AnyRunner::connect(&spec).await?;

    let listener = tokio::net::TcpListener::bind(&cmd.at).await?;
    eprintln!("Listening on {}", cmd.at);

    loop {
        let (stream, peer) = listener.accept().await?;
        eprintln!("Connection from {peer}");

        let mut server = lutra_runner::binary::tokio::Server::new(stream, &runner);
        if let Err(e) = server.run().await {
            eprintln!("Connection from {peer} error: {e}");
        }
        eprintln!("Connection from {peer} closed");
    }
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
