use clap::{Args, Parser, Subcommand};

use lutra_compiler::{CheckParams, DiscoverParams};
use lutra_runner::Run;

fn main() {
    let action = Command::parse();

    if action.verbose {
        tracing_subscriber::fmt::Subscriber::builder()
            .without_time()
            .with_max_level(tracing::Level::DEBUG)
            .with_writer(std::io::stderr)
            .init();
    }

    let res = match action.command {
        Action::Discover(cmd) => discover(cmd),
        Action::Check(cmd) => check(cmd),
        Action::Run(cmd) => run(cmd),
        Action::Pull(cmd) => pull_interface(cmd),
        Action::Codegen(cmd) => codegen(cmd),
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

    /// Compile a program and run it
    Run(RunCommand),

    /// Pull interface from the runner
    Pull(PullSchemaPostgresCommand),

    /// Compile the project and generate bindings code
    Codegen(CodegenCommand),
}

#[derive(Args)]
#[group(required = true, multiple = false)]
struct RunnerParams {
    /// Use interpreter runner
    #[arg(long, short)]
    interpreter: bool,

    /// Use PostgreSQL runner. Requires a postgres:// URL.
    #[arg(long)]
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
        let program = lutra_compiler::lower_expr(&project.root_module, &expr);
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
pub struct RunCommand {
    #[clap(flatten)]
    discover: DiscoverParams,

    #[clap(flatten)]
    check: CheckParams,

    #[clap(flatten)]
    runner: RunnerParams,

    #[clap(long, default_value = "main")]
    program: String,
}

#[tokio::main(flavor = "current_thread")]
pub async fn run(cmd: RunCommand) -> anyhow::Result<()> {
    // compile
    let project = lutra_compiler::discover(cmd.discover)?;

    let project = lutra_compiler::check(project, cmd.check)?;

    let (program, ty) = lutra_compiler::compile(
        &project,
        &cmd.program,
        Some("--program"),
        cmd.runner.get_program_format(),
    )?;

    // execute
    let output = if cmd.runner.interpreter {
        let runner = lutra_interpreter::InterpreterRunner::default();
        std::env::set_current_dir(project.source.root)?;

        let handle = runner.prepare(program).await?;
        runner.execute(&handle, &[]).await?
    } else if let Some(pg_url) = cmd.runner.postgres {
        let runner = init_runner_postgres(&pg_url).await?;
        let handle = runner.prepare(program).await?;
        runner.execute(&handle, &[]).await?
    } else {
        unreachable!()
    };

    // handle output
    let value = lutra_bin::Value::decode(&output, &ty.output, &ty.ty_defs)?;
    println!("{}", value.print_source(&ty.output, &ty.ty_defs).unwrap());
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
    sr_modules: Vec<String>,

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
        opts = opts.no_generate_function_traits();
    }
    for mod_name in cmd.sr_modules {
        opts = opts.generate_sr_in_module(mod_name);
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
