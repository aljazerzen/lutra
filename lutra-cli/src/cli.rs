use clap::{Parser, Subcommand};

use lutra_compiler::{pr, CompileParams, DiscoverParams};

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
        Action::Compile(cmd) => compile(cmd),
        Action::Run(cmd) => run(cmd),
        Action::Sql(cmd) => sql(cmd),
        Action::RunPostgres(cmd) => run_postgres(cmd),
    };

    match res {
        Ok(_) => {}
        Err(err) => {
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

    /// Compile the project
    Check(CheckCommand),

    /// Compile the project to IR
    Compile(CompileCommand),

    /// Compile the project and run a program
    Run(RunCommand),

    /// Compile the project to SQL
    Sql(SqlCommand),

    /// Compile the project and run against PostgreSQL
    RunPostgres(RunPostgresCommand),
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
    compile: CompileParams,

    #[arg(long = "print", value_enum)]
    print: Option<CheckPrint>,
}

#[derive(clap::ValueEnum, Clone)]
enum CheckPrint {
    Debug,
}

pub fn check(cmd: CheckCommand) -> anyhow::Result<()> {
    let project = lutra_compiler::discover(cmd.discover)?;

    let project = lutra_compiler::compile(project, cmd.compile)?;

    match cmd.print {
        Some(CheckPrint::Debug) => {
            println!("{project:#?}");
        }
        None => {
            println!("All good.")
        }
    }

    Ok(())
}

#[derive(clap::Parser)]
pub struct CompileCommand {
    #[clap(flatten)]
    discover: DiscoverParams,

    #[clap(flatten)]
    compile: CompileParams,

    #[clap(default_value = "main")]
    path: String,
}

pub fn compile(cmd: CompileCommand) -> anyhow::Result<()> {
    let project = lutra_compiler::discover(cmd.discover)?;

    let project = lutra_compiler::compile(project, cmd.compile)?;

    let path = pr::Path::new(cmd.path.split("::"));
    let program = lutra_compiler::lower(&project.root_module, &path);

    let program_source = lutra_ir::print(&program);
    println!("------ IR ------");
    println!("{program_source}");
    println!("----------------");
    Ok(())
}

#[derive(clap::Parser)]
pub struct RunCommand {
    #[clap(flatten)]
    discover: DiscoverParams,

    #[clap(flatten)]
    compile: CompileParams,

    #[clap(default_value = "main")]
    path: String,
}

pub fn run(cmd: RunCommand) -> anyhow::Result<()> {
    let project = lutra_compiler::discover(cmd.discover)?;

    let project = lutra_compiler::compile(project, cmd.compile)?;

    let path = pr::Path::new(cmd.path.split("::"));
    let program = lutra_compiler::lower(&project.root_module, &path);
    tracing::debug!("ir: {}", lutra_ir::print(&program));
    let output_ty = program.get_output_ty().clone();
    let bytecode = lutra_compiler::bytecode_program(program);

    let res = lutra_runtime::evaluate(&bytecode, vec![], lutra_runtime::BUILTIN_MODULES);
    let value = lutra_bin::Value::decode(&res, &output_ty)?;

    println!("{}", value.print_source(&output_ty).unwrap());
    Ok(())
}

#[derive(clap::Parser)]
pub struct SqlCommand {
    #[clap(flatten)]
    discover: DiscoverParams,

    #[clap(flatten)]
    compile: CompileParams,

    #[clap(default_value = "main")]
    path: String,
}

pub fn sql(cmd: SqlCommand) -> anyhow::Result<()> {
    let project = lutra_compiler::discover(cmd.discover)?;

    let project = lutra_compiler::compile(project, cmd.compile)?;

    let path = pr::Path::new(cmd.path.split("::"));
    let program = lutra_compiler::lower(&project.root_module, &path);

    tracing::debug!("ir: {}", lutra_ir::print(&program));
    let sql = lutra_compiler::compile_to_sql(&program);

    println!("{}", sql);
    Ok(())
}

#[derive(clap::Parser)]
pub struct RunPostgresCommand {
    #[clap(flatten)]
    discover: DiscoverParams,

    #[clap(flatten)]
    compile: CompileParams,

    #[clap(default_value = "postgresql://localhost:5432")]
    postgres_url: String,

    #[clap(default_value = "main")]
    path: String,
}

#[tokio::main(flavor = "current_thread")]
pub async fn run_postgres(cmd: RunPostgresCommand) -> anyhow::Result<()> {
    let project = lutra_compiler::discover(cmd.discover)?;

    let project = lutra_compiler::compile(project, cmd.compile)?;

    let path = pr::Path::new(cmd.path.split("::"));
    let program = lutra_compiler::lower(&project.root_module, &path);

    tracing::debug!("ir: {}", lutra_ir::print(&program));
    let sql = lutra_compiler::compile_to_sql(&program);
    tracing::debug!("sql: {sql}");

    let (client, connection) =
        tokio_postgres::connect(&cmd.postgres_url, tokio_postgres::NoTls).await?;

    tokio::spawn(async move {
        if let Err(e) = connection.await {
            eprintln!("connection error: {}", e);
        }
    });

    let (db_ty, data) = lutra_db_driver::query(client, &sql).await?;
    let data = lutra_db_driver::repack(&db_ty, data, program.get_output_ty());
    let data = data.flatten();

    let value = lutra_bin::Value::decode(&data, program.get_output_ty())?;

    println!("{}", value.print_source(program.get_output_ty())?);
    Ok(())
}
