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

    /// Compile the project to SQL
    Sql(SqlCommand),

    /// Compile the project to bytecode and run it
    Run(RunCommand),

    /// Compile the project to SQL and run it against PostgreSQL
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

    /// Lutra program expression to be compiled
    #[clap(long, default_value = "main")]
    program: String,

    /// Prints debug information about compiled project
    #[clap(long, default_value = "false")]
    print_project: bool,

    /// Prints the Intermediate Representation
    #[clap(long, default_value = "false")]
    print_ir: bool,
}

pub fn check(cmd: CheckCommand) -> anyhow::Result<()> {
    let project = lutra_compiler::discover(cmd.discover)?;

    let project = lutra_compiler::compile(project, cmd.compile)?;

    if cmd.print_project {
        println!("------ PROJECT ------");
        println!("{project:#?}");
        println!("---------------------");
    }

    if cmd.print_ir {
        let program = lutra_compiler::_lower_expr(&project, &cmd.program)?;
        let program = lutra_compiler::layouter::on_program(program);

        if cmd.print_project {
            let program_source = lutra_bin::ir::print(&program);
            println!("------ IR ------");
            println!("{program_source}");
            println!("----------------");
        }
    }

    if !cmd.print_project && !cmd.print_ir {
        println!("All good.")
    }

    Ok(())
}

#[derive(clap::Parser)]
pub struct RunCommand {
    #[clap(flatten)]
    discover: DiscoverParams,

    #[clap(flatten)]
    compile: CompileParams,

    #[clap(default_value = "main")]
    main: String,
}

pub fn run(cmd: RunCommand) -> anyhow::Result<()> {
    let project = lutra_compiler::discover(cmd.discover)?;

    let project = lutra_compiler::compile(project, cmd.compile)?;

    let program = lutra_compiler::_lower_expr(&project, &cmd.main)?;
    tracing::debug!("ir:\n{}", lutra_bin::ir::print(&program));
    let bytecode = lutra_compiler::bytecode_program(program.clone());

    let res = lutra_runtime::evaluate(&bytecode, vec![], lutra_runtime::BUILTIN_MODULES);
    let value = lutra_bin::Value::decode(&res, program.get_output_ty(), &program.types)?;

    println!(
        "{}",
        value
            .print_source(program.get_output_ty(), &program.types)
            .unwrap()
    );
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

    let program = lutra_compiler::compile_to_sql(&project, &path);

    println!("{}", program.sql);
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

    let program = lutra_compiler::compile_to_sql(&project, &path);
    tracing::debug!("sql: {}", program.sql);

    let (client, connection) =
        tokio_postgres::connect(&cmd.postgres_url, tokio_postgres::NoTls).await?;

    tokio::spawn(async move {
        if let Err(e) = connection.await {
            eprintln!("connection error: {}", e);
        }
    });

    let data = lutra_db_driver::query(&client, &program, vec![]).await?;

    let value = lutra_bin::Value::decode(&data, &program.output_ty, &program.types)?;

    println!(
        "{}",
        value.print_source(&program.output_ty, &program.types)?
    );
    Ok(())
}
