use clap::{Parser, Subcommand};

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
        Action::RunPostgres(cmd) => run_postgres(cmd),
        Action::PullSchemaPostgres(cmd) => pull_schema_postgres(cmd),
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

    /// Compile the project
    Check(CheckCommand),

    /// Compile the project to bytecode and run it
    Run(RunCommand),

    /// Compile the project to SQL and run it against PostgreSQL
    RunPostgres(RunPostgresCommand),

    /// Pull schema from PostgreSQL
    PullSchemaPostgres(PullSchemaPostgresCommand),

    /// Compile the project and generate bindings code
    Codegen(CodegenCommand),
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
    #[clap(long, default_value = "main")]
    program: String,

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

    let expr = lutra_compiler::check_overlay(&project, &cmd.program, Some("--program"))?;
    let program = lutra_compiler::lower_expr(&project.root_module, &expr);
    let program = lutra_compiler::layouter::on_program(program);

    if cmd.print_ir {
        let program_source = lutra_bin::ir::print(&program);
        println!("------ IR ------");
        println!("{program_source}");
        println!("----------------");
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
    check: CheckParams,

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
        lutra_compiler::ProgramFormat::BytecodeLt,
    )?;

    // execute
    let runner = lutra_interpreter::InterpreterRunner::default();
    let output = runner.execute_raw(&program, &[]).await?;

    // handle output
    let value = lutra_bin::Value::decode(&output, &ty.output, &ty.ty_defs)?;
    println!("{}", value.print_source(&ty.output, &ty.ty_defs).unwrap());
    Ok(())
}

#[derive(clap::Parser)]
pub struct RunPostgresCommand {
    #[clap(flatten)]
    discover: DiscoverParams,

    #[clap(flatten)]
    check: CheckParams,

    #[clap(default_value = "postgresql://localhost:5432")]
    postgres_url: String,

    #[clap(long, default_value = "main")]
    program: String,
}

#[tokio::main(flavor = "current_thread")]
pub async fn run_postgres(cmd: RunPostgresCommand) -> anyhow::Result<()> {
    // compile
    let project = lutra_compiler::discover(cmd.discover)?;

    let project = lutra_compiler::check(project, cmd.check)?;

    let (program, ty) = lutra_compiler::compile(
        &project,
        &cmd.program,
        Some("--program"),
        lutra_compiler::ProgramFormat::SqlPg,
    )?;
    let program = program.as_sql_pg().unwrap();

    // debug: print formatted sql
    if tracing::enabled!(tracing::Level::DEBUG) {
        let options = sqlformat::FormatOptions::default();
        let formatted_sql =
            sqlformat::format(&program.sql, &sqlformat::QueryParams::None, &options);
        tracing::debug!("sql:\n{formatted_sql}");
    }

    // execute
    let mut client = postgres::Client::connect(&cmd.postgres_url, postgres::NoTls)?;
    let output = lutra_runner_postgres::execute(&mut client, program, &[])?;

    // handle output
    let value = lutra_bin::Value::decode(&output, &ty.output, &ty.ty_defs)?;
    println!("{}", value.print_source(&ty.output, &ty.ty_defs)?);
    Ok(())
}

#[derive(clap::Parser)]
pub struct PullSchemaPostgresCommand {
    #[clap(default_value = "postgresql://localhost:5432")]
    postgres_url: String,
}

pub fn pull_schema_postgres(cmd: PullSchemaPostgresCommand) -> anyhow::Result<()> {
    let mut client = postgres::Client::connect(&cmd.postgres_url, postgres::NoTls)?;

    println!("# generated by Lutra CLI from PostgreSQL database schema");

    let tables = lutra_runner_postgres::table_list(&mut client)?;
    for table in tables {
        let table_ty = lutra_runner_postgres::table_get(&mut client, &table)?;

        let ty_name = if let Some(n) = table.strip_suffix("s") {
            n.to_string()
        } else {
            format!("{table}_item")
        };

        println!();
        println!("type {ty_name} = {}", lutra_bin::ir::print_ty(&table_ty));
        println!("let {table}: func (): [{ty_name}]");
    }
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
