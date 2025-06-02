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
        Action::PullSchemaPostgres(cmd) => pull_schema_postgres(cmd),
        Action::Codegen(cmd) => codegen(cmd),
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

pub fn run_postgres(cmd: RunPostgresCommand) -> anyhow::Result<()> {
    let project = lutra_compiler::discover(cmd.discover)?;

    let project = lutra_compiler::compile(project, cmd.compile)?;

    let path = pr::Path::new(cmd.path.split("::"));

    let program = lutra_compiler::compile_to_sql(&project, &path);
    tracing::debug!("sql: {}", program.sql);

    let mut client = postgres::Client::connect(&cmd.postgres_url, postgres::NoTls)?;

    let data = lutra_db_driver::query_sync(&mut client, &program, &[])?;

    let value = lutra_bin::Value::decode(&data, &program.output_ty, &program.types)?;
    println!(
        "{}",
        value.print_source(&program.output_ty, &program.types)?
    );
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

    let tables = lutra_db_driver::table_list(&mut client)?;
    for table in tables {
        let table_ty = lutra_db_driver::table_get(&mut client, &table)?;

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
