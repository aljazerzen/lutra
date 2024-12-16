fn main() {
    use clap::Parser;
    use inner::*;

    env_logger::builder().format_timestamp(None).init();

    let action = Command::parse();

    let res = match action.command {
        Action::Discover(cmd) => discover_and_print(cmd),
        Action::Compile(cmd) => compile_and_print(cmd),
        Action::Run(cmd) => run_and_print(cmd),
    };

    match res {
        Ok(_) => {}
        Err(err) => {
            println!("{err}");
            std::process::exit(1);
        }
    }
}

mod inner {
    use clap::{Parser, Subcommand};
    use lutra_frontend::{pr, CompileParams, DiscoverParams};

    #[derive(Parser)]
    pub struct Command {
        #[clap(subcommand)]
        pub command: Action,
    }

    #[derive(Subcommand)]
    pub enum Action {
        /// Read the project
        Discover(DiscoverCommand),

        /// Compile the project
        Compile(CompileCommand),

        /// Compile the project and run a program
        Run(RunCommand),
    }

    #[derive(clap::Parser)]
    pub struct DiscoverCommand {
        #[clap(flatten)]
        discover: DiscoverParams,
    }

    pub fn discover_and_print(cmd: DiscoverCommand) -> anyhow::Result<()> {
        let project = lutra_frontend::discover(cmd.discover)?;

        println!("{project}");
        Ok(())
    }

    #[derive(clap::Parser)]
    pub struct CompileCommand {
        #[clap(flatten)]
        discover: DiscoverParams,

        #[clap(flatten)]
        compile: CompileParams,
    }

    pub fn compile_and_print(cmd: CompileCommand) -> anyhow::Result<()> {
        let project = lutra_frontend::discover(cmd.discover)?;

        let project = lutra_frontend::compile(project, cmd.compile)?;

        println!("{project:#?}");
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

    pub fn run_and_print(cmd: RunCommand) -> anyhow::Result<()> {
        let project = lutra_frontend::discover(cmd.discover)?;

        let project = lutra_frontend::compile(project, cmd.compile)?;

        let path = pr::Path::new(cmd.path.split("::"));
        let program = lutra_frontend::lower(&project.root_module, &path);
        log::debug!("ir: {}", lutra_ir::print(&program));
        let program = lutra_frontend::bytecode_program(program);

        let res = lutra_runtime::evaluate(&program, vec![], lutra_runtime::BUILTIN_MODULES);
        let value = lutra_bin::Value::decode(&res, &program.output_ty)?;

        println!("{}", value.print_source(&program.output_ty).unwrap());
        Ok(())
    }
}
