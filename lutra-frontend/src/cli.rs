fn main() {
    use clap::Parser;
    use inner::*;

    env_logger::builder().format_timestamp(None).init();

    let action = Command::parse();

    let res = match action.command {
        Action::Discover(cmd) => discover_and_print(cmd),
        Action::Compile(cmd) => compile_and_print(cmd),
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
    use lutra_frontend::{CompileParams, DiscoverParams};

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
    }

    #[derive(clap::Parser)]
    pub struct DiscoverCommand {
        #[clap(flatten)]
        discover: DiscoverParams,
    }

    #[derive(clap::Parser)]
    pub struct CompileCommand {
        #[clap(flatten)]
        discover: DiscoverParams,

        #[clap(flatten)]
        compile: CompileParams,
    }

    pub fn discover_and_print(cmd: DiscoverCommand) -> anyhow::Result<()> {
        let project = lutra_frontend::discover(cmd.discover)?;

        println!("{project}");
        Ok(())
    }

    pub fn compile_and_print(cmd: CompileCommand) -> anyhow::Result<()> {
        let project = lutra_frontend::discover(cmd.discover)?;

        let project = lutra_frontend::compile(project, cmd.compile)?;

        println!("{project:?}");
        Ok(())
    }
}
