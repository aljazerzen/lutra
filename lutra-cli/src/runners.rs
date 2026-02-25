#[derive(clap::Args)]
#[group(required = false, multiple = false)]
pub(crate) struct RunnerParams {
    /// Use interpreter runner
    #[arg(long, short)]
    interpreter: bool,

    /// Use PostgreSQL runner. Requires a postgres:// URL or a libpq-style connection config.
    #[arg(long, name = "DSN")]
    postgres: Option<String>,

    /// Use DuckDB runner. Provide path to database file or ":memory:" for in-memory database.
    #[arg(long, name = "PATH")]
    duckdb: Option<String>,
}

impl RunnerParams {
    /// Returns the program format needed for this runner
    pub fn get_program_format(&self) -> lutra_compiler::ProgramFormat {
        #[allow(clippy::if_same_then_else)]
        if self.interpreter {
            lutra_compiler::ProgramFormat::BytecodeLt
        } else if self.postgres.is_some() {
            lutra_compiler::ProgramFormat::SqlPg
        } else if self.duckdb.is_some() {
            lutra_compiler::ProgramFormat::SqlDuckdb
        } else {
            unreachable!()
        }
    }

    /// Convert from launcher RunnerParams
    pub fn from_launcher(params: lutra_tui::launcher::RunnerParams) -> Self {
        match params {
            lutra_tui::launcher::RunnerParams::Interpreter => Self {
                interpreter: true,
                postgres: None,
                duckdb: None,
            },
            lutra_tui::launcher::RunnerParams::PostgreSQL(dsn) => Self {
                interpreter: false,
                postgres: Some(dsn),
                duckdb: None,
            },
            lutra_tui::launcher::RunnerParams::DuckDB(path) => Self {
                interpreter: false,
                postgres: None,
                duckdb: Some(path),
            },
        }
    }

    /// Convert to launcher RunnerParams
    pub fn into_launcher(self) -> lutra_tui::launcher::RunnerParams {
        if self.interpreter {
            lutra_tui::launcher::RunnerParams::Interpreter
        } else if let Some(dsn) = self.postgres {
            lutra_tui::launcher::RunnerParams::PostgreSQL(dsn)
        } else if let Some(path) = self.duckdb {
            lutra_tui::launcher::RunnerParams::DuckDB(path)
        } else {
            // Default to interpreter if nothing is specified
            lutra_tui::launcher::RunnerParams::Interpreter
        }
    }
}

/// Construct a runner and spawns it on a separate thread.
///
/// This is done only because we want to have a uniform interface that is
/// provided by the [lutra_runner::channel::Client].
/// Ideally, we could return `Box<dyn RunSync>`, but RunSync is not dyn compatible.
pub fn init(
    runner: RunnerParams,
    project: &lutra_compiler::SourceTree,
) -> anyhow::Result<(lutra_runner::channel::Client, std::thread::JoinHandle<()>)> {
    let file_system = Some(project.get_project_dir().to_path_buf());

    Ok(if runner.interpreter {
        let runner = lutra_interpreter::InterpreterRunner::default().with_file_system(file_system);
        let runner = lutra_runner::AsyncRunner::new(runner);
        let (client, server) = lutra_runner::channel::new_pair(runner);

        let handle = std::thread::Builder::new()
            .name("runner-interpreter".to_string())
            .spawn(move || {
                tokio::runtime::Builder::new_current_thread()
                    .build()
                    .expect("failed to create tokio runtime")
                    .block_on(server.listen());
            })?;
        (client, handle)
    } else if let Some(pg_url) = runner.postgres {
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;

        let runner = rt.block_on(async {
            lutra_runner_postgres::RunnerAsync::connect_no_tls(&pg_url).await
        })?;

        let (client, server) = lutra_runner::channel::new_pair(runner);

        let handle = std::thread::Builder::new()
            .name("runner-postgres".to_string())
            .spawn(move || {
                rt.block_on(server.listen());
            })?;
        (client, handle)
    } else if let Some(duckdb_url) = runner.duckdb {
        let runner = lutra_runner_duckdb::Runner::open(&duckdb_url, file_system)?;
        let runner = lutra_runner::AsyncRunner::new(runner);
        let (client, server) = lutra_runner::channel::new_pair(runner);

        let handle = std::thread::Builder::new()
            .name("runner-duckdb".to_string())
            .spawn(move || {
                tokio::runtime::Builder::new_current_thread()
                    .build()
                    .expect("failed to create tokio runtime")
                    .block_on(server.listen());
            })?;
        (client, handle)
    } else {
        unreachable!()
    })
}
