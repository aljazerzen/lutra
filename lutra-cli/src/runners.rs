#[derive(clap::Args)]
#[group(required = false, multiple = false)]
pub(crate) struct RunnerParams {
    /// Runner URL. Scheme selects the runner:
    ///   interpreter:// or lt:// - interpreter
    ///   postgres://DSN          - PostgreSQL
    ///   duckdb://PATH           - DuckDB (use duckdb://:memory: for in-memory)
    #[arg(long)]
    runner: Option<String>,

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
    /// Parse `--runner URL` into the individual fields, then apply defaults.
    pub(crate) fn or_default(mut self) -> Self {
        if let Some(url) = self.runner.take() {
            self.apply_runner_url(url).unwrap_or_else(|e| {
                eprintln!("Error: {e}");
                std::process::exit(1);
            });
        }
        if !self.interpreter && self.postgres.is_none() && self.duckdb.is_none() {
            self.duckdb = Some(":memory:".into());
        }
        self
    }

    fn apply_runner_url(&mut self, url: String) -> anyhow::Result<()> {
        // Split on "://"; if absent, treat the whole string as the scheme.
        let (scheme, rest) = url.split_once("://").unwrap_or((&url, ""));
        match scheme {
            "interpreter" | "lt" => {
                self.interpreter = true;
            }
            "postgres" | "postgresql" => {
                // Pass the full original URL so libpq / tokio-postgres can parse it.
                self.postgres = Some(url);
            }
            "duckdb" => {
                let path = if rest.is_empty() { ":memory:" } else { rest };
                self.duckdb = Some(path.to_string());
            }
            _ => {
                anyhow::bail!(
                    "unknown runner scheme {scheme:?}; expected one of: \
                     interpreter, lt, postgres, duckdb"
                );
            }
        }
        Ok(())
    }
}

impl RunnerParams {
    /// Returns the program repr needed for this runner
    pub fn get_program_repr(&self) -> lutra_compiler::ProgramRepr {
        #[allow(clippy::if_same_then_else)]
        if self.interpreter {
            lutra_compiler::ProgramRepr::BytecodeLt
        } else if self.postgres.is_some() {
            lutra_compiler::ProgramRepr::SqlPg
        } else if self.duckdb.is_some() {
            lutra_compiler::ProgramRepr::SqlDuckdb
        } else {
            unreachable!()
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
