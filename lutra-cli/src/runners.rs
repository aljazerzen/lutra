use std::{path, str::FromStr};

use lutra_bin::{rr, string, vec};
use lutra_runner::proto;

#[derive(clap::Args)]
pub(crate) struct RunnerParams {
    /// Runner URL.
    ///
    /// Supported schemes include duckdb:PATH, postgres://DSN, interpreter:PATH
    /// or lt:PATH, tcp://HOST:PORT.
    #[arg(long, short)]
    runner: Option<String>,
}

impl RunnerParams {
    pub(crate) fn into_spec(self, default: Option<&str>) -> anyhow::Result<Spec> {
        let url = self
            .runner
            .or_else(|| default.map(|x| x.to_string()))
            .ok_or(anyhow::anyhow!("Missing --runner or @!runner"))?;
        let (scheme, rest) = split_scheme(url);

        if let Some(path) = (scheme == "interpreter" || scheme == "lt").then_some(rest.as_str()) {
            let file_system = (!path.is_empty())
                .then(|| path::PathBuf::from_str(path))
                .transpose()?;
            return Ok(Spec::Interpreter { file_system });
        } else if ["postgres", "pg", "postgresql"].contains(&scheme.as_str()) {
            return Ok(Spec::Postgres {
                url: format!("postgresql:{}", rest),
            });
        } else if scheme == "duckdb" {
            let path_s = rest;
            let path = path::PathBuf::from_str(&path_s)?;
            let (db, file_system) = if path_s.is_empty() {
                (":memory:".to_string(), None)
            } else if path.is_dir() {
                (":memory:".to_string(), Some(path))
            } else {
                let fs_dir = path.parent().map(|p| p.to_path_buf());
                (path.to_string_lossy().into_owned(), fs_dir)
            };
            return Ok(Spec::Duckdb { db, file_system });
        } else if scheme == "tcp" {
            let addr = rest
                .strip_prefix("//")
                .ok_or_else(|| anyhow::anyhow!("Invalid tcp: URL"))?
                .to_string();
            return Ok(Spec::Tcp { addr });
        }
        Err(anyhow::anyhow!("Unknown runner scheme: {scheme}"))
    }
}

/// Split on ":"; if absent, treat the whole string as the scheme.
fn split_scheme(mut url: String) -> (String, String) {
    let mut rest = String::new();
    if let Some(i) = url.find(":") {
        rest = url.split_off(i + 1);
        url.truncate(i);
    }
    (url, rest)
}

#[derive(Clone, Debug)]
pub(crate) enum Spec {
    Interpreter {
        file_system: Option<path::PathBuf>,
    },
    Postgres {
        url: String,
    },
    Duckdb {
        db: String,
        file_system: Option<path::PathBuf>,
    },
    Tcp {
        addr: String,
    },
}

impl Spec {
    fn thread_name(&self) -> &'static str {
        match self {
            Self::Interpreter { .. } => "runner-interpreter",
            Self::Postgres { .. } => "runner-postgres",
            Self::Duckdb { .. } => "runner-duckdb",
            Self::Tcp { .. } => "runner-tcp",
        }
    }
}

pub(crate) enum AnyRunner {
    Interpreter(lutra_runner::AsyncRunner<lutra_interpreter::InterpreterRunner<'static>>),
    Postgres(lutra_runner_postgres::RunnerAsync),
    Duckdb(lutra_runner::AsyncRunner<lutra_runner_duckdb::Runner>),
    Tcp(lutra_runner::binary::tokio::Client<tokio::net::TcpStream>),
}

impl AnyRunner {
    pub(crate) async fn connect(spec: &Spec) -> anyhow::Result<AnyRunner> {
        match spec {
            Spec::Interpreter { file_system } => {
                let runner = lutra_interpreter::InterpreterRunner::default()
                    .with_file_system(file_system.clone());
                Ok(AnyRunner::Interpreter(lutra_runner::AsyncRunner::new(
                    runner,
                )))
            }
            Spec::Postgres { url } => {
                let (runner, conn) =
                    lutra_runner_postgres::RunnerAsync::connect_no_tls(url).await?;
                tokio::task::spawn(async {
                    if let Err(e) = conn.await {
                        eprintln!("{e}");
                    }
                });
                Ok(AnyRunner::Postgres(runner))
            }
            Spec::Duckdb { db, file_system } => {
                Ok(AnyRunner::Duckdb(lutra_runner::AsyncRunner::new(
                    lutra_runner_duckdb::Runner::open(db, file_system.clone())?,
                )))
            }
            Spec::Tcp { addr } => {
                let tcp_stream = tokio::net::TcpStream::connect(addr)
                    .await
                    .map_err(|e| anyhow::anyhow!("failed to connect to runner at {addr}: {e}"))?;
                Ok(AnyRunner::Tcp(lutra_runner::binary::tokio::Client::new(
                    tcp_stream,
                )))
            }
        }
    }
}

impl lutra_runner::Run for AnyRunner {
    async fn prepare(&self, program: rr::Program) -> Result<u32, proto::Error> {
        match self {
            Self::Interpreter(r) => r.prepare(program).await,
            Self::Postgres(r) => r.prepare(program).await,
            Self::Duckdb(r) => r.prepare(program).await,
            Self::Tcp(r) => r.prepare(program).await,
        }
    }

    async fn execute(&self, program_id: u32, input: &[u8]) -> Result<vec::Vec<u8>, proto::Error> {
        match self {
            Self::Interpreter(r) => r.execute(program_id, input).await,
            Self::Postgres(r) => r.execute(program_id, input).await,
            Self::Duckdb(r) => r.execute(program_id, input).await,
            Self::Tcp(r) => r.execute(program_id, input).await,
        }
    }

    async fn pull_schema(&self) -> Result<string::String, proto::Error> {
        match self {
            Self::Interpreter(r) => r.pull_schema().await,
            Self::Postgres(r) => r.pull_schema().await,
            Self::Duckdb(r) => r.pull_schema().await,
            Self::Tcp(r) => r.pull_schema().await,
        }
    }

    async fn release(&self, program_id: u32) -> Result<(), proto::Error> {
        match self {
            Self::Interpreter(r) => r.release(program_id).await,
            Self::Postgres(r) => r.release(program_id).await,
            Self::Duckdb(r) => r.release(program_id).await,
            Self::Tcp(r) => r.release(program_id).await,
        }
    }

    async fn shutdown(&self) -> Result<(), proto::Error> {
        match self {
            Self::Interpreter(r) => r.shutdown().await,
            Self::Postgres(r) => r.shutdown().await,
            Self::Duckdb(r) => r.shutdown().await,
            Self::Tcp(r) => r.shutdown().await,
        }
    }

    async fn get_externals(&self) -> Result<vec::Vec<string::String>, proto::Error> {
        match self {
            Self::Interpreter(r) => r.get_externals().await,
            Self::Postgres(r) => r.get_externals().await,
            Self::Duckdb(r) => r.get_externals().await,
            Self::Tcp(r) => r.get_externals().await,
        }
    }
}

pub(crate) enum AnyRunnerSync {
    Interpreter(lutra_interpreter::InterpreterRunner<'static>),
    Postgres(lutra_runner::SyncRunner<lutra_runner_postgres::RunnerAsync>),
    Duckdb(lutra_runner_duckdb::Runner),
    Tcp(lutra_runner::SyncRunner<lutra_runner::binary::tokio::Client<tokio::net::TcpStream>>),
}

impl AnyRunnerSync {
    pub(crate) fn connect(spec: &Spec) -> anyhow::Result<AnyRunnerSync> {
        match spec {
            Spec::Interpreter { file_system } => {
                let runner = lutra_interpreter::InterpreterRunner::default()
                    .with_file_system(file_system.clone());
                Ok(AnyRunnerSync::Interpreter(runner))
            }
            Spec::Postgres { url } => {
                let rt = tokio::runtime::Builder::new_current_thread()
                    .enable_all()
                    .build()?;
                let (runner, conn) =
                    rt.block_on(lutra_runner_postgres::RunnerAsync::connect_no_tls(url))?;
                rt.spawn(async {
                    if let Err(e) = conn.await {
                        eprintln!("{e}");
                    }
                });
                Ok(AnyRunnerSync::Postgres(
                    lutra_runner::SyncRunner::with_runtime(runner, rt),
                ))
            }
            Spec::Duckdb { db, file_system } => Ok(AnyRunnerSync::Duckdb(
                lutra_runner_duckdb::Runner::open(db, file_system.clone())?,
            )),
            Spec::Tcp { addr } => {
                let rt = tokio::runtime::Builder::new_current_thread()
                    .enable_all()
                    .build()
                    .unwrap();

                let tcp_stream = rt
                    .block_on(tokio::net::TcpStream::connect(addr))
                    .map_err(|e| anyhow::anyhow!("failed to connect to runner at {addr}: {e}"))?;
                Ok(AnyRunnerSync::Tcp(lutra_runner::SyncRunner::with_runtime(
                    lutra_runner::binary::tokio::Client::new(tcp_stream),
                    rt,
                )))
            }
        }
    }
}

impl lutra_runner::RunSync for AnyRunnerSync {
    fn prepare_sync(&mut self, program: rr::Program) -> Result<u32, proto::Error> {
        match self {
            Self::Interpreter(r) => r.prepare_sync(program),
            Self::Postgres(r) => r.prepare_sync(program),
            Self::Duckdb(r) => r.prepare_sync(program),
            Self::Tcp(r) => r.prepare_sync(program),
        }
    }

    fn execute_sync(
        &mut self,
        program_id: u32,
        input: &[u8],
    ) -> Result<vec::Vec<u8>, proto::Error> {
        match self {
            Self::Interpreter(r) => r.execute_sync(program_id, input),
            Self::Postgres(r) => r.execute_sync(program_id, input),
            Self::Duckdb(r) => r.execute_sync(program_id, input),
            Self::Tcp(r) => r.execute_sync(program_id, input),
        }
    }

    fn pull_schema_sync(&mut self) -> Result<string::String, proto::Error> {
        match self {
            Self::Interpreter(r) => r.pull_schema_sync(),
            Self::Postgres(r) => r.pull_schema_sync(),
            Self::Duckdb(r) => r.pull_schema_sync(),
            Self::Tcp(r) => r.pull_schema_sync(),
        }
    }

    fn release_sync(&mut self, program_id: u32) -> Result<(), proto::Error> {
        match self {
            Self::Interpreter(r) => r.release_sync(program_id),
            Self::Postgres(r) => r.release_sync(program_id),
            Self::Duckdb(r) => r.release_sync(program_id),
            Self::Tcp(r) => r.release_sync(program_id),
        }
    }

    fn shutdown_sync(&mut self) -> Result<(), proto::Error> {
        match self {
            Self::Interpreter(r) => r.shutdown_sync(),
            Self::Postgres(r) => r.shutdown_sync(),
            Self::Duckdb(r) => r.shutdown_sync(),
            Self::Tcp(r) => r.shutdown_sync(),
        }
    }

    fn get_externals_sync(&mut self) -> Result<vec::Vec<string::String>, proto::Error> {
        match self {
            Self::Interpreter(r) => r.get_externals_sync(),
            Self::Postgres(r) => r.get_externals_sync(),
            Self::Duckdb(r) => r.get_externals_sync(),
            Self::Tcp(r) => r.get_externals_sync(),
        }
    }
}

/// Construct a runner and spawns it on a separate thread.
///
/// This is done only because we want to have a uniform interface that is
/// provided by the [lutra_runner::channel::Client].
/// Ideally, we could return `Box<dyn RunSync>`, but RunSync is not dyn compatible.
pub fn spawn(
    spec: Spec,
) -> anyhow::Result<(lutra_runner::channel::Client, std::thread::JoinHandle<()>)> {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()?;
    let runner = rt.block_on(AnyRunner::connect(&spec))?;

    let (client, server) = lutra_runner::channel::new_pair(runner);
    let handle = std::thread::Builder::new()
        .name(spec.thread_name().to_string())
        .spawn(move || {
            rt.block_on(server.listen());
        })?;

    Ok((client, handle))
}
