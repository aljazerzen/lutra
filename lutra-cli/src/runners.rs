#[derive(clap::Args)]
#[group(required = false, multiple = false)]
pub(crate) struct RunnerParams {
    /// Runner URL. Scheme selects the runner:
    ///   interpreter:// or lt:// - interpreter
    ///   postgres://DSN          - PostgreSQL
    ///   duckdb://PATH           - DuckDB (use duckdb://:memory: for in-memory)
    ///   tcp://HOST:PORT          - remote runner over TCP (binary protocol)
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
    pub(crate) fn into_url(mut self) -> Option<RunnerUrl> {
        if let Some(url) = self.runner.take() {
            return Some(RunnerUrl::parse(url));
        }
        if self.interpreter {
            return Some(RunnerUrl::new("lt", "".into()));
        }
        if let Some(postgres) = self.postgres {
            return Some(RunnerUrl::new("postgres", postgres));
        }
        if let Some(duckdb) = self.duckdb {
            return Some(RunnerUrl::new("duckdb", duckdb));
        }
        None
    }

    pub(crate) fn into_url_or(self, default: Option<&str>) -> anyhow::Result<RunnerUrl> {
        self.into_url()
            .or_else(|| default.map(|url| RunnerUrl::parse(url.to_string())))
            .ok_or(anyhow::anyhow!("Missing --runner or @!runner"))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct RunnerUrl {
    scheme: String,
    rest: String,
}

impl RunnerUrl {
    fn new(scheme: &str, rest: String) -> Self {
        let scheme = scheme.to_string();
        Self { scheme, rest }
    }
    fn parse(mut url: String) -> Self {
        // Split on "://"; if absent, treat the whole string as the scheme.
        let mut rest = String::new();
        if let Some(i) = url.find("://") {
            rest = url.split_off(i + 3);
            url.truncate(i);
        }
        RunnerUrl { scheme: url, rest }
    }
    fn as_interpreter(&self) -> Option<&str> {
        (self.scheme == "interpreter" || self.scheme == "lt").then_some(&self.rest)
    }
    fn as_postgres(&self) -> Option<&str> {
        (self.scheme == "postgres").then_some(&self.rest)
    }
    fn as_duckdb(&self) -> Option<&str> {
        (self.scheme == "duckdb").then_some(&self.rest)
    }
    fn as_tcp(&self) -> Option<&str> {
        (self.scheme == "tcp").then_some(&self.rest)
    }
    fn err_unknown(&self) -> anyhow::Error {
        anyhow::anyhow!("Unknown runner scheme: {}", self.scheme)
    }
}

/// Construct a runner and spawns it on a separate thread.
///
/// This is done only because we want to have a uniform interface that is
/// provided by the [lutra_runner::channel::Client].
/// Ideally, we could return `Box<dyn RunSync>`, but RunSync is not dyn compatible.
pub fn init(
    runner: RunnerUrl,
    project: Option<&lutra_compiler::SourceTree>,
) -> anyhow::Result<(lutra_runner::channel::Client, std::thread::JoinHandle<()>)> {
    let file_system = project.map(|p| p.get_project_dir().to_path_buf());

    Ok(if runner.as_interpreter().is_some() {
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
    } else if let Some(pg_url) = runner.as_postgres() {
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;

        let runner = rt
            .block_on(async { lutra_runner_postgres::RunnerAsync::connect_no_tls(pg_url).await })?;

        let (client, server) = lutra_runner::channel::new_pair(runner);

        let handle = std::thread::Builder::new()
            .name("runner-postgres".to_string())
            .spawn(move || {
                rt.block_on(server.listen());
            })?;
        (client, handle)
    } else if let Some(duckdb_url) = runner.as_duckdb() {
        let runner = lutra_runner_duckdb::Runner::open(duckdb_url, file_system)?;
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
    } else if let Some(addr) = runner.as_tcp() {
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;

        let tcp_stream = rt
            .block_on(tokio::net::TcpStream::connect(&addr))
            .map_err(|e| anyhow::anyhow!("failed to connect to runner at {addr}: {e}"))?;

        let runner = lutra_runner::binary::tokio::Client::new(tcp_stream);
        let (client, server) = lutra_runner::channel::new_pair(runner);

        let handle = std::thread::Builder::new()
            .name("runner-tcp".to_string())
            .spawn(move || {
                rt.block_on(server.listen());
            })?;
        (client, handle)
    } else {
        unreachable!()
    })
}

/// Bind a TCP server and proxy incoming connections to the downstream runner.
#[tokio::main(flavor = "current_thread")]
pub async fn serve(runner: RunnerUrl, addr: &str) -> anyhow::Result<()> {
    let listener = tokio::net::TcpListener::bind(addr).await?;
    eprintln!("Listening on {addr}");

    if runner.as_interpreter().is_some() {
        let r = lutra_interpreter::InterpreterRunner::default();
        let r = lutra_runner::AsyncRunner::new(r);
        accept_loop(listener, &r).await
    } else if let Some(pg_url) = runner.as_postgres() {
        let r = lutra_runner_postgres::RunnerAsync::connect_no_tls(pg_url).await?;
        accept_loop(listener, &r).await
    } else if let Some(duckdb_url) = runner.as_duckdb() {
        let r = lutra_runner_duckdb::Runner::open(duckdb_url, None)?;
        let r = lutra_runner::AsyncRunner::new(r);
        accept_loop(listener, &r).await
    } else if let Some(tcp_addr) = runner.as_tcp() {
        let tcp_stream = tokio::net::TcpStream::connect(&tcp_addr)
            .await
            .map_err(|e| anyhow::anyhow!("failed to connect to runner at {tcp_addr}: {e}"))?;
        let r = lutra_runner::binary::tokio::Client::new(tcp_stream);
        accept_loop(listener, &r).await
    } else {
        return Err(runner.err_unknown());
    }
}

/// Accept loop for serving a runner over TCP.
///
/// One client at a time; after disconnect the next connection is accepted.
async fn accept_loop<R: lutra_runner::Run>(
    listener: tokio::net::TcpListener,
    runner: &R,
) -> anyhow::Result<()> {
    loop {
        let (stream, peer) = listener.accept().await?;
        eprintln!("Connection from {peer}");

        let mut server = lutra_runner::binary::tokio::Server::new(stream, runner);
        if let Err(e) = server.run().await {
            eprintln!("Connection from {peer} error: {e}");
        }
        eprintln!("Connection from {peer} closed");
    }
}
