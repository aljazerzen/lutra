use std::sync::mpsc;
use std::thread::JoinHandle;

use crate::terminal::Action;

use std::path::PathBuf;

/// Runner configuration for execution.
#[derive(Debug, Clone)]
pub enum RunnerConfig {
    Interpreter { fs_root: Option<PathBuf> },
    Postgres { connection_string: String },
    DuckDB { path: String },
}

impl Default for RunnerConfig {
    fn default() -> Self {
        RunnerConfig::Interpreter { fs_root: None }
    }
}

pub(crate) struct Runner {
    sender: lutra_runner::channel::ClientSender,

    runner_thread: JoinHandle<()>,
    message_thread: JoinHandle<()>,
}

impl Runner {
    pub fn try_new(
        runner: &RunnerConfig,
        action_tx: mpsc::Sender<Action>,
    ) -> Result<Self, anyhow::Error> {
        // Create runner client + thread based on runner config
        let (client, runner_thread) = match &runner {
            RunnerConfig::Interpreter { fs_root } => {
                let runner = lutra_interpreter::InterpreterRunner::default()
                    .with_file_system(fs_root.clone());
                let (client, server) = lutra_runner::channel::new_pair(runner);

                let _runner_thread = std::thread::Builder::new()
                    .name("runner-interpreter".to_string())
                    .spawn(move || {
                        let rt = tokio::runtime::Builder::new_current_thread()
                            .enable_all()
                            .build()
                            .unwrap();
                        rt.block_on(server.run());
                    })?;
                (client, _runner_thread)
            }
            RunnerConfig::Postgres { connection_string } => {
                // Create tokio runtime for async postgres connection
                let connection_string = connection_string.clone();
                let rt = tokio::runtime::Builder::new_current_thread()
                    .enable_all()
                    .build()?;

                let runner = rt.block_on(async {
                    lutra_runner_postgres::RunnerAsync::connect_no_tls(&connection_string).await
                })?;

                let (client, server) = lutra_runner::channel::new_pair(runner);

                let _runner_thread = std::thread::Builder::new()
                    .name("runner-postgres".to_string())
                    .spawn(move || {
                        rt.block_on(server.run());
                    })?;
                (client, _runner_thread)
            }
            RunnerConfig::DuckDB { path } => {
                // Create tokio runtime for async duckdb connection
                let path = path.clone();
                let rt = tokio::runtime::Builder::new_current_thread()
                    .enable_all()
                    .build()?;

                let runner =
                    rt.block_on(async { lutra_runner_duckdb::Runner::open(&path).await })?;

                let (client, server) = lutra_runner::channel::new_pair(runner);

                let _runner_thread = std::thread::Builder::new()
                    .name("runner-duckdb".to_string())
                    .spawn(move || {
                        rt.block_on(server.run());
                    })?;
                (client, _runner_thread)
            }
        };

        // Split client into sender and receiver
        let (sender, receiver) = client.split();

        // Spawn runner message thread that translates runner messages to actions
        let action_tx_runner = action_tx.clone();
        let message_thread = std::thread::Builder::new()
            .name("runner-messages".to_string())
            .spawn(move || {
                // Blocking loop receiving messages
                while let Ok(msg) = receiver.recv() {
                    if action_tx_runner.send(Action::RunnerMessage(msg)).is_err() {
                        break; // Action channel closed, exit
                    }
                }
            })?;

        Ok(Self {
            sender,
            runner_thread,
            message_thread,
        })
    }

    pub fn get_client(&self) -> lutra_runner::channel::ClientSender {
        self.sender.clone()
    }

    /// Waits for all threads to stop
    pub fn join(self) {
        drop(self.sender);
        let _ = self.runner_thread.join();
        let _ = self.message_thread.join();
    }
}
