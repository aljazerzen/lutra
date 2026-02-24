use std::sync::mpsc;
use std::thread::JoinHandle;

use crate::terminal::Action;

/// Runner configuration for execution.
#[derive(Debug, Clone)]
pub struct RunnerConfig {
    pub format: lutra_compiler::ProgramFormat,
}

pub(crate) struct Runner {
    sender: lutra_runner::channel::ClientSender,

    runner_thread: JoinHandle<()>,
    message_thread: JoinHandle<()>,
}

impl Runner {
    pub fn try_new(
        client: lutra_runner::channel::Client,
        runner_thread: std::thread::JoinHandle<()>,
        action_tx: mpsc::Sender<Action>,
    ) -> Result<Self, anyhow::Error> {
        // Split client into sender and receiver
        let (sender, mut receiver) = client.split();

        // Spawn runner message thread that translates runner messages to actions
        let action_tx_runner = action_tx.clone();
        let message_thread = std::thread::Builder::new()
            .name("runner-messages".to_string())
            .spawn(move || {
                // Blocking loop receiving messages
                while let Some(msg) = receiver.blocking_recv() {
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
