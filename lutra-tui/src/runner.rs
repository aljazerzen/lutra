use std::sync::mpsc;
use std::thread::JoinHandle;
use std::time::{Duration, Instant};

use crate::terminal::Action;
use lutra_bin::rr;

pub use lutra_runner::proto;

#[derive(Debug, Clone)]
pub struct RunnerConfig {
    pub format: lutra_compiler::ProgramFormat,
}

/// Handle of a runner instance.
pub(crate) struct RunnerProxy {
    sender: lutra_runner::channel::ClientSender,

    runner_thread: JoinHandle<()>,
    message_thread: JoinHandle<()>,
}

impl RunnerProxy {
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

/// State needed to communicate with a runner
pub(crate) struct RunnerSession {
    format: lutra_compiler::ProgramFormat,
    client: lutra_runner::channel::ClientSender,
    prepared: Option<PreparedProgram>,
    pending: Option<Pending>,
}

struct PreparedProgram {
    program_id: u32,
}

struct Pending {
    request_id: u32,
    start_time: Instant,
    kind: PendingKind,
}
enum PendingKind {
    Execute,
    Pull,
}

pub(crate) struct RunnerEvent {
    pub duration: Duration,
    pub response: proto::ResponseKind,
}

impl RunnerSession {
    pub fn new(
        client: lutra_runner::channel::ClientSender,
        format: lutra_compiler::ProgramFormat,
    ) -> Self {
        Self {
            format,
            client,
            prepared: None,
            pending: None,
        }
    }

    pub fn format(&self) -> lutra_compiler::ProgramFormat {
        self.format
    }

    pub fn run(&mut self, program: &rr::Program, input: Vec<u8>) -> Result<(), String> {
        self.prepare(program)?;
        self.execute(input)
    }

    pub fn prepare(&mut self, program: &rr::Program) -> Result<(), String> {
        if self.pending.is_some() {
            return Err("Execution already in progress".to_string());
        }

        self.release();

        let (_request_id, program_id) = self
            .client
            .prepare(program)
            .map_err(|e| format!("Failed to prepare program: {e}"))?;

        self.prepared = Some(PreparedProgram { program_id });
        Ok(())
    }

    pub fn release(&mut self) {
        if let Some(prepared) = self.prepared.take() {
            let _ = self.client.release(prepared.program_id);
        }
    }

    pub fn execute(&mut self, input: Vec<u8>) -> Result<(), String> {
        if self.pending.is_some() {
            return Err("runner busy".to_string());
        }

        let Some(prepared) = self.prepared.as_ref() else {
            return Err("internal error: missing prepared program".to_string());
        };

        let request_id = match self.client.execute(prepared.program_id, &input) {
            Ok(request_id) => request_id,
            Err(e) => {
                self.release();
                return Err(format!("Failed to execute: {e}"));
            }
        };

        self.pending = Some(Pending {
            request_id,
            start_time: Instant::now(),
            kind: PendingKind::Execute,
        });
        Ok(())
    }

    pub fn pull(&mut self) -> Result<(), String> {
        if self.pending.is_some() {
            return Err("runner busy".to_string());
        }

        let request_id = self
            .client
            .pull_schema()
            .map_err(|e| format!("failed to request schema: {e}"))?;
        self.pending = Some(Pending {
            request_id,
            start_time: Instant::now(),
            kind: PendingKind::Pull,
        });
        Ok(())
    }

    pub fn handle_response(&mut self, response: proto::Response) -> Option<RunnerEvent> {
        let pending = self.pending.as_ref()?;
        if response.request_id != pending.request_id {
            todo!();
        }
        let pending = self.pending.take().unwrap();
        match (pending.kind, &response.kind) {
            (PendingKind::Pull, proto::ResponseKind::Schema(_)) => {}
            (PendingKind::Execute, proto::ResponseKind::Execute(_)) => {}
            _ => panic!("protocol error"),
        }
        let duration = pending.start_time.elapsed();
        Some(RunnerEvent {
            duration,
            response: response.kind,
        })
    }
}
