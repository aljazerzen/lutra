use std::collections::HashMap;
use tokio::sync::mpsc::{Receiver, Sender};

use lutra_bin::{Decode, rr};

use crate::Run;
use crate::binary::messages;

/// Server that receives commands from clients and executes them on a Run runner.
///
/// The server runs in a dedicated async task and maintains a cache of prepared programs.
/// It processes commands sequentially in the order they are received.
pub struct Server<R: Run> {
    rx: Receiver<messages::ClientMessage>,
    tx: Sender<messages::ServerMessage>,
    runner: R,
    prepared_programs: HashMap<u32, Result<R::Prepared, messages::Error>>,
}

impl<R: Run> Server<R>
where
    R::Prepared: Send,
{
    /// Create a new server with the given channels and runner.
    ///
    /// Typically you should use `new_pair()` instead of constructing directly.
    pub fn new(
        rx: Receiver<messages::ClientMessage>,
        tx: Sender<messages::ServerMessage>,
        runner: R,
    ) -> Self {
        Self {
            rx,
            tx,
            runner,
            prepared_programs: HashMap::new(),
        }
    }

    /// Run the server loop asynchronously.
    ///
    /// This method processes commands until the client disconnects
    /// (channel is closed). When the loop exits, cleanup is performed.
    pub async fn listen(mut self) {
        // Process commands until channel closes
        while let Some(msg) = self.rx.recv().await {
            match msg {
                messages::ClientMessage::Prepare(prepare) => {
                    self.handle_prepare(prepare).await;
                }
                messages::ClientMessage::Execute(execute) => {
                    self.handle_execute(execute).await;
                }
                messages::ClientMessage::Release(release) => {
                    self.handle_release(release).await;
                }
                messages::ClientMessage::PullSchema => {
                    self.handle_pull_schema().await;
                }
            }
        }

        // Cleanup: shutdown runner
        let _ = self.runner.shutdown().await;
    }

    async fn handle_prepare(&mut self, prepare: messages::Prepare) {
        let result = match rr::Program::decode(&prepare.program) {
            Ok(program) => self
                .runner
                .prepare(program)
                .await
                .map_err(|e| messages::Error {
                    message: format!("{:#?}", e),
                    code: Some(crate::error_codes::PREPARE_ERROR.to_string()),
                }),
            Err(e) => Err(messages::Error {
                message: format!("{:#?}", e),
                code: Some(crate::error_codes::DECODE_ERROR.to_string()),
            }),
        };

        self.prepared_programs.insert(prepare.program_id, result);
    }

    async fn handle_execute(&mut self, execute: messages::Execute) {
        let result = self.do_execute(&execute).await;

        let response = messages::ServerMessage::Response(messages::Response {
            request_id: execute.request_id,
            result,
        });

        let _ = self.tx.send(response).await;
    }

    async fn do_execute(&mut self, execute: &messages::Execute) -> messages::Result {
        let Some(prepared) = self.prepared_programs.get(&execute.program_id) else {
            return messages::Result::Err(messages::Error {
                message: format!("Program {} not prepared", execute.program_id),
                code: Some(crate::error_codes::PROGRAM_NOT_FOUND.to_string()),
            });
        };

        let handle = match prepared {
            Ok(handle) => handle,
            Err(err) => {
                return messages::Result::Err(err.clone());
            }
        };

        match self.runner.execute(handle, &execute.input).await {
            Ok(output) => messages::Result::Ok(output),
            Err(e) => messages::Result::Err(messages::Error {
                message: format!("{:#?}", e),
                code: Some(crate::error_codes::EXECUTION_ERROR.to_string()),
            }),
        }
    }

    async fn handle_release(&mut self, release: messages::Release) {
        if let Some(Ok(prepared)) = self.prepared_programs.remove(&release.program_id) {
            let _ = self.runner.release(prepared).await;
        }
    }

    async fn handle_pull_schema(&mut self) {
        let result = match self.runner.pull_schema().await {
            Ok(schema) => messages::SchemaResult::Ok(schema),
            Err(e) => messages::SchemaResult::Err(messages::Error {
                message: format!("{:#?}", e),
                code: None,
            }),
        };

        let _ = self
            .tx
            .send(messages::ServerMessage::SchemaResponse(
                messages::SchemaResponse { result },
            ))
            .await;
    }
}
