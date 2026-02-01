use std::collections::HashMap;
use std::sync::mpsc::{Receiver, Sender};

use lutra_bin::{Decode, rr};

use crate::Run;
use crate::binary::messages;

/// Server that receives commands from clients and executes them on a runner.
///
/// The server runs in a dedicated thread and maintains a cache of prepared programs.
/// It processes commands sequentially in the order they are received.
pub struct Server<R: Run> {
    rx: Receiver<messages::ClientMessage>,
    tx: Sender<messages::ServerMessage>,
    runner: R,
    prepared_programs: HashMap<u32, Result<R::Prepared, messages::Error>>,
}

impl<R: Run> Server<R> {
    /// Create a new server with the given channels and runner.
    ///
    /// Typically you should use `channel_pair()` instead of constructing directly.
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

    /// Run the server loop.
    ///
    /// This method blocks, processing commands until the client disconnects
    /// (channel is closed). When the loop exits, cleanup is performed.
    pub async fn run(mut self) {
        // Process commands until channel closes
        while let Ok(msg) = self.rx.recv() {
            match msg {
                messages::ClientMessage::Prepare(prepare) => {
                    self.handle_prepare(prepare).await;
                }
                messages::ClientMessage::Execute(execute) => {
                    self.handle_execute(execute).await;
                }
                messages::ClientMessage::Release(release) => {
                    self.handle_release(release);
                }
            }
        }

        // Cleanup: shutdown runner
        let _ = self.runner.shutdown().await;
    }

    async fn handle_prepare(&mut self, prepare: messages::Prepare) {
        let result = match rr::Program::decode(&prepare.program) {
            Ok(program) => {
                let r = self.runner.prepare(program).await;

                r.map_err(|e| messages::Error {
                    message: format!("{:#?}", e),
                    code: Some(crate::error_codes::PREPARE_ERROR.to_string()),
                })
            }
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

        let _ = self.tx.send(response);
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
            Err(err) => return messages::Result::Err(err.clone()),
        };

        let res = self.runner.execute(handle, &execute.input).await;

        match res {
            Ok(bytes) => messages::Result::Ok(bytes),
            Err(e) => messages::Result::Err(messages::Error {
                message: format!("{:#?}", e),
                code: Some(crate::error_codes::EXECUTION_ERROR.to_string()),
            }),
        }
    }

    fn handle_release(&mut self, release: messages::Release) {
        self.prepared_programs.remove(&release.program_id);
    }
}
