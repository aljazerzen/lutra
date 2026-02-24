use std::collections::HashMap;
use tokio::sync::mpsc::{Receiver, Sender};

use lutra_bin::{Decode, rr};

use crate::Run;
use crate::proto;

/// Server that receives commands from clients and executes them on a Run runner.
pub struct Server<R: Run> {
    rx: Receiver<proto::Request>,
    tx: Sender<proto::Response>,
    runner: R,
    /// Maps client program_id → runner-allocated program_id.
    program_ids: HashMap<u32, u32>,
}

impl<R: Run> Server<R> {
    /// Create a new server with the given channels and runner.
    ///
    /// Typically you should use `new_pair()` instead of constructing directly.
    pub fn new(rx: Receiver<proto::Request>, tx: Sender<proto::Response>, runner: R) -> Self {
        Self {
            rx,
            tx,
            runner,
            program_ids: HashMap::new(),
        }
    }

    /// Run the server loop asynchronously.
    pub async fn listen(mut self) {
        while let Some(req) = self.rx.recv().await {
            let response = match req.kind {
                proto::RequestKind::Prepare(prepare) => {
                    proto::ResponseKind::Prepare(self.handle_prepare(prepare).await.into())
                }
                proto::RequestKind::Execute(execute) => {
                    proto::ResponseKind::Execute(self.handle_execute(execute).await.into())
                }
                proto::RequestKind::Release(release) => {
                    proto::ResponseKind::Release(self.handle_release(release).await.into())
                }
                proto::RequestKind::PullSchema => {
                    proto::ResponseKind::Schema(self.runner.pull_schema().await.into())
                }
            };
            let response = proto::Response {
                request_id: req.id,
                kind: response,
            };
            self.send(response).await;
        }

        let _ = self.runner.shutdown().await;
    }

    async fn send(&mut self, response: proto::Response) {
        let _ = self.tx.send(response).await;
    }

    async fn handle_prepare(&mut self, prepare: proto::PrepareRequest) -> Result<(), proto::Error> {
        let program_id = rr::Program::decode(&prepare.program).map_err(proto::Error::from)?;

        let runner_id = self.runner.prepare(program_id).await?;
        self.program_ids.insert(prepare.program_id, runner_id);

        Ok(())
    }

    async fn handle_execute(
        &mut self,
        execute: proto::ExecuteRequest,
    ) -> Result<Vec<u8>, proto::Error> {
        let handle = (self.program_ids.get(&execute.program_id))
            .copied()
            .ok_or_else(|| proto::Error::program_not_found(execute.program_id))?;

        self.runner.execute(handle, &execute.input).await
    }

    async fn handle_release(&mut self, release: proto::ReleaseRequest) -> Result<(), proto::Error> {
        let Some(program_id) = self.program_ids.remove(&release.program_id) else {
            return Ok(());
        };
        self.runner.release(program_id).await
    }
}
