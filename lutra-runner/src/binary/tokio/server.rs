use std::collections::HashMap;
use std::marker::Unpin;

use lutra_bin::Decode;
use tokio::io::{AsyncRead, AsyncWrite, AsyncWriteExt};

use crate::Run;
use crate::proto;

pub struct Server<C, R>
where
    C: AsyncRead + AsyncWrite + Unpin,
    R: Run,
{
    stream: C,
    runner: R,
    /// Maps client program_id → runner-allocated program_id.
    program_ids: HashMap<u32, u32>,
}

impl<C, R> Server<C, R>
where
    C: AsyncRead + AsyncWrite + Unpin,
    R: Run,
{
    pub fn new(stream: C, runner: R) -> Self {
        Self {
            stream,
            runner,
            program_ids: HashMap::new(),
        }
    }

    pub async fn run(&mut self) -> Result<(), std::io::Error> {
        while let Ok(request) = super::read_message(&mut self.stream).await {
            self.handle_request(request).await?;

            self.stream.flush().await?;
        }
        Ok(())
    }

    async fn handle_request(&mut self, req: proto::Request) -> Result<(), std::io::Error> {
        let res = match req.kind {
            proto::RequestKind::Prepare(prepare) => {
                tracing::trace!("prepare");

                proto::ResponseKind::Prepare(self.handle_prepare(prepare).await.into())
            }
            proto::RequestKind::Execute(execute) => {
                tracing::trace!("execute");

                proto::ResponseKind::Execute(self.handle_execute(execute).await.into())
            }
            proto::RequestKind::Release(release) => {
                tracing::trace!("release");

                proto::ResponseKind::Release(self.handle_release(release).await.into())
            }
            proto::RequestKind::PullSchema => {
                tracing::trace!("pull_schema");

                proto::ResponseKind::Schema(self.runner.pull_schema().await.into())
            }
        };
        let res = proto::Response {
            kind: res,
            request_id: req.id,
        };

        super::write_message(&mut self.stream, res).await
    }

    async fn handle_prepare(&mut self, prepare: proto::PrepareRequest) -> Result<(), proto::Error> {
        let program =
            lutra_bin::rr::Program::decode(&prepare.program).map_err(proto::Error::from)?;

        let handle = self.runner.prepare(program).await?;
        self.program_ids.insert(prepare.program_id, handle);
        Ok(())
    }

    async fn handle_execute(
        &mut self,
        execute: proto::ExecuteRequest,
    ) -> Result<Vec<u8>, proto::Error> {
        let runner_id = (self.program_ids.get(&execute.program_id))
            .copied()
            .ok_or_else(|| proto::Error::program_not_found(execute.program_id))?;

        self.runner.execute(runner_id, &execute.input).await
    }

    async fn handle_release(&mut self, release: proto::ReleaseRequest) -> Result<(), proto::Error> {
        let Some(handle) = self.program_ids.remove(&release.program_id) else {
            return Ok(());
        };

        self.runner.release(handle).await
    }
}
