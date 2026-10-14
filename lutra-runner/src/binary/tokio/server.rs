use std::collections::HashMap;
use std::marker::Unpin;

use lutra_bin::{Decode, Encode};
use lutra_bin_stream::{
    Compression,
    r#async::{Reader, Writer},
    tokio::TokioIo,
};
use tokio::io::{AsyncRead, AsyncWrite};

use crate::Run;
use crate::proto;

pub struct Server<'r, C, R>
where
    C: AsyncRead + AsyncWrite + Unpin,
    R: Run,
{
    stream: C,
    runner: &'r R,
    /// Maps client program_id → runner-allocated program_id.
    program_ids: HashMap<u32, u32>,
    started: bool,
}

impl<'r, C, R> Server<'r, C, R>
where
    C: AsyncRead + AsyncWrite + Unpin,
    R: Run,
{
    pub fn new(stream: C, runner: &'r R) -> Self {
        Self {
            stream,
            runner,
            program_ids: HashMap::new(),
            started: false,
        }
    }

    /// Serve one connection. A cancelled or completed session cannot be restarted.
    pub async fn run(&mut self) -> Result<(), std::io::Error> {
        if self.started {
            return Err(super::stream_error("server session has already started"));
        }
        self.started = true;
        let result = self.run_session().await;
        let mut cleanup_error = None;
        for (_, handle) in self.program_ids.drain() {
            if let Err(err) = self.runner.release(handle).await {
                cleanup_error.get_or_insert_with(|| super::stream_error(err.display));
            }
        }
        result.and(match cleanup_error {
            Some(err) => Err(err),
            None => Ok(()),
        })
    }

    async fn run_session(&mut self) -> Result<(), std::io::Error> {
        let mut reader = Reader::new();
        let mut writer = Writer::new(Compression::None).map_err(super::stream_error)?;
        // Send the response header with the first response (or EOS). Writing
        // it before reading requests can deadlock a tiny bounded duplex pipe
        // when the client is still writing its request.
        while let Some(frame) = reader
            .read_frame(&mut TokioIo(&mut self.stream))
            .await
            .map_err(super::stream_error)?
        {
            let request = proto::Request::decode(&frame).map_err(super::stream_error)?;
            let response = self.handle_request(request).await;
            writer
                .write_frame(&mut TokioIo(&mut self.stream), &response.encode())
                .await
                .map_err(super::stream_error)?;
            writer
                .flush(&mut TokioIo(&mut self.stream))
                .await
                .map_err(super::stream_error)?;
        }
        writer
            .finish(&mut TokioIo(&mut self.stream))
            .await
            .map_err(super::stream_error)
    }

    async fn handle_request(&mut self, req: proto::Request) -> proto::Response {
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
            proto::RequestKind::GetExternals => {
                tracing::trace!("get_externals");

                proto::ResponseKind::Externals(self.runner.get_externals().await.into())
            }
        };
        proto::Response {
            kind: res,
            request_id: req.id,
        }
    }

    async fn handle_prepare(&mut self, prepare: proto::PrepareRequest) -> Result<(), proto::Error> {
        let program =
            lutra_bin::rr::Program::decode(&prepare.program).map_err(proto::Error::from)?;

        let handle = self.runner.prepare(program).await?;
        if let Some(previous) = self.program_ids.insert(prepare.program_id, handle) {
            self.runner.release(previous).await?;
        }
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
