use core::ops::DerefMut;
use std::marker::Unpin;

use lutra_bin::Encode;
use tokio::io::{AsyncRead, AsyncWrite, AsyncWriteExt};

use crate::Run;
use crate::proto;

pub struct Client<C>
where
    C: AsyncRead + AsyncWrite + Unpin,
{
    stream: tokio::sync::Mutex<C>,
    next_program_id: std::sync::atomic::AtomicU32,
    next_request_id: std::sync::atomic::AtomicU32,
}

impl<C> Client<C>
where
    C: AsyncRead + AsyncWrite + Unpin,
{
    pub fn new(inner: C) -> Self {
        Client {
            stream: tokio::sync::Mutex::new(inner),
            next_program_id: 0.into(),
            next_request_id: 0.into(),
        }
    }

    pub async fn shutdown_stream(&self) -> Result<(), proto::Error> {
        self.stream
            .lock()
            .await
            .shutdown()
            .await
            .map_err(io_to_proto)
    }

    /// Assign a fresh `request_id`, wrap `kind` in a `Request`, write it to
    /// the stream, and return the id for response correlation.
    async fn send_request(&self, kind: proto::RequestKind) -> Result<u32, proto::Error> {
        let id = self
            .next_request_id
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        let mut stream = self.stream.lock().await;
        super::write_message(stream.deref_mut(), proto::Request { id, kind })
            .await
            .map_err(io_to_proto)?;
        Ok(id)
    }

    async fn send_prepare(&self, program: &lutra_bin::rr::Program) -> Result<u32, proto::Error> {
        let program_id = self
            .next_program_id
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let program_buf = program.encode();

        let request_id = self
            .send_request(proto::RequestKind::Prepare(proto::PrepareRequest {
                program_id,
                program: program_buf.to_vec(),
            }))
            .await?;

        let proto::ResponseKind::Prepare(result) = self.recv_for(request_id).await? else {
            return Err(unexpected_response_kind());
        };

        if let Some(err) = result.0 {
            return Err(err);
        }
        Ok(program_id)
    }

    async fn send_execute(&self, program_id: u32, input: &[u8]) -> Result<u32, proto::Error> {
        self.send_request(proto::RequestKind::Execute(proto::ExecuteRequest {
            program_id,
            input: input.to_vec(),
        }))
        .await
    }

    async fn send_release(&self, program_id: u32) -> Result<(), proto::Error> {
        let request_id = self
            .send_request(proto::RequestKind::Release(proto::ReleaseRequest {
                program_id,
            }))
            .await?;

        let proto::ResponseKind::Release(result) = self.recv_for(request_id).await? else {
            return Err(unexpected_response_kind());
        };

        if let Some(err) = result.0 {
            return Err(err);
        }
        Ok(())
    }

    pub async fn pull_schema(&self) -> Result<std::string::String, proto::Error> {
        let request_id = self.send_request(proto::RequestKind::PullSchema).await?;

        let proto::ResponseKind::Schema(result) = self.recv_for(request_id).await? else {
            return Err(unexpected_response_kind());
        };

        match result {
            proto::SchemaResult::Ok(schema) => Ok(schema),
            proto::SchemaResult::Err(e) => Err(e),
        }
    }

    /// Read responses until finding the one with the matching `request_id`.
    pub async fn recv_for(&self, request_id: u32) -> Result<proto::ResponseKind, proto::Error> {
        loop {
            let message = {
                let mut stream = self.stream.lock().await;
                stream.flush().await.map_err(io_to_proto)?;

                super::read_message::<proto::Response>(stream.deref_mut())
                    .await
                    .map_err(io_to_proto)
            };

            match message {
                Ok(response) => {
                    if response.request_id == request_id {
                        return Ok(response.kind);
                    }
                }
                Err(e) => return Err(e),
            }
        }
    }
}

impl<C> Run for Client<C>
where
    C: AsyncRead + AsyncWrite + Unpin,
{
    async fn prepare(&self, program: lutra_bin::rr::Program) -> Result<u32, proto::Error> {
        self.send_prepare(&program).await
    }

    async fn execute(
        &self,
        program_id: u32,
        input: &[u8],
    ) -> Result<std::vec::Vec<u8>, proto::Error> {
        let request_id = self.send_execute(program_id, input).await?;
        let proto::ResponseKind::Execute(result) = self.recv_for(request_id).await? else {
            return Err(unexpected_response_kind());
        };

        match result {
            proto::ExecuteResult::Ok(output) => Ok(output),
            proto::ExecuteResult::Err(err) => Err(err),
        }
    }

    async fn pull_schema(&self) -> Result<std::string::String, proto::Error> {
        self.pull_schema().await
    }

    async fn release(&self, program_id: u32) -> Result<(), proto::Error> {
        self.send_release(program_id).await
    }

    fn shutdown(&self) -> impl Future<Output = Result<(), proto::Error>> {
        self.shutdown_stream()
    }
}

fn unexpected_response_kind() -> proto::Error {
    proto::Error {
        display: "unexpected response kind".to_string(),
        code: None,
    }
}

fn io_to_proto(e: std::io::Error) -> proto::Error {
    proto::Error {
        display: format!("{}", e),
        code: None,
    }
}
