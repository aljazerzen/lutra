use crate::vec;

use embedded_io_async::Read;
use embedded_io_async::ReadExactError;
use embedded_io_async::Write;

use crate::proto;

pub struct Client<C>
where
    C: Read + Write + Unpin,
{
    inner: C,

    next_program_id: u32,
    next_request_id: u32,
}

impl<C> Client<C>
where
    C: Read + Write + Unpin,
{
    pub fn new(inner: C) -> Self {
        Client {
            inner,
            next_program_id: 0,
            next_request_id: 0,
        }
    }

    async fn send_request(&mut self, kind: proto::RequestKind) -> Result<u32, C::Error> {
        let id = self.next_request_id;
        self.next_request_id += 1;

        super::write_message(&mut self.inner, proto::Request { id, kind }).await?;
        Ok(id)
    }

    /// Prepare, execute and release.
    pub async fn run_once(
        &mut self,
        program: &[u8],
        input: vec::Vec<u8>,
    ) -> Result<proto::ExecuteResult, C::Error> {
        let program_id = self.prepare(program).await?;
        let request_id = self.execute(program_id, input).await?;
        self.release(program_id).await?;

        self.recv_execute(request_id).await
    }

    pub async fn prepare(&mut self, program: &[u8]) -> Result<u32, C::Error> {
        let program_id = self.next_program_id;
        self.next_program_id += 1;

        let id = self
            .send_request(proto::RequestKind::Prepare(proto::PrepareRequest {
                program_id,
                program: program.to_vec(),
            }))
            .await?;
        self.inner.flush().await?;

        // Await prepare response and surface errors immediately
        loop {
            let res = super::read_message(&mut self.inner).await;
            let response: proto::Response = match res {
                Ok(Ok(msg)) => msg,
                Ok(Err(lutra_err)) => {
                    tracing::error!("prepare recv: {lutra_err:?}");
                    continue;
                }
                Err(ReadExactError::UnexpectedEof) => {
                    panic!("prepare: connection closed before receiving response");
                }
                Err(ReadExactError::Other(e)) => {
                    return Err(e);
                }
            };

            if response.request_id == id {
                match response.kind {
                    proto::ResponseKind::Prepare(result) => {
                        if let Some(err) = result.0 {
                            // Surface prepare errors immediately as a panic since embedded
                            // has no io::Error. Callers should handle this via the execute result.
                            panic!("prepare error: {}", err.display);
                        }
                        return Ok(program_id);
                    }
                    _ => {
                        // Unexpected response kind; discard.
                    }
                }
            }
        }
    }

    pub async fn execute(&mut self, program_id: u32, input: vec::Vec<u8>) -> Result<u32, C::Error> {
        self.send_request(proto::RequestKind::Execute(proto::ExecuteRequest {
            program_id,
            input,
        }))
        .await
    }

    pub async fn release(&mut self, program_id: u32) -> Result<(), C::Error> {
        let id = self
            .send_request(proto::RequestKind::Release(proto::ReleaseRequest {
                program_id,
            }))
            .await?;
        self.inner.flush().await?;

        // Await release response
        loop {
            let res = super::read_message(&mut self.inner).await;
            let response: proto::Response = match res {
                Ok(Ok(msg)) => msg,
                Ok(Err(lutra_err)) => {
                    tracing::error!("release recv: {lutra_err:?}");
                    continue;
                }
                Err(ReadExactError::UnexpectedEof) => {
                    panic!("release: connection closed before receiving response");
                }
                Err(ReadExactError::Other(e)) => {
                    return Err(e);
                }
            };

            if response.request_id == id {
                match response.kind {
                    proto::ResponseKind::Release(_result) => {
                        return Ok(());
                    }
                    _ => {
                        // Unexpected response kind; discard.
                    }
                }
            }
        }
    }

    pub async fn recv_execute(
        &mut self,
        request_id: u32,
    ) -> Result<proto::ExecuteResult, C::Error> {
        self.inner.flush().await?;
        tracing::debug!("flushed");
        loop {
            let res = super::read_message(&mut self.inner).await;

            let response: proto::Response = match res {
                Ok(Ok(msg)) => msg,
                Ok(Err(e)) => {
                    tracing::error!("recv_execute: {e:?}");
                    continue;
                }
                Err(ReadExactError::UnexpectedEof) => break,
                Err(ReadExactError::Other(e)) => {
                    return Err(e);
                }
            };

            if response.request_id == request_id {
                match response.kind {
                    proto::ResponseKind::Execute(result) => {
                        return Ok(result);
                    }
                    _ => {
                        // Unexpected response kind while waiting for execute; discard.
                    }
                }
            }
        }
        panic!(
            "recv_execute: connection closed before receiving response for request_id {}",
            request_id
        );
    }

    /// Send a pull_schema request and receive the schema result.
    pub async fn pull_schema(&mut self) -> Result<proto::SchemaResult, C::Error> {
        let id = self.send_request(proto::RequestKind::PullSchema).await?;
        self.inner.flush().await?;
        loop {
            let res = super::read_message(&mut self.inner).await;

            let response: proto::Response = match res {
                Ok(Ok(msg)) => msg,
                Ok(Err(e)) => {
                    tracing::error!("pull_schema: {e:?}");
                    continue;
                }
                Err(ReadExactError::UnexpectedEof) => {
                    panic!("pull_schema: connection closed before receiving schema response");
                }
                Err(ReadExactError::Other(e)) => {
                    return Err(e);
                }
            };

            if response.request_id == id {
                match response.kind {
                    proto::ResponseKind::Schema(result) => {
                        return Ok(result);
                    }
                    _ => {
                        // Unexpected response kind while waiting for schema; discard.
                    }
                }
            }
        }
    }

    pub fn destruct(self) -> C {
        self.inner
    }
}
