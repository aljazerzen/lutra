use crate::vec;

use embedded_io_async::Read;
use embedded_io_async::ReadExactError;
use embedded_io_async::Write;

use crate::binary::messages;

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

    /// Prepare, execute and release.
    pub async fn run_once(
        &mut self,
        program: &[u8],
        input: vec::Vec<u8>,
    ) -> Result<messages::Result, C::Error> {
        let program_id = self.prepare(program).await?;
        let request_id = self.execute(program_id, input).await?;
        self.release(program_id).await?;

        self.recv_response(request_id).await
    }

    pub async fn prepare(&mut self, program: &[u8]) -> Result<u32, C::Error> {
        let program_id = self.next_program_id;
        self.next_program_id += 1;

        let prepare = messages::ClientMessage::Prepare(messages::Prepare {
            program_id,
            program: program.to_vec(),
        });

        super::write_message(&mut self.inner, prepare).await?;
        Ok(program_id)
    }

    pub async fn execute(&mut self, program_id: u32, input: vec::Vec<u8>) -> Result<u32, C::Error> {
        let request_id = self.next_request_id;
        self.next_request_id += 1;

        let execute = messages::ClientMessage::Execute(messages::Execute {
            program_id,
            input,
            request_id,
        });

        super::write_message(&mut self.inner, execute).await?;
        Ok(request_id)
    }

    pub async fn release(&mut self, program_id: u32) -> Result<(), C::Error> {
        let release = messages::ClientMessage::Release(messages::Release { program_id });
        super::write_message(&mut self.inner, release).await?;
        Ok(())
    }

    pub async fn recv_response(&mut self, request_id: u32) -> Result<messages::Result, C::Error> {
        self.inner.flush().await?;
        tracing::debug!("flushed");
        loop {
            let res = super::read_message(&mut self.inner).await;

            let message = match res {
                Ok(Ok(message)) => message,
                Ok(Err(lutra_err)) => {
                    tracing::error!("recv_response: {lutra_err:?}");
                    continue;
                }
                Err(ReadExactError::UnexpectedEof) => break,
                Err(ReadExactError::Other(e)) => {
                    return Err(e);
                }
            };

            match message {
                messages::ServerMessage::Response(response) => {
                    if response.request_id == request_id {
                        return Ok(response.result);
                    }
                }
            }
        }
        panic!(
            "recv_response: connection closed before receiving response for request_id {}",
            request_id
        );
    }

    pub fn destruct(self) -> C {
        self.inner
    }
}
