use core::ops::DerefMut;
use std::io;
use std::marker::Unpin;

use lutra_bin::Encode;
use tokio::io::{AsyncRead, AsyncWrite, AsyncWriteExt};

use crate::Run;
use crate::binary::messages;

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

    pub async fn shutdown_stream(&self) -> io::Result<()> {
        self.stream.lock().await.shutdown().await
    }

    /// Prepare, execute and release.
    pub async fn run_once(
        &self,
        program: &lutra_bin::rr::Program,
        input: &[u8],
    ) -> io::Result<messages::Result> {
        let program_id = self.send_prepare(program).await?;
        let request_id = self.send_execute(program_id, input).await?;
        self.send_release(program_id).await?;

        self.recv_response(request_id).await
    }

    pub async fn send_prepare(&self, program: &lutra_bin::rr::Program) -> io::Result<u32> {
        let program_id = self
            .next_program_id
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        let program_buf = program.encode();

        let prepare = messages::ClientMessage::Prepare(messages::Prepare {
            program_id,
            program: program_buf.to_vec(),
        });

        let mut stream = self.stream.lock().await;
        super::write_message(stream.deref_mut(), prepare).await?;
        Ok(program_id)
    }

    pub async fn send_execute(&self, program_id: u32, input: &[u8]) -> io::Result<u32> {
        let request_id: u32 = self
            .next_request_id
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        let execute = messages::ClientMessage::Execute(messages::Execute {
            program_id,
            input: input.to_vec(), // TODO: I hate this clone
            request_id,
        });

        let mut stream = self.stream.lock().await;
        super::write_message(stream.deref_mut(), execute).await?;
        Ok(request_id)
    }

    pub async fn send_release(&self, program_id: u32) -> Result<(), io::Error> {
        let release = messages::ClientMessage::Release(messages::Release { program_id });

        let mut stream = self.stream.lock().await;
        super::write_message(stream.deref_mut(), release).await
    }

    pub async fn recv_response(&self, request_id: u32) -> io::Result<messages::Result> {
        let message = {
            let mut stream = self.stream.lock().await;
            super::read_message(stream.deref_mut()).await
        };

        match message {
            Ok(messages::ServerMessage::Response(response)) => {
                // TODO: what happens if responses are not in the same order as requests?
                assert!(response.request_id == request_id);

                Ok(response.result)
            }
            Ok(messages::ServerMessage::SchemaResponse(_)) => {
                Err(io::Error::other("unexpected schema response"))
            }
            Err(e) => Err(e),
        }
    }

    pub async fn pull_schema(&self) -> io::Result<std::string::String> {
        let mut stream = self.stream.lock().await;
        super::write_message(stream.deref_mut(), messages::ClientMessage::PullSchema).await?;
        stream.flush().await?;
        let message = super::read_message(stream.deref_mut()).await?;
        match message {
            messages::ServerMessage::SchemaResponse(response) => match response.result {
                messages::SchemaResult::Ok(schema) => Ok(schema),
                messages::SchemaResult::Err(e) => Err(io::Error::other(e.message)),
            },
            messages::ServerMessage::Response(_) => {
                Err(io::Error::other("unexpected execute response"))
            }
        }
    }
}

impl<C> Run for Client<C>
where
    C: AsyncRead + AsyncWrite + Unpin,
{
    type Prepared = u32;
    type Error = io::Error;

    async fn prepare(
        &self,
        program: lutra_bin::rr::Program,
    ) -> Result<Self::Prepared, Self::Error> {
        self.send_prepare(&program).await
    }

    async fn execute(
        &self,
        program: &Self::Prepared,
        input: &[u8],
    ) -> Result<std::vec::Vec<u8>, Self::Error> {
        let request_id = self.send_execute(*program, input).await?;
        err_to_io(self.recv_response(request_id).await?)
    }

    async fn pull_schema(&self) -> Result<std::string::String, Self::Error> {
        self.pull_schema().await
    }

    async fn release(&self, prepared: Self::Prepared) -> Result<(), Self::Error> {
        self.send_release(prepared).await
    }

    fn shutdown(&self) -> impl Future<Output = Result<(), Self::Error>> {
        self.shutdown_stream()
    }
}

fn err_to_io(res: messages::Result) -> Result<Vec<u8>, io::Error> {
    match res {
        messages::Result::Ok(output) => Ok(output),
        messages::Result::Err(err) => {
            let msg = match &err.code {
                Some(code) => format!("{}: {}", code, err.message),
                None => err.message.clone(),
            };
            Err(io::Error::other(msg))
        }
    }
}
