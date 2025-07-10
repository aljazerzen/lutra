use std::marker::Unpin;

use lutra_bin::Encode;
use lutra_bin::br;
use tokio::io::{AsyncRead, AsyncWrite, AsyncWriteExt};

use crate::messages;
use crate::server::{read_message, write_message};

pub struct ClientConnection<C>
where
    C: AsyncRead + AsyncWrite + Unpin,
{
    inner: C,
    next_program_id: u32,
    next_request_id: u32,
}

impl<C> ClientConnection<C>
where
    C: AsyncRead + AsyncWrite + Unpin,
{
    pub fn new(inner: C) -> Self {
        ClientConnection {
            inner,
            next_program_id: 0,
            next_request_id: 0,
        }
    }

    pub async fn shutdown(&mut self) -> std::io::Result<()> {
        self.inner.shutdown().await
    }

    /// Prepare, execute and release.
    pub async fn run_once(&mut self, program: &br::Program, input: Vec<u8>) -> messages::Result {
        let program_id = self.prepare(program).await;
        let request_id = self.execute(program_id, input).await;
        self.release(program_id).await;

        self.recv_response(request_id).await
    }

    pub async fn prepare(&mut self, program: &br::Program) -> u32 {
        let program_id = self.next_program_id;
        self.next_program_id += 1;

        let mut program_buf = lutra_bin::bytes::BytesMut::new();
        program.encode(&mut program_buf);

        let prepare = messages::ClientMessage::Prepare(messages::Prepare {
            program_id,
            program: program_buf.to_vec(),
        });

        write_message(&mut self.inner, prepare).await.unwrap();
        program_id
    }

    pub async fn execute(&mut self, program_id: u32, input: Vec<u8>) -> u32 {
        let request_id = self.next_request_id;
        self.next_request_id += 1;

        let execute = messages::ClientMessage::Execute(messages::Execute {
            program_id,
            input,
            request_id,
        });

        write_message(&mut self.inner, execute).await.unwrap();
        request_id
    }

    pub async fn release(&mut self, program_id: u32) {
        let release = messages::ClientMessage::Release(messages::Release { program_id });
        write_message(&mut self.inner, release).await.unwrap();
    }

    pub async fn recv_response(&mut self, request_id: u32) -> messages::Result {
        while let Ok(message) = read_message(&mut self.inner).await {
            // TODO: error handling

            match message {
                messages::ServerMessage::Response(response) => {
                    if response.request_id == request_id {
                        return response.result;
                    }
                }
            }

            // TODO: don't discard non-matching messages or message left in buffer
        }
        panic!();
    }
}
