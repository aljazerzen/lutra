use std::marker::Unpin;

use lutra_bin::{br, Encode};
use tokio::io::{AsyncRead, AsyncWrite, AsyncWriteExt};

use crate::messages;
use crate::server::{read_message, write_message};

pub struct ClientConnection<R, W>
where
    R: AsyncRead + Unpin,
    W: AsyncWrite + Unpin,
{
    rx: R,
    tx: W,
    next_program_id: u32,
    next_request_id: u32,
}

impl<R, W> ClientConnection<R, W>
where
    R: AsyncRead + Unpin,
    W: AsyncWrite + Unpin,
{
    pub fn new(rx: R, tx: W) -> Self {
        ClientConnection {
            rx,
            tx,
            next_program_id: 0,
            next_request_id: 0,
        }
    }

    pub async fn shutdown(&mut self) -> std::io::Result<()> {
        self.tx.shutdown().await
    }

    /// Prepare, execute and release.
    pub async fn run_once(
        &mut self,
        program: &br::Program,
        inputs: Vec<Vec<u8>>,
    ) -> messages::Result {
        let program_id = self.prepare(program).await;
        let request_id = self.execute(program_id, inputs).await;
        self.release(program_id).await;

        self.recv_response(request_id).await
    }

    pub async fn prepare(&mut self, program: &br::Program) -> u32 {
        let program_id = self.next_program_id;
        self.next_program_id += 1;

        let mut program_buf = Vec::new();
        program.encode(&mut program_buf).unwrap();

        let prepare = messages::ClientMessage::Prepare(messages::Prepare {
            program_id,
            program: program_buf,
        });

        write_message(&mut self.tx, prepare).await.unwrap();
        program_id
    }

    pub async fn execute(&mut self, program_id: u32, inputs: Vec<Vec<u8>>) -> u32 {
        let request_id = self.next_request_id;
        self.next_request_id += 1;

        let execute = messages::ClientMessage::Execute(messages::Execute {
            program_id,
            inputs,
            request_id,
        });

        write_message(&mut self.tx, execute).await.unwrap();
        request_id
    }

    pub async fn release(&mut self, program_id: u32) {
        let release = messages::ClientMessage::Release(messages::Release { program_id });
        write_message(&mut self.tx, release).await.unwrap();
    }

    pub async fn recv_response(&mut self, request_id: u32) -> messages::Result {
        while let Ok(message) = read_message(&mut self.rx).await {
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
