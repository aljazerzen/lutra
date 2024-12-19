use std::{collections::HashMap, marker::Unpin};

use lutra_bin::{br, Decode, Encode};
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt};

use crate::messages;

pub struct Server<R, W>
where
    R: AsyncRead + Unpin,
    W: AsyncWrite + Unpin,
{
    rx: R,
    tx: W,
    prepared_programs: HashMap<u32, br::Program>,
}

impl<R, W> Server<R, W>
where
    R: AsyncRead + Unpin,
    W: AsyncWrite + Unpin,
{
    pub fn new(rx: R, tx: W) -> Self {
        Self {
            rx,
            tx,
            prepared_programs: Default::default(),
        }
    }

    pub async fn run(&mut self) {
        let mut up_buffer = [0; 1024];
        loop {
            let bytes_read = self.rx.read(&mut up_buffer).await.unwrap();
            if bytes_read == 0 {
                break;
            }

            let mut reader = lutra_bin::Reader::new(&up_buffer[0..bytes_read]);
            while reader.remaining() > 0 {
                let message = messages::ClientMessage::decode(&mut reader).unwrap();
                reader.skip_read();

                self.handle_message(message).await;
            }
        }
    }

    async fn handle_message(&mut self, message: messages::ClientMessage) {
        match message {
            messages::ClientMessage::Prepare(prepare) => {
                let program = lutra_bin::br::Program::decode_buffer(&prepare.program).unwrap();

                self.prepared_programs.insert(prepare.program_id, program);
                // maybe send some kind of successful prepare response?
            }
            messages::ClientMessage::Execute(execute) => {
                let program = self.prepared_programs.get(&execute.program_id).unwrap();

                let result = lutra_runtime::evaluate(
                    program,
                    execute.inputs,
                    lutra_runtime::BUILTIN_MODULES,
                );

                let response = messages::ServerMessage::Response(messages::Response {
                    request_id: execute.request_id,
                    result: messages::Result::Ok(result),
                });

                let mut response_buf = Vec::new();
                response.encode(&mut response_buf).unwrap();

                self.tx.write_all(&response_buf).await.unwrap();
            }
            messages::ClientMessage::Release(release) => {
                self.prepared_programs.remove(&release.program_id);
                // maybe send some kind of successful release response?
            }
        }
    }
}
