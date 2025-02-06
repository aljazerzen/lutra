use std::{collections::HashMap, marker::Unpin};

use lutra_bin::{br, Decode};
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt};

use crate::messages;

pub struct ServerConnection<R, W>
where
    R: AsyncRead + Unpin,
    W: AsyncWrite + Unpin,
{
    rx: R,
    tx: W,

    modules: Vec<(&'static str, &'static dyn lutra_runtime::NativeModule)>,

    prepared_programs: HashMap<u32, br::Program>,
}

impl<R, W> ServerConnection<R, W>
where
    R: AsyncRead + Unpin,
    W: AsyncWrite + Unpin,
{
    pub fn new(
        rx: R,
        tx: W,
        modules: Vec<(&'static str, &'static dyn lutra_runtime::NativeModule)>,
    ) -> Self {
        Self {
            rx,
            tx,
            modules,
            prepared_programs: Default::default(),
        }
    }

    pub async fn run(&mut self) {
        while let Ok(message) = read_message(&mut self.rx).await {
            self.handle_message(message).await;
        }
    }

    async fn handle_message(&mut self, message: messages::ClientMessage) {
        match message {
            messages::ClientMessage::Prepare(prepare) => {
                let program = lutra_bin::br::Program::decode(&prepare.program).unwrap();

                self.prepared_programs.insert(prepare.program_id, program);
                // maybe send some kind of successful prepare response?

                self.tx.flush().await.unwrap();
            }
            messages::ClientMessage::Execute(execute) => {
                let program = self.prepared_programs.get(&execute.program_id).unwrap();

                let result = lutra_runtime::evaluate(program, execute.inputs, &self.modules);

                let response = messages::ServerMessage::Response(messages::Response {
                    request_id: execute.request_id,
                    result: messages::Result::Ok(result),
                });

                write_message(&mut self.tx, response).await.unwrap();
            }
            messages::ClientMessage::Release(release) => {
                self.prepared_programs.remove(&release.program_id);
                // maybe send some kind of successful release response?

                self.tx.flush().await.unwrap();
            }
        }
    }
}

pub async fn write_message(
    mut tx: impl AsyncWrite + Unpin,
    e: impl lutra_bin::Encode,
) -> std::io::Result<()> {
    let mut buf = Vec::new();
    e.encode(&mut buf).unwrap();

    tx.write_all(&(buf.len() as u32).to_le_bytes()).await?;
    tx.write_all(&buf).await?;
    Ok(())
}

pub async fn read_message<D: Decode + Sized>(mut rx: impl AsyncRead + Unpin) -> std::io::Result<D> {
    let mut buf = [0; 4];
    rx.read_exact(&mut buf).await?;
    let len = u32::from_le_bytes(buf) as usize;

    let mut buf = vec![0; len];
    rx.read_exact(&mut buf).await?;

    Ok(D::decode(&buf).unwrap())
}
