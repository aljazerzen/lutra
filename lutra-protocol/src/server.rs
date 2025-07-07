use std::{collections::HashMap, marker::Unpin};

use lutra_bin::{Decode, br};
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt};

use crate::messages;

pub struct ServerConnection<C>
where
    C: AsyncRead + AsyncWrite + Unpin,
{
    inner: C,

    modules: Vec<(&'static str, &'static dyn lutra_runtime::NativeModule)>,

    prepared_programs: HashMap<u32, br::Program>,
}

impl<C> ServerConnection<C>
where
    C: AsyncRead + AsyncWrite + Unpin,
{
    pub fn new(
        inner: C,
        modules: Vec<(&'static str, &'static dyn lutra_runtime::NativeModule)>,
    ) -> Self {
        Self {
            inner,
            modules,
            prepared_programs: Default::default(),
        }
    }

    pub async fn run(&mut self) {
        while let Ok(message) = read_message(&mut self.inner).await {
            self.handle_message(message).await;
        }
    }

    async fn handle_message(&mut self, message: messages::ClientMessage) {
        match message {
            messages::ClientMessage::Prepare(prepare) => {
                let program = lutra_bin::br::Program::decode(&prepare.program).unwrap();
                log::trace!("prepare");

                self.prepared_programs.insert(prepare.program_id, program);
                // maybe send some kind of successful prepare response?

                self.inner.flush().await.unwrap();
            }
            messages::ClientMessage::Execute(execute) => {
                let program = self.prepared_programs.get(&execute.program_id).unwrap();
                log::trace!("execute");

                let result =
                    lutra_runtime::evaluate(program, execute.input, &self.modules).unwrap();

                let response = messages::ServerMessage::Response(messages::Response {
                    request_id: execute.request_id,
                    result: messages::Result::Ok(result),
                });

                write_message(&mut self.inner, response).await.unwrap();
            }
            messages::ClientMessage::Release(release) => {
                self.prepared_programs.remove(&release.program_id);
                log::trace!("release");
                // maybe send some kind of successful release response?

                self.inner.flush().await.unwrap();
            }
        }
    }
}

pub async fn write_message(
    mut tx: impl AsyncWrite + Unpin,
    e: impl lutra_bin::Encode,
) -> std::io::Result<()> {
    log::trace!("write message");

    let mut buf = lutra_bin::bytes::BytesMut::new();
    e.encode(&mut buf);

    log::trace!(".. len = {}", buf.len());
    tx.write_all(&(buf.len() as u32).to_le_bytes()).await?;
    log::trace!(".. contents");
    tx.write_all(&buf).await?;
    log::trace!("done");
    Ok(())
}

pub async fn read_message<D: Decode + Sized>(mut rx: impl AsyncRead + Unpin) -> std::io::Result<D> {
    log::trace!("read message");

    let mut buf = [0; 4];
    rx.read_exact(&mut buf).await?;
    let len = u32::from_le_bytes(buf) as usize;

    log::trace!(".. len = {len}");
    let mut buf = vec![0; len];
    rx.read_exact(&mut buf).await?;

    log::trace!(".. decode");
    let r = D::decode(&buf).unwrap();

    log::trace!(".. done");
    Ok(r)
}
