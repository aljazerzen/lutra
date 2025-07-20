use std::{collections::HashMap, marker::Unpin};

use lutra_bin::Decode;
use tokio::io::{AsyncRead, AsyncWrite, AsyncWriteExt};

use crate::Run;
use crate::binary::messages;

pub struct Server<C, R>
where
    C: AsyncRead + AsyncWrite + Unpin,
    R: Run,
{
    stream: C,
    runner: R,

    prepared_programs: HashMap<u32, R::Prepared>,
}

impl<C, R> Server<C, R>
where
    C: AsyncRead + AsyncWrite + Unpin,
    R: Run,
{
    pub fn new(stream: C, runner: R) -> Self {
        Self {
            stream,
            runner,
            prepared_programs: Default::default(),
        }
    }

    pub async fn run(&mut self) -> Result<(), std::io::Error> {
        while let Ok(message) = super::read_message(&mut self.stream).await {
            self.handle_message(message).await?;
        }
        Ok(())
    }

    async fn handle_message(
        &mut self,
        message: messages::ClientMessage,
    ) -> Result<(), std::io::Error> {
        match message {
            messages::ClientMessage::Prepare(prepare) => {
                let program = lutra_bin::rr::Program::decode(&prepare.program).unwrap();
                tracing::trace!("prepare");

                let handle = self.runner.prepare(program).await.unwrap();

                self.prepared_programs.insert(prepare.program_id, handle);
                // maybe send some kind of successful prepare response?
            }
            messages::ClientMessage::Execute(messages::Execute {
                request_id,
                program_id,
                input,
            }) => {
                tracing::trace!("execute");

                let result = self.handle_execute(program_id, &input).await;

                super::write_message(
                    &mut self.stream,
                    messages::ServerMessage::Response(messages::Response { request_id, result }),
                )
                .await
                .unwrap();

                self.stream.flush().await?;
            }
            messages::ClientMessage::Release(release) => {
                tracing::trace!("release");

                self.prepared_programs.remove(&release.program_id);

                // maybe send some kind of successful release response?

                self.stream.flush().await?;
            }
        }
        Ok(())
    }

    async fn handle_execute(&mut self, program_id: u32, input: &[u8]) -> messages::Result {
        let Some(program) = self.prepared_programs.get(&program_id) else {
            return messages::Result::Err(messages::Error {});
        };

        let output = self.runner.execute(program, input).await.unwrap();
        messages::Result::Ok(output)
    }
}
