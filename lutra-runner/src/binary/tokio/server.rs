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

    prepared_programs: HashMap<u32, Result<R::Prepared, messages::Error>>,
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
                tracing::trace!("prepare");

                let result = match lutra_bin::rr::Program::decode(&prepare.program) {
                    Ok(program) => {
                        self.runner
                            .prepare(program)
                            .await
                            .map_err(|e| messages::Error {
                                message: format!("{:?}", e),
                                code: Some(crate::error_codes::PREPARE_ERROR.to_string()),
                            })
                    }
                    Err(e) => Err(messages::Error {
                        message: format!("{:?}", e),
                        code: Some(crate::error_codes::DECODE_ERROR.to_string()),
                    }),
                };

                self.prepared_programs.insert(prepare.program_id, result);
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
        match self.prepared_programs.get(&program_id) {
            Some(Ok(program)) => match self.runner.execute(program, input).await {
                Ok(output) => messages::Result::Ok(output),
                Err(e) => messages::Result::Err(messages::Error {
                    message: format!("{:?}", e),
                    code: Some(crate::error_codes::EXECUTION_ERROR.to_string()),
                }),
            },
            Some(Err(err)) => messages::Result::Err(err.clone()),
            None => messages::Result::Err(messages::Error {
                message: format!("Program {} not prepared", program_id),
                code: Some(crate::error_codes::PROGRAM_NOT_FOUND.to_string()),
            }),
        }
    }
}
