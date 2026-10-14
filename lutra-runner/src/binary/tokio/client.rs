use lutra_bin::{Decode, Encode};
use lutra_bin_stream::{
    Compression,
    r#async::{Reader, Writer},
    tokio::TokioIo,
};
use tokio::io::{AsyncRead, AsyncWrite, AsyncWriteExt};

use crate::{Run, proto};

pub struct Client<C> {
    session: tokio::sync::Mutex<Session<C>>,
    next_program_id: std::sync::atomic::AtomicU32,
}

impl<C: AsyncRead + AsyncWrite + Unpin> Client<C> {
    pub fn new(connection: C) -> Self {
        Self {
            session: tokio::sync::Mutex::new(Session {
                connection,
                reader: Reader::new(),
                writer: None,
                next_request_id: 0,
                in_flight: false,
                closed: false,
            }),
            next_program_id: 0.into(),
        }
    }

    pub async fn shutdown_stream(&self) -> Result<(), proto::Error> {
        let mut session = self.session.lock().await;
        if session.closed {
            return Ok(());
        }
        if session.in_flight {
            // A cancelled exchange cannot safely be resumed as another RPC.
            session.closed = true;
            session.connection.shutdown().await.map_err(error)?;
            return Err(error("session poisoned by an interrupted exchange"));
        }
        session.in_flight = true;
        let Session {
            connection,
            reader,
            writer,
            ..
        } = &mut *session;
        if writer.is_none() {
            *writer = Some(Writer::new(Compression::None).map_err(error)?);
        }
        if let Some(writer) = writer {
            writer
                .finish(&mut TokioIo(&mut *connection))
                .await
                .map_err(error)?;
        }
        if reader
            .read_frame(&mut TokioIo(&mut *connection))
            .await
            .map_err(error)?
            .is_some()
        {
            return Err(error("unexpected response while shutting down"));
        }
        connection.shutdown().await.map_err(error)?;
        session.closed = true;
        session.in_flight = false;
        Ok(())
    }

    pub async fn pull_schema(&self) -> Result<String, proto::Error> {
        match self.exchange(proto::RequestKind::PullSchema).await? {
            proto::ResponseKind::Schema(result) => result.0,
            _ => Err(error("unexpected response kind")),
        }
    }

    async fn exchange(
        &self,
        kind: proto::RequestKind,
    ) -> Result<proto::ResponseKind, proto::Error> {
        let mut session = self.session.lock().await;
        if session.closed || session.in_flight {
            return Err(error(
                "session closed or poisoned by an interrupted exchange",
            ));
        }
        session.in_flight = true;
        let id = session.next_request_id;
        session.next_request_id = id.wrapping_add(1);
        let Session {
            connection,
            reader,
            writer,
            ..
        } = &mut *session;
        if writer.is_none() {
            *writer = Some(Writer::new(Compression::None).map_err(error)?);
        }
        if let Some(writer) = writer {
            writer
                .write_frame(
                    &mut TokioIo(&mut *connection),
                    &proto::Request { id, kind }.encode(),
                )
                .await
                .map_err(error)?;
            writer
                .flush(&mut TokioIo(&mut *connection))
                .await
                .map_err(error)?;
        }
        let frame = reader
            .read_frame(&mut TokioIo(&mut *connection))
            .await
            .map_err(error)?
            .ok_or_else(|| error("response stream ended before response"))?;
        let response = proto::Response::decode(&frame).map_err(error)?;
        if response.request_id != id {
            return Err(error("unexpected response request ID"));
        }
        session.in_flight = false;
        Ok(response.kind)
    }
}

impl<C: AsyncRead + AsyncWrite + Unpin> Run for Client<C> {
    async fn prepare(&self, program: lutra_bin::rr::Program) -> Result<u32, proto::Error> {
        let program_id = self
            .next_program_id
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        match self
            .exchange(proto::RequestKind::Prepare(proto::PrepareRequest {
                program_id,
                program: program.encode().to_vec(),
            }))
            .await?
        {
            proto::ResponseKind::Prepare(result) => match result.0 {
                Some(err) => Err(err),
                None => Ok(program_id),
            },
            _ => Err(error("unexpected response kind")),
        }
    }

    async fn execute(&self, program_id: u32, input: &[u8]) -> Result<Vec<u8>, proto::Error> {
        match self
            .exchange(proto::RequestKind::Execute(proto::ExecuteRequest {
                program_id,
                input: input.to_vec(),
            }))
            .await?
        {
            proto::ResponseKind::Execute(result) => result.0,
            _ => Err(error("unexpected response kind")),
        }
    }

    async fn release(&self, program_id: u32) -> Result<(), proto::Error> {
        match self
            .exchange(proto::RequestKind::Release(proto::ReleaseRequest {
                program_id,
            }))
            .await?
        {
            proto::ResponseKind::Release(result) => match result.0 {
                Some(err) => Err(err),
                None => Ok(()),
            },
            _ => Err(error("unexpected response kind")),
        }
    }

    async fn pull_schema(&self) -> Result<String, proto::Error> {
        self.pull_schema().await
    }
    async fn shutdown(&self) -> Result<(), proto::Error> {
        self.shutdown_stream().await
    }
    async fn get_externals(&self) -> Result<Vec<String>, proto::Error> {
        match self.exchange(proto::RequestKind::GetExternals).await? {
            proto::ResponseKind::Externals(result) => result.0,
            _ => Err(error("unexpected response kind")),
        }
    }
}

struct Session<C> {
    connection: C,
    reader: Reader,
    writer: Option<Writer>,
    next_request_id: u32,
    in_flight: bool,
    closed: bool,
}

fn error(e: impl std::fmt::Display) -> proto::Error {
    proto::Error {
        display: e.to_string(),
        code: None,
    }
}
