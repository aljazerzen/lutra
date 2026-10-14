use crate::{proto, vec};
use embedded_io_async::{Read, Write};
use lutra_bin::{Decode, Encode};
use lutra_bin_stream::{
    Compression,
    r#async::{Reader, Writer},
};

pub struct Client<C> {
    inner: C,
    reader: Reader,
    writer: Option<Writer>,
    next_program_id: u32,
    next_request_id: u32,
    pending: vec::Vec<u32>,
    responses: vec::Vec<proto::Response>,
    busy: bool,
    closed: bool,
}

impl<C: Read + Write + Unpin> Client<C> {
    pub fn new(inner: C) -> Self {
        Self {
            inner,
            reader: Reader::new(),
            writer: None,
            next_program_id: 0,
            next_request_id: 0,
            pending: vec::Vec::new(),
            responses: vec::Vec::new(),
            busy: false,
            closed: false,
        }
    }

    pub async fn run_once(
        &mut self,
        program: &[u8],
        input: vec::Vec<u8>,
    ) -> Result<proto::ExecuteResult, Error<C::Error>> {
        let program_id = self.prepare(program).await?;
        let request_id = self.execute(program_id, input).await?;
        // recv_execute must happen before release: the server sends responses in
        // request order, so the execute response sits in the stream ahead of the
        // release response and would be silently consumed by `release()`.
        let result = self.recv_execute(request_id).await?;
        self.release(program_id).await?;
        Ok(result)
    }

    pub async fn prepare(&mut self, program: &[u8]) -> Result<u32, Error<C::Error>> {
        let program_id = self.next_program_id;
        self.next_program_id = program_id.wrapping_add(1);
        let id = self
            .send_request(proto::RequestKind::Prepare(proto::PrepareRequest {
                program_id,
                program: program.to_vec(),
            }))
            .await?;
        match self.recv_for(id).await? {
            proto::ResponseKind::Prepare(result) => match result.0 {
                Some(e) => Err(Error::Protocol(e)),
                None => Ok(program_id),
            },
            _ => self.unexpected(),
        }
    }

    pub async fn execute(
        &mut self,
        program_id: u32,
        input: vec::Vec<u8>,
    ) -> Result<u32, Error<C::Error>> {
        self.send_request(proto::RequestKind::Execute(proto::ExecuteRequest {
            program_id,
            input,
        }))
        .await
    }

    pub async fn release(&mut self, program_id: u32) -> Result<(), Error<C::Error>> {
        let id = self
            .send_request(proto::RequestKind::Release(proto::ReleaseRequest {
                program_id,
            }))
            .await?;
        match self.recv_for(id).await? {
            proto::ResponseKind::Release(result) => match result.0 {
                Some(e) => Err(Error::Protocol(e)),
                None => Ok(()),
            },
            _ => self.unexpected(),
        }
    }

    pub async fn recv_execute(
        &mut self,
        request_id: u32,
    ) -> Result<proto::ExecuteResult, Error<C::Error>> {
        match self.recv_for(request_id).await? {
            proto::ResponseKind::Execute(result) => Ok(result),
            _ => self.unexpected(),
        }
    }

    pub async fn pull_schema(&mut self) -> Result<proto::SchemaResult, Error<C::Error>> {
        let id = self.send_request(proto::RequestKind::PullSchema).await?;
        match self.recv_for(id).await? {
            proto::ResponseKind::Schema(result) => Ok(result),
            _ => self.unexpected(),
        }
    }

    /// Complete all pending execute calls before shutting down.
    pub async fn shutdown(&mut self) -> Result<(), Error<C::Error>> {
        if self.closed {
            return Ok(());
        }
        self.check_ready()?;
        if !self.pending.is_empty() {
            return Err(Error::Session(
                "pending responses must be received before shutdown",
            ));
        }
        self.busy = true;
        self.init_writer()?;
        if let Some(writer) = &mut self.writer {
            writer
                .finish(&mut self.inner)
                .await
                .map_err(Error::Framing)?;
        }
        if self
            .reader
            .read_frame(&mut self.inner)
            .await
            .map_err(Error::Framing)?
            .is_some()
        {
            return Err(Error::Session("unexpected response while shutting down"));
        }
        self.closed = true;
        self.busy = false;
        Ok(())
    }

    pub fn destruct(self) -> C {
        self.inner
    }

    fn check_ready(&self) -> Result<(), Error<C::Error>> {
        if self.busy || self.closed {
            Err(Error::Session(
                "session closed or poisoned by an interrupted operation",
            ))
        } else {
            Ok(())
        }
    }

    fn init_writer(&mut self) -> Result<(), Error<C::Error>> {
        if self.writer.is_none() {
            self.writer = Some(
                Writer::new(Compression::None)
                    .map_err(|e| Error::Framing(lutra_bin_stream::r#async::Error::Stream(e)))?,
            );
        }
        Ok(())
    }

    async fn send_request(&mut self, kind: proto::RequestKind) -> Result<u32, Error<C::Error>> {
        self.check_ready()?;
        self.busy = true;
        self.init_writer()?;
        let id = self.next_request_id;
        self.next_request_id = id.wrapping_add(1);
        if self.pending.contains(&id) {
            return Err(Error::Session("request ID exhausted"));
        }
        if let Some(writer) = &mut self.writer {
            writer
                .write_frame(&mut self.inner, &proto::Request { id, kind }.encode())
                .await
                .map_err(Error::Framing)?;
            writer
                .flush(&mut self.inner)
                .await
                .map_err(Error::Framing)?;
        }
        self.pending.push(id);
        self.busy = false;
        Ok(id)
    }

    async fn recv_for(&mut self, id: u32) -> Result<proto::ResponseKind, Error<C::Error>> {
        self.check_ready()?;
        let Some(pending_index) = self.pending.iter().position(|v| *v == id) else {
            return Err(Error::Session("unknown pending request ID"));
        };
        self.busy = true;
        loop {
            let response =
                if let Some(index) = self.responses.iter().position(|r| r.request_id == id) {
                    self.responses.swap_remove(index)
                } else {
                    let frame = self
                        .reader
                        .read_frame(&mut self.inner)
                        .await
                        .map_err(Error::Framing)?
                        .ok_or(Error::Session("response stream ended before response"))?;
                    proto::Response::decode(&frame).map_err(Error::Decode)?
                };
            if response.request_id == id {
                self.pending.swap_remove(pending_index);
                self.busy = false;
                return Ok(response.kind);
            }
            if !self.pending.contains(&response.request_id)
                || self
                    .responses
                    .iter()
                    .any(|r| r.request_id == response.request_id)
            {
                return Err(Error::Session(
                    "unexpected or duplicate response request ID",
                ));
            }
            self.responses.push(response);
        }
    }

    fn unexpected<T>(&mut self) -> Result<T, Error<C::Error>> {
        self.busy = true;
        Err(Error::Session("unexpected response kind"))
    }
}

#[derive(Debug)]
pub enum Error<E> {
    Framing(lutra_bin_stream::r#async::Error<E>),
    Decode(lutra_bin::Error),
    Protocol(proto::Error),
    Session(&'static str),
}

impl<E: core::fmt::Display> core::fmt::Display for Error<E> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Framing(e) => write!(f, "{e}"),
            Self::Decode(e) => write!(f, "{e}"),
            Self::Protocol(e) => write!(f, "{}", e.display),
            Self::Session(e) => f.write_str(e),
        }
    }
}
impl<E: core::error::Error> core::error::Error for Error<E> {}
