use std::sync::Arc;
use std::sync::atomic::{AtomicU32, Ordering};
use tokio::sync::mpsc::{Receiver, Sender};

use lutra_bin::{Encode, rr, vec};

use crate::RunSync;
use crate::proto;

fn disconnected() -> proto::Error {
    proto::Error {
        display: "runner disconnected".to_string(),
        code: None,
    }
}

/// Sender half of the client - can be cloned and shared across threads.
#[derive(Clone)]
pub struct ClientSender {
    tx: Sender<proto::Request>,
    next_program_id: Arc<AtomicU32>,
    next_request_id: Arc<AtomicU32>,
}

/// Receiver half of the client - must have unique ownership.
pub struct ClientReceiver {
    rx: Receiver<proto::Response>,
}

/// Client for sending commands to a runner over channels.
///
/// The Client uses typed message passing instead of byte serialization,
/// making it efficient for in-process communication.
pub struct Client {
    sender: ClientSender,
    receiver: ClientReceiver,
}

impl ClientSender {
    /// Create a new sender with the given channel.
    pub fn new(tx: Sender<proto::Request>) -> Self {
        Self {
            tx,
            next_program_id: Arc::new(AtomicU32::new(0)),
            next_request_id: Arc::new(AtomicU32::new(0)),
        }
    }

    fn next_request_id(&self) -> u32 {
        self.next_request_id.fetch_add(1, Ordering::Relaxed)
    }

    fn next_program_id(&self) -> u32 {
        self.next_program_id.fetch_add(1, Ordering::Relaxed)
    }

    /// Assign a fresh `request_id`, wrap `kind` in a `Request`, send it, and
    /// return the id for response correlation.
    fn send_request(&self, kind: proto::RequestKind) -> Result<u32, proto::Error> {
        let id = self.next_request_id();
        self.tx
            .blocking_send(proto::Request { id, kind })
            .map_err(|_| disconnected())?;
        Ok(id)
    }

    /// Send prepare request, allocate a program_id, and return `(request_id, program_id)`.
    pub fn prepare(&self, program: &rr::Program) -> Result<(u32, u32), proto::Error> {
        let program_id = self.next_program_id();
        let request_id = self.send_request(proto::RequestKind::Prepare(proto::PrepareRequest {
            program_id,
            program: program.encode(),
        }))?;
        Ok((request_id, program_id))
    }

    /// Send execute request and return request_id.
    pub fn execute(&self, program_id: u32, input: &[u8]) -> Result<u32, proto::Error> {
        self.send_request(proto::RequestKind::Execute(proto::ExecuteRequest {
            program_id,
            input: input.to_vec(),
        }))
    }

    /// Send release request and return request_id.
    pub fn release(&self, program_id: u32) -> Result<u32, proto::Error> {
        self.send_request(proto::RequestKind::Release(proto::ReleaseRequest {
            program_id,
        }))
    }

    /// Send pull_schema request and return request_id.
    pub fn pull_schema(&self) -> Result<u32, proto::Error> {
        self.send_request(proto::RequestKind::PullSchema)
    }
}

impl ClientReceiver {
    /// Create a new receiver with the given channel.
    pub fn new(rx: Receiver<proto::Response>) -> Self {
        Self { rx }
    }

    /// Receive next server response.
    pub async fn recv(&mut self) -> Option<proto::Response> {
        self.rx.recv().await
    }

    /// Blocking receive next server response.
    pub fn blocking_recv(&mut self) -> Option<proto::Response> {
        self.rx.blocking_recv()
    }

    /// Non-blocking receive of next server response.
    pub fn try_recv(&mut self) -> Result<proto::Response, tokio::sync::mpsc::error::TryRecvError> {
        self.rx.try_recv()
    }

    /// Block waiting for a response with the matching `request_id`.
    pub fn blocking_recv_for(
        &mut self,
        request_id: u32,
    ) -> Result<proto::ResponseKind, proto::Error> {
        loop {
            let resp = self.blocking_recv().ok_or_else(disconnected)?;
            if resp.request_id == request_id {
                return Ok(resp.kind);
            }
        }
    }
}

impl Client {
    /// Create a new client with the given channels.
    pub fn new(tx: Sender<proto::Request>, rx: Receiver<proto::Response>) -> Self {
        Self {
            sender: ClientSender::new(tx),
            receiver: ClientReceiver::new(rx),
        }
    }

    /// Split the client into sender and receiver halves.
    pub fn split(self) -> (ClientSender, ClientReceiver) {
        (self.sender, self.receiver)
    }
}

impl RunSync for Client {
    fn prepare_sync(&mut self, program: rr::Program) -> Result<u32, proto::Error> {
        let (request_id, program_id) = self.sender.prepare(&program)?;
        match self.receiver.blocking_recv_for(request_id)? {
            proto::ResponseKind::Prepare(result) => {
                if let Some(err) = result.0 {
                    return Err(err);
                }
                Ok(program_id)
            }
            _ => Err(disconnected()),
        }
    }

    fn execute_sync(
        &mut self,
        program_id: u32,
        input: &[u8],
    ) -> Result<vec::Vec<u8>, proto::Error> {
        let request_id = self.sender.execute(program_id, input)?;
        match self.receiver.blocking_recv_for(request_id)? {
            proto::ResponseKind::Execute(result) => match result {
                proto::ExecuteResult::Ok(output) => Ok(output),
                proto::ExecuteResult::Err(err) => Err(err),
            },
            _ => Err(disconnected()),
        }
    }

    fn pull_schema_sync(&mut self) -> Result<lutra_bin::string::String, proto::Error> {
        let request_id = self.sender.pull_schema()?;
        match self.receiver.blocking_recv_for(request_id)? {
            proto::ResponseKind::Schema(result) => match result {
                proto::SchemaResult::Ok(schema) => Ok(schema),
                proto::SchemaResult::Err(e) => Err(e),
            },
            _ => Err(disconnected()),
        }
    }

    fn release_sync(&mut self, program_id: u32) -> Result<(), proto::Error> {
        let request_id = self.sender.release(program_id)?;
        match self.receiver.blocking_recv_for(request_id)? {
            proto::ResponseKind::Release(result) => {
                if let Some(err) = result.0 {
                    return Err(err);
                }
                Ok(())
            }
            _ => Err(disconnected()),
        }
    }

    fn shutdown_sync(&mut self) -> Result<(), proto::Error> {
        Ok(())
    }
}
