use std::io::{self, ErrorKind};
use std::sync::Arc;
use std::sync::atomic::{AtomicU32, Ordering};
use tokio::sync::mpsc::{Receiver, Sender};

use lutra_bin::{Encode, rr, vec};

use crate::RunSync;
use crate::binary::messages;

/// Sender half of the client - can be cloned and shared across threads.
///
/// Manages atomic counters for program_id and request_id generation.
#[derive(Clone)]
pub struct ClientSender {
    tx: Sender<messages::ClientMessage>,
    next_program_id: Arc<AtomicU32>,
    next_request_id: Arc<AtomicU32>,
}

/// Receiver half of the client - must have unique ownership.
pub struct ClientReceiver {
    rx: Receiver<messages::ServerMessage>,
}

/// Client for sending commands to a runner over channels.
///
/// The Client uses typed message passing instead of byte serialization,
/// making it efficient for in-process communication.
///
/// # Thread Safety
///
/// Client can be split into sender and receiver halves:
///
/// ```ignore
/// use lutra_runner::channel;
/// use lutra_interpreter::InterpreterRunner;
///
/// let runner = InterpreterRunner::default();
/// let (client, server) = channel::new_pair(runner);
/// let (sender, receiver) = client.split();
///
/// // Sender can be cloned across multiple components
/// let sender1 = sender.clone();
/// let sender2 = sender.clone();
///
/// // Receiver goes to dedicated message thread
/// std::thread::spawn(move || {
///     while let Ok(msg) = receiver.recv() {
///         // Handle message
///     }
/// });
/// drop(server);
/// ```
pub struct Client {
    sender: ClientSender,
    receiver: ClientReceiver,
}

impl ClientSender {
    /// Create a new sender with the given channel.
    pub fn new(tx: Sender<messages::ClientMessage>) -> Self {
        Self {
            tx,
            next_program_id: Arc::new(AtomicU32::new(0)),
            next_request_id: Arc::new(AtomicU32::new(0)),
        }
    }

    /// Send prepare command and return program_id.
    ///
    /// The program is encoded and sent to the server for preparation.
    /// The server will cache the prepared program for future executions.
    pub fn prepare(&self, program: &rr::Program) -> io::Result<u32> {
        let program_id = self.next_program_id.fetch_add(1, Ordering::Relaxed);

        let msg = messages::ClientMessage::Prepare(messages::Prepare {
            program_id,
            program: program.encode(),
        });

        self.tx
            .blocking_send(msg)
            .map_err(|_| io::Error::new(ErrorKind::BrokenPipe, "runner disconnected"))?;

        Ok(program_id)
    }

    /// Send execute command and return request_id.
    ///
    /// The program must have been prepared first using `prepare()`.
    pub fn execute(&self, program_id: u32, input: &[u8]) -> io::Result<u32> {
        // Note: request_id wraps after 2^32 requests (4 billion - not a practical concern)
        let request_id = self.next_request_id.fetch_add(1, Ordering::Relaxed);

        let msg = messages::ClientMessage::Execute(messages::Execute {
            program_id,
            request_id,
            input: input.to_vec(),
        });

        self.tx
            .blocking_send(msg)
            .map_err(|_| io::Error::new(ErrorKind::BrokenPipe, "runner disconnected"))?;

        Ok(request_id)
    }

    /// Send release command to free a prepared program.
    ///
    /// This removes the program from the server's cache.
    pub fn release(&self, program_id: u32) -> io::Result<()> {
        let msg = messages::ClientMessage::Release(messages::Release { program_id });

        self.tx
            .blocking_send(msg)
            .map_err(|_| io::Error::new(ErrorKind::BrokenPipe, "runner disconnected"))?;

        Ok(())
    }

    /// Send pull_schema command to request the database schema.
    pub fn pull_schema(&self) -> io::Result<()> {
        let msg = messages::ClientMessage::PullSchema;

        self.tx
            .blocking_send(msg)
            .map_err(|_| io::Error::new(ErrorKind::BrokenPipe, "runner disconnected"))?;

        Ok(())
    }
}

impl ClientReceiver {
    /// Create a new receiver with the given channel.
    pub fn new(rx: Receiver<messages::ServerMessage>) -> Self {
        Self { rx }
    }

    /// Receive next server message.
    pub async fn recv(&mut self) -> Option<messages::ServerMessage> {
        self.rx.recv().await
    }

    /// Blocking receive next server message.
    pub fn blocking_recv(&mut self) -> Option<messages::ServerMessage> {
        self.rx.blocking_recv()
    }

    /// Non-blocking receive of next server message.
    pub fn try_recv(
        &mut self,
    ) -> Result<messages::ServerMessage, tokio::sync::mpsc::error::TryRecvError> {
        self.rx.try_recv()
    }

    /// Block waiting for response with matching request_id.
    ///
    /// This method loops until it receives a response with the correct request_id.
    /// If multiple requests are in flight, responses for other requests are discarded.
    pub fn blocking_recv_response(&mut self, request_id: u32) -> io::Result<messages::Result> {
        loop {
            let msg = self
                .blocking_recv()
                .ok_or_else(|| io::Error::new(ErrorKind::BrokenPipe, "runner disconnected"))?;

            match msg {
                messages::ServerMessage::Response(resp) => {
                    if resp.request_id == request_id {
                        return Ok(resp.result);
                    }
                    // Wrong request_id, keep waiting
                    // (happens if multiple requests in flight)
                }
                messages::ServerMessage::SchemaResponse(_) => {
                    // Unexpected schema response while waiting for an execute response; discard.
                }
            }
        }
    }

    /// Block waiting for a schema response.
    pub fn blocking_recv_schema(&mut self) -> io::Result<messages::SchemaResult> {
        loop {
            let msg = self
                .blocking_recv()
                .ok_or_else(|| io::Error::new(ErrorKind::BrokenPipe, "runner disconnected"))?;

            match msg {
                messages::ServerMessage::SchemaResponse(resp) => {
                    return Ok(resp.result);
                }
                messages::ServerMessage::Response(_) => {
                    // Unexpected execute response while waiting for schema; discard.
                }
            }
        }
    }
}

impl Client {
    /// Create a new client with the given channels.
    ///
    /// Typically you should use `channel_pair()` instead of constructing directly.
    pub fn new(tx: Sender<messages::ClientMessage>, rx: Receiver<messages::ServerMessage>) -> Self {
        Self {
            sender: ClientSender::new(tx),
            receiver: ClientReceiver::new(rx),
        }
    }

    /// Split the client into sender and receiver halves.
    ///
    /// The sender can be cloned and shared across multiple threads/components.
    /// The receiver must have unique ownership (typically for a dedicated message thread).
    pub fn split(self) -> (ClientSender, ClientReceiver) {
        (self.sender, self.receiver)
    }
}

/// Implementation of `RunSync` for the channel client.
///
/// This allows the channel client to be used anywhere a `RunSync` runner is expected,
/// enabling uniform handling of both direct runners (like the interpreter or DuckDB)
/// and channel-based runners.
///
/// # Example
///
/// ```ignore
/// use lutra_runner::{RunSync, channel};
/// use lutra_interpreter::InterpreterRunner;
///
/// let runner = InterpreterRunner::default();
/// let (mut client, server) = channel::new_pair(runner);
///
/// // Spawn server thread
/// std::thread::spawn(move || {
///     server.run();
/// });
///
/// // Use client through RunSync trait
/// let prepared = client.prepare_sync(program)?;
/// let output = client.execute_sync(&prepared, &input)?;
/// ```
impl RunSync for Client {
    type Error = io::Error;
    type Prepared = PreparedHandle;

    fn prepare_sync(&mut self, program: rr::Program) -> Result<Self::Prepared, Self::Error> {
        let program_id = self.sender.prepare(&program)?;
        Ok(PreparedHandle { program_id })
    }

    fn execute_sync(
        &mut self,
        prepared: &Self::Prepared,
        input: &[u8],
    ) -> Result<vec::Vec<u8>, Self::Error> {
        let request_id = self.sender.execute(prepared.program_id, input)?;
        let result = self.receiver.blocking_recv_response(request_id)?;

        match result {
            messages::Result::Ok(output) => Ok(output.into()),
            messages::Result::Err(err) => {
                let msg = match &err.code {
                    Some(code) => format!("{}: {}", code, err.message),
                    None => err.message.clone(),
                };
                Err(io::Error::other(msg))
            }
        }
    }

    fn pull_schema_sync(&mut self) -> Result<lutra_bin::string::String, Self::Error> {
        self.sender.pull_schema()?;
        match self.receiver.blocking_recv_schema()? {
            messages::SchemaResult::Ok(schema) => Ok(schema),
            messages::SchemaResult::Err(e) => Err(io::Error::other(e.message)),
        }
    }

    fn release_sync(&mut self, prepared: Self::Prepared) -> Result<(), Self::Error> {
        self.sender.release(prepared.program_id)
    }

    fn shutdown_sync(&mut self) -> Result<(), Self::Error> {
        // Client shutdown is handled by dropping the sender
        Ok(())
    }
}

/// Prepared program handle for the channel client.
///
/// Stores both the program_id for communication and the program itself
/// to allow releasing it when the handle is dropped.
pub struct PreparedHandle {
    program_id: u32,
}
