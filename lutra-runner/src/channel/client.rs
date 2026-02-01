use std::io::{self, ErrorKind};
use std::sync::Arc;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::mpsc::{Receiver, Sender};

use lutra_bin::{Encode, rr};

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
            .send(msg)
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
            .send(msg)
            .map_err(|_| io::Error::new(ErrorKind::BrokenPipe, "runner disconnected"))?;

        Ok(request_id)
    }

    /// Send release command to free a prepared program.
    ///
    /// This removes the program from the server's cache.
    pub fn release(&self, program_id: u32) -> io::Result<()> {
        let msg = messages::ClientMessage::Release(messages::Release { program_id });

        self.tx
            .send(msg)
            .map_err(|_| io::Error::new(ErrorKind::BrokenPipe, "runner disconnected"))?;

        Ok(())
    }
}

impl ClientReceiver {
    /// Create a new receiver with the given channel.
    pub fn new(rx: Receiver<messages::ServerMessage>) -> Self {
        Self { rx }
    }

    /// Blocking receive of next server message.
    pub fn recv(&self) -> io::Result<messages::ServerMessage> {
        self.rx
            .recv()
            .map_err(|_| io::Error::new(ErrorKind::BrokenPipe, "runner disconnected"))
    }

    /// Non-blocking receive of next server message.
    pub fn try_recv(&self) -> Result<messages::ServerMessage, std::sync::mpsc::TryRecvError> {
        self.rx.try_recv()
    }

    /// Block waiting for response with matching request_id.
    ///
    /// This method loops until it receives a response with the correct request_id.
    /// If multiple requests are in flight, responses for other requests are discarded.
    pub fn recv_response(&self, request_id: u32) -> io::Result<messages::Result> {
        loop {
            let msg = self.recv()?;

            match msg {
                messages::ServerMessage::Response(resp) => {
                    if resp.request_id == request_id {
                        return Ok(resp.result);
                    }
                    // Wrong request_id, keep waiting
                    // (happens if multiple requests in flight)
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

    /// Send prepare command and return program_id.
    ///
    /// The program is encoded and sent to the server for preparation.
    /// The server will cache the prepared program for future executions.
    pub fn prepare(&self, program: &rr::Program) -> io::Result<u32> {
        self.sender.prepare(program)
    }

    /// Send execute command and return request_id.
    ///
    /// The program must have been prepared first using `prepare()`.
    /// Use `recv_response()` to wait for the execution result.
    pub fn execute(&self, program_id: u32, input: &[u8]) -> io::Result<u32> {
        self.sender.execute(program_id, input)
    }

    /// Send release command to free a prepared program.
    ///
    /// This removes the program from the server's cache.
    pub fn release(&self, program_id: u32) -> io::Result<()> {
        self.sender.release(program_id)
    }

    /// Block waiting for response with matching request_id.
    ///
    /// This method loops until it receives a response with the correct request_id.
    /// If multiple requests are in flight, responses for other requests are discarded.
    pub fn recv_response(&self, request_id: u32) -> io::Result<messages::Result> {
        self.receiver.recv_response(request_id)
    }

    /// Convenience method: prepare + execute + release + wait for result.
    ///
    /// This is the simplest way to execute a program. The program is prepared,
    /// executed with the given input, and then released from the cache.
    ///
    /// # Example
    ///
    /// ```ignore
    /// use lutra_runner::channel;
    /// use lutra_interpreter::InterpreterRunner;
    ///
    /// let runner = InterpreterRunner::default();
    /// let (client, server) = channel::new_pair(runner);
    /// // In production you would have actual program and input
    /// let output = client.run_once(&program, &input)?;
    /// drop(client);
    /// drop(server);
    /// ```
    pub fn run_once(&self, program: &rr::Program, input: &[u8]) -> io::Result<Vec<u8>> {
        let program_id = self.prepare(program)?;
        let request_id = self.execute(program_id, input)?;
        let result = self.recv_response(request_id)?;
        self.release(program_id)?;

        match result {
            messages::Result::Ok(output) => Ok(output),
            messages::Result::Err(err) => {
                let msg = match &err.code {
                    Some(code) => format!("{}: {}", code, err.message),
                    None => err.message.clone(),
                };
                Err(io::Error::other(msg))
            }
        }
    }
}
