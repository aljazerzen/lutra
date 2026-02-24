//! Channel-based adapter for Run runners.
//!
//! This module provides a channel-based adapter that enables
//! communication between threads using typed message passing instead of
//! serialized byte streams.
//!
//! # Example
//!
//! ```ignore
//! use lutra_runner::channel;
//! use lutra_interpreter::InterpreterRunner;
//!
//! // Create runner and spawn server thread
//! let runner = InterpreterRunner::default();
//! let (client, server) = channel::new_pair(runner);
//!
//! std::thread::spawn(move || {
//!     server.run();
//! });
//!
//! // Execute program (in production you would have actual program and input)
//! // let output = client.run_once(&program, &input)?;
//!
//! // Cleanup
//! drop(client);
//! ```

mod client;
mod server;

pub use client::{Client, ClientReceiver, ClientSender};
pub use server::Server;

use crate::{Run, proto};
use tokio::sync::mpsc;

/// Create connected client/server pair for Run runners.
///
/// # Example
///
/// ```ignore
/// use lutra_runner::channel;
///
/// let runner = ...;
/// let (client, server) = channel::new_pair(runner);
///
/// // Spawn server task inside a tokio runtime
/// tokio::spawn(async move { server.run().await });
///
/// // Cleanup
/// drop(client);
/// ```
pub fn new_pair<R>(runner: R) -> (Client, Server<R>)
where
    R: Run + Send + 'static,
{
    let (client_tx, server_rx): (mpsc::Sender<proto::Request>, mpsc::Receiver<proto::Request>) =
        mpsc::channel(1);
    let (server_tx, client_rx): (
        mpsc::Sender<proto::Response>,
        mpsc::Receiver<proto::Response>,
    ) = mpsc::channel(1);

    let client = Client::new(client_tx, client_rx);
    let server = Server::new(server_rx, server_tx, runner);

    (client, server)
}
