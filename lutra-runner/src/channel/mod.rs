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

pub use client::{Client, ClientReceiver, ClientSender, PreparedHandle};
pub use server::Server;

use crate::Run;
use tokio::sync::mpsc;

// Re-export message types for external use
pub use crate::binary::messages;

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
/// // Use client
/// let output = client.run_once(&program, &input)?;
///
/// // Cleanup
/// drop(client);
/// ```
pub fn new_pair<R>(runner: R) -> (Client, Server<R>)
where
    R: Run + Send + 'static,
    R::Prepared: Send,
{
    let (client_tx, server_rx) = mpsc::channel(1);
    let (server_tx, client_rx) = mpsc::channel(1);

    let client = Client::new(client_tx, client_rx);
    let server = Server::new(server_rx, server_tx, runner);

    (client, server)
}
