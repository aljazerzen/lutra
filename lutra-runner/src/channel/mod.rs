//! Channel-based adapter for RunSync runners.
//!
//! This module provides a synchronous, channel-based adapter that enables
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

use crate::RunSync;
use std::sync::mpsc;

// Re-export message types for external use
pub use crate::binary::messages;

/// Create connected client/server pair for RunSync runners.
///
/// Uses unbounded channels for simplicity. In practice, execution is rate-limited
/// by user interaction and runner execution time, so unbounded is sufficient.
///
/// # Example
///
/// ```ignore
/// use lutra_runner::channel;
/// use lutra_interpreter::InterpreterRunner;
///
/// let runner = InterpreterRunner::default();
/// let (client, server) = channel::new_pair(runner);
///
/// // Spawn server thread
/// std::thread::spawn(move || {
///     server.run();
/// });
///
/// // Use client (in production you would have actual program and input)
/// // let output = client.run_once(&program, &input)?;
///
/// // Cleanup
/// drop(client);
/// ```
pub fn new_pair<R>(runner: R) -> (Client, Server<R>)
where
    R: RunSync + Send + 'static,
    R::Prepared: Send + Clone,
{
    let (client_tx, server_rx) = mpsc::channel();
    let (server_tx, client_rx) = mpsc::channel();

    let client = Client::new(client_tx, client_rx);
    let server = Server::new(server_rx, server_tx, runner);

    (client, server)
}

#[cfg(test)]
mod tests {
    use super::*;
    use lutra_bin::rr;

    // Mock sync runner for testing
    struct MockRunner;

    impl RunSync for MockRunner {
        type Error = String;
        type Prepared = (); // No state needed

        fn prepare_sync(&mut self, _program: rr::Program) -> Result<Self::Prepared, Self::Error> {
            Ok(())
        }

        fn execute_sync(
            &mut self,
            _handle: &Self::Prepared,
            input: &[u8],
        ) -> Result<Vec<u8>, Self::Error> {
            // Echo the input
            Ok(input.to_vec())
        }
    }

    #[test]
    fn test_channel_compiles() {
        // This test just verifies the module compiles and basic types work
        let runner = MockRunner;
        let (client, server) = new_pair(runner);

        // Cleanup
        drop(client);
        drop(server);
    }
}
