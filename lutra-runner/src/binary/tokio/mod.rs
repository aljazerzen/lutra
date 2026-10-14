//! LTDS duplex sessions. Each direction has one header and ends with EOS.
//! RPC cancellation poisons the client session; create a new connection to retry.
mod client;
mod server;

pub use client::Client;
pub use server::Server;

fn stream_error(error: impl std::fmt::Display) -> std::io::Error {
    std::io::Error::new(std::io::ErrorKind::InvalidData, error.to_string())
}
