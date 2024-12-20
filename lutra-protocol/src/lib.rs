mod client;
pub mod http;
mod server;

pub mod messages {
    include!(concat!(env!("OUT_DIR"), "/messages.rs"));
}

pub use client::ClientConnection;
pub use server::ServerConnection;
