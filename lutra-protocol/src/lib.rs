mod client;
mod server;
pub mod http;

pub mod messages {
    include!(concat!(env!("OUT_DIR"), "/messages.rs"));
}

pub use client::ClientConnection;
pub use server::ServerConnection;
