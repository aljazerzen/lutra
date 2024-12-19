mod client;
mod server;

pub mod messages {
    include!(concat!(env!("OUT_DIR"), "/messages.rs"));
}

pub use client::Client;
pub use server::Server;
