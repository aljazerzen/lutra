//! A protocol for invoking a lutra runner over binary duplex stream.
//!
//! To expose a runner that implements [crate::Run] over a binary stream, use [Server].
//! To access a runner over binary stream via [crate::Run] trait, use [Client] or [EmbeddedClient].

#[cfg(feature = "binary-embedded")]
pub mod embedded;
#[cfg(feature = "binary-tokio")]
pub mod tokio;

#[cfg(any(feature = "binary-tokio", feature = "binary-embedded"))]
mod messages {
    include!(concat!(env!("OUT_DIR"), "/lutra.rs"));
}
