//! Allocation-backed, no_std LTDS client. Pending execute responses are correlated
//! and cached. Cancelling an operation poisons the session; reconnect to retry.
mod client;

pub use client::{Client, Error};
