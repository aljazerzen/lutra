#![cfg_attr(not(feature = "std"), no_std)]

pub mod binary;

use lutra_bin::{string, vec};

// Runner Posix Interface:
// - an executable that can read arguments and env variables,
// - Runner RPC over stdin+stdout,

// Runner Binary Interface:
// - requires duplex stream,
// - passes messages encoded as lutra-bin, see messages.lt

// callers will be able to invoke
// - types implementing 'Runner Rust Interface' trait,
// - Binary streams, via `runner::binary::Client` wrapper,
// - Posix executables, via `runner::posix::Client` wrapper,
// - HTTP servers, via `runner::http::Client`,

// lutra-runner-postgres will be accessible:
// - via using `Runner` trait,
// - trough `runner::binary::Server`,
// - trough `runner::posix::Server`,
// - over HTTP, via `runner::http::Server`,

/// Ability to execute a lutra program.
pub trait Run {
    type Error: core::fmt::Debug;

    /// Execute a compiled program.
    /// Program's format must match the format supported by this runner.
    fn execute_raw(
        &self,
        program: &lutra_bin::Program,
        input: &[u8],
    ) -> impl Future<Output = Result<vec::Vec<u8>, Self::Error>>;

    /// Execute a compiled program.
    fn execute<I, O>(
        &self,
        program: &lutra_bin::TypedProgram<I, O>,
        input: &I,
    ) -> impl Future<Output = Result<lutra_bin::Result<O>, Self::Error>>
    where
        I: lutra_bin::Encode,
        O: lutra_bin::Decode,
    {
        async {
            let input = input.encode();
            let output = self.execute_raw(&program.inner, &input).await?;
            Ok(O::decode(&output))
        }
    }

    /// Return static interface of this runner as Lutra source code.
    ///
    /// Runners can provide implementations for functions that are not part of
    /// standard library. This function returns definitions of these functions as
    /// Lutra source code.
    ///
    /// For example: interpreter can provide `fs::read_parquet()`
    /// and PostgreSQL runner can provide `pg::reload_conf()`.
    fn get_interface(&self) -> impl Future<Output = Result<string::String, Self::Error>> {
        async { Ok(string::String::new()) }
    }

    /// Releases any claimed resources or network connections.
    fn shutdown(&self) -> impl Future<Output = Result<(), Self::Error>> {
        async { Ok(()) }
    }
}
