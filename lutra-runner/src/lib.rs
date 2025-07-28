#![cfg_attr(not(feature = "std"), no_std)]

pub mod binary;

use lutra_bin::{rr, string, vec};

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
    type Prepared;

    /// Run a program.
    ///
    /// This is helper function for [Run::prepare] followed by [Run::execute],
    /// with input encoding and output decoding.
    fn run<I, O>(
        &self,
        program: &rr::TypedProgram<I, O>,
        input: &I,
    ) -> impl Future<Output = Result<lutra_bin::Result<O>, Self::Error>>
    where
        I: lutra_bin::Encode,
        O: lutra_bin::Decode,
    {
        async {
            let input = input.encode();
            let handle = self.prepare(program.inner.clone()).await?;
            let output = self.execute(&handle, &input).await?;
            Ok(O::decode(&output))
        }
    }

    /// Prepares a program for execution and returns a handle, which can be
    /// used with [Run::execute].
    ///
    /// If the program is invalid, error is returned either now or later by [Run::execute].
    ///
    /// When the handle is returned, the program might not be
    /// fully prepared yet, so first execution of the program
    /// might take longer then subsequent [Run::execute] calls.
    fn prepare(
        &self,
        program: rr::Program,
    ) -> impl Future<Output = Result<Self::Prepared, Self::Error>>;

    /// Execute a prepared program.
    /// Program's format must match the format supported by this runner.
    fn execute(
        &self,
        program: &Self::Prepared,
        input: &[u8],
    ) -> impl Future<Output = Result<vec::Vec<u8>, Self::Error>>;

    /// Return static interface of this runner as Lutra source code.
    ///
    /// Runners can provide implementations for functions that are not part of
    /// standard library. This function returns definitions of these functions as
    /// Lutra source code.
    ///
    /// For example: interpreter can provide `fs::read_parquet()`
    /// and PostgreSQL runner can provide `sql::read_table()`.
    fn get_interface(&self) -> impl Future<Output = Result<string::String, Self::Error>> {
        async { Ok(string::String::new()) }
    }

    /// Releases any claimed resources or network connections.
    fn shutdown(&self) -> impl Future<Output = Result<(), Self::Error>> {
        async { Ok(()) }
    }
}
