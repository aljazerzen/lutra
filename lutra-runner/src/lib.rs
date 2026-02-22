#![cfg_attr(not(feature = "std"), no_std)]

pub mod binary;

#[cfg(feature = "channel")]
pub mod channel;

#[cfg(feature = "sync")]
mod sync;

#[cfg(feature = "async")]
mod r#async;

#[cfg(feature = "sync")]
pub use sync::SyncRunner;

#[cfg(feature = "async")]
pub use r#async::AsyncRunner;

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
// - through `runner::binary::Server`,
// - through `runner::posix::Server`,
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

/// Synchronous version of the Run trait for runners that don't block the process.
///
/// This trait should only be implemented for runners that:
/// - Execute entirely in-memory (like the interpreter)
/// - Use internal thread pools for blocking operations (like DuckDB)
///
/// Do NOT implement this for runners that perform actual async I/O operations
/// (like network database connections).
///
/// Methods take `&mut self` since synchronous operations may need to mutate
/// internal state (like database connections).
pub trait RunSync {
    type Error: core::fmt::Debug;
    type Prepared;

    /// Run a program.
    ///
    /// This is helper function for [RunSync::prepare_sync] followed by [RunSync::execute_sync],
    /// with input encoding and output decoding.
    fn run_sync<I, O>(
        &mut self,
        program: &rr::TypedProgram<I, O>,
        input: &I,
    ) -> Result<lutra_bin::Result<O>, Self::Error>
    where
        I: lutra_bin::Encode,
        O: lutra_bin::Decode,
    {
        let input = input.encode();
        let handle = self.prepare_sync(program.inner.clone())?;
        let output = self.execute_sync(&handle, &input)?;
        Ok(O::decode(&output))
    }

    /// Prepares a program for execution and returns a handle, which can be
    /// used with [RunSync::execute_sync].
    ///
    /// If the program is invalid, error is returned either now or later by [RunSync::execute_sync].
    ///
    /// When the handle is returned, the program might not be
    /// fully prepared yet, so first execution of the program
    /// might take longer then subsequent [RunSync::execute_sync] calls.
    fn prepare_sync(&mut self, program: rr::Program) -> Result<Self::Prepared, Self::Error>;

    /// Execute a prepared program synchronously.
    /// Program's format must match the format supported by this runner.
    fn execute_sync(
        &mut self,
        program: &Self::Prepared,
        input: &[u8],
    ) -> Result<vec::Vec<u8>, Self::Error>;

    /// Return static interface of this runner as Lutra source code.
    ///
    /// Runners can provide implementations for functions that are not part of
    /// standard library. This function returns definitions of these functions as
    /// Lutra source code.
    ///
    /// For example: interpreter can provide `fs::read_parquet()`
    /// and DuckDB runner can provide `sql::read_table()`.
    fn get_interface_sync(&mut self) -> Result<string::String, Self::Error> {
        Ok(string::String::new())
    }

    /// Releases any claimed resources.
    fn shutdown_sync(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

/// Standard error codes used in runner protocol
pub mod error_codes {
    pub const DECODE_ERROR: &str = "DECODE_ERROR";
    pub const PROGRAM_NOT_FOUND: &str = "PROGRAM_NOT_FOUND";
    pub const EXECUTION_ERROR: &str = "EXECUTION_ERROR";
    pub const PREPARE_ERROR: &str = "PREPARE_ERROR";
}
