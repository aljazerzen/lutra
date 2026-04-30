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
#[cfg(not(feature = "std"))]
#[macro_use]
extern crate alloc;

pub mod proto {
    include!(concat!(env!("OUT_DIR"), "/lutra.rs"));
}

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
    /// Run a program.
    ///
    /// This is a helper for calls to [Run::prepare], [Run::execute], and [Run::release],
    /// with input encoding and output decoding.
    fn run<I, O>(
        &self,
        program: &rr::TypedProgram<I, O>,
        input: &I,
    ) -> impl Future<Output = Result<lutra_bin::Result<O>, proto::Error>>
    where
        I: lutra_bin::Encode,
        O: lutra_bin::Decode,
    {
        async move {
            let input = input.encode();
            let program_id = self.prepare(program.inner.clone()).await?;
            let output = self.execute(program_id, &input).await?;
            self.release(program_id).await?;
            Ok(O::decode(&output))
        }
    }

    /// Prepares a program for execution and returns its handle.
    ///
    /// If the program is invalid, error is returned either now or later by [Run::execute].
    ///
    /// The runner allocates the `program_id` internally.
    /// On success, the program is ready to be executed via [Run::execute].
    fn prepare(&self, program: rr::Program) -> impl Future<Output = Result<u32, proto::Error>>;

    /// Execute a previously prepared program.
    fn execute(
        &self,
        program_id: u32,
        input: &[u8],
    ) -> impl Future<Output = Result<vec::Vec<u8>, proto::Error>>;

    /// Return static schema of this runner as Lutra source code.
    ///
    /// Runners can provide implementations for functions that are not part of
    /// standard library. This function returns definitions of these functions as
    /// Lutra source code.
    ///
    /// For example: interpreter can provide `fs::read_parquet()`
    /// and PostgreSQL runner can provide `sql::read_table()`.
    fn pull_schema(&self) -> impl Future<Output = Result<string::String, proto::Error>> {
        async { Ok(string::String::new()) }
    }

    /// Releases a prepared program, freeing any associated resources.
    /// No-op if `program_id` is not known.
    fn release(&self, program_id: u32) -> impl Future<Output = Result<(), proto::Error>>;

    /// Releases any claimed resources or network connections.
    fn shutdown(&self) -> impl Future<Output = Result<(), proto::Error>> {
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
    /// Run a program.
    ///
    /// This is a helper for calls to [RunSync::prepare_sync], [RunSync::execute_sync],
    /// and [RunSync::release_sync], with input encoding and output decoding.
    fn run_sync<I, O>(
        &mut self,
        program: &rr::TypedProgram<I, O>,
        input: &I,
    ) -> Result<lutra_bin::Result<O>, proto::Error>
    where
        I: lutra_bin::Encode,
        O: lutra_bin::Decode,
    {
        let input = input.encode();
        let program_id = self.prepare_sync(program.inner.clone())?;
        let output = self.execute_sync(program_id, &input)?;
        self.release_sync(program_id)?;
        Ok(O::decode(&output))
    }

    /// Prepares a program for execution and returns its handle.
    ///
    /// If the program is invalid, error is returned either now or later by [RunSync::execute_sync].
    ///
    /// The runner allocates the `program_id` internally.
    fn prepare_sync(&mut self, program: rr::Program) -> Result<u32, proto::Error>;

    /// Execute a previously prepared program.
    fn execute_sync(&mut self, program_id: u32, input: &[u8])
    -> Result<vec::Vec<u8>, proto::Error>;

    /// Return static schema of this runner as Lutra source code.
    ///
    /// Runners can provide implementations for functions that are not part of
    /// standard library. This function returns definitions of these functions as
    /// Lutra source code.
    ///
    /// For example: interpreter can provide `fs::read_parquet()`
    /// and DuckDB runner can provide `sql::read_table()`.
    fn pull_schema_sync(&mut self) -> Result<string::String, proto::Error> {
        Ok(string::String::new())
    }

    /// Releases a prepared program, freeing any associated resources.
    /// No-op if `program_id` is not known.
    fn release_sync(&mut self, program_id: u32) -> Result<(), proto::Error>;

    /// Releases any claimed resources.
    fn shutdown_sync(&mut self) -> Result<(), proto::Error> {
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

impl proto::Error {
    /// Set the `code` field, overwriting any existing value.
    pub fn with_code(mut self, code: &str) -> Self {
        self.code = Some(::lutra_bin::string::String::from(code));
        self
    }

    pub fn program_not_found(program_id: u32) -> proto::Error {
        use lutra_bin::string::ToString;
        proto::Error {
            display: format!("Program {} not prepared", program_id),
            code: Some(crate::error_codes::PROGRAM_NOT_FOUND.to_string()),
        }
    }
}

impl ::core::fmt::Display for proto::Error {
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
        f.write_str(&self.display)
    }
}

#[cfg(feature = "std")]
impl ::std::error::Error for proto::Error {}

impl From<lutra_bin::Error> for proto::Error {
    fn from(value: lutra_bin::Error) -> Self {
        use lutra_bin::string::ToString;
        Self {
            code: Some(error_codes::DECODE_ERROR.into()),
            display: value.to_string(),
        }
    }
}

impl From<Result<(), proto::Error>> for proto::PrepareResult {
    fn from(value: Result<(), proto::Error>) -> Self {
        Self(value.err())
    }
}

impl From<Result<lutra_bin::vec::Vec<u8>, proto::Error>> for proto::ExecuteResult {
    fn from(value: Result<lutra_bin::vec::Vec<u8>, proto::Error>) -> Self {
        Self(value)
    }
}

impl From<Result<(), proto::Error>> for proto::ReleaseResult {
    fn from(value: Result<(), proto::Error>) -> Self {
        Self(value.err())
    }
}

impl From<Result<lutra_bin::string::String, proto::Error>> for proto::SchemaResult {
    fn from(value: Result<lutra_bin::string::String, proto::Error>) -> Self {
        Self(value)
    }
}
