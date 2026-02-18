//! Synchronous adapter for the async [`Run`] trait.
//!
//! This module provides [`SyncRunner`], which wraps any async [`Run`] implementation
//! and provides blocking versions of all methods by managing a Tokio runtime internally.
//!
//! # Usage
//!
//! Enable the `sync` feature in your `Cargo.toml`:
//!
//! ```toml
//! [dependencies]
//! lutra-runner = { ..., features = ["sync"] }
//! ```
//!
//! Then wrap any async runner:
//!
//! ```no_run
//! use lutra_runner::{Run, sync::SyncRunner};
//!
//! let async_runner = MyAsyncRunner;
//! let sync_runner = SyncRunner::new(async_runner);
//!
//! // Now use sync_runner with blocking calls
//! let program = /* ... */;
//! let prepared = sync_runner.prepare(program).unwrap();
//! ```
//!
//! # Design
//!
//! - Each `SyncRunner` owns a Tokio runtime and wraps the runner in an `Arc` for safe sharing
//! - All async methods are converted to blocking calls using `Runtime::block_on`
//! - The underlying runner is cloned (via `Arc`) for each blocking call to avoid lifetime issues
//! - Cloning a `SyncRunner` creates a new runtime but shares the same underlying runner via `Arc`

use crate::Run;
use lutra_bin::{rr, string, vec};
use std::sync::Arc;
use tokio::runtime::Runtime;

/// A synchronous adapter for async [`Run`] implementations.
///
/// This wrapper contains a Tokio runtime and converts all async methods
/// into blocking calls. This is useful when you need to use an async runner
/// in a synchronous context.
///
/// # Example
///
/// ```no_run
/// use lutra_runner::{Run, sync::SyncRunner};
///
/// async fn get_runner() -> impl Run<Error = std::io::Error, Prepared = ()> {
///     // ... create your async runner
/// #   todo!()
/// }
///
/// let async_runner = tokio::runtime::Runtime::new()
///     .unwrap()
///     .block_on(get_runner());
/// let sync_runner = SyncRunner::new(async_runner);
///
/// // Now you can use sync_runner in synchronous code
/// ```
pub struct SyncRunner<R> {
    runner: Arc<R>,
    rt: Runtime,
}

impl<R> SyncRunner<R> {
    /// Creates a new synchronous runner wrapper.
    ///
    /// This will create a new Tokio runtime with default configuration.
    /// If you need custom runtime configuration, use [`SyncRunner::with_runtime`].
    pub fn new(runner: R) -> Self
    where
        R: Send + Sync + 'static,
    {
        let runtime = tokio::runtime::Builder::new_current_thread()
            .build()
            .expect("failed to create Tokio runtime");
        Self::with_runtime(runner, runtime)
    }

    /// Creates a new synchronous runner wrapper with a custom Tokio runtime.
    pub fn with_runtime(runner: R, rt: Runtime) -> Self {
        Self {
            runner: Arc::new(runner),
            rt,
        }
    }

    /// Returns a reference to the underlying runner.
    pub fn inner(&self) -> &R {
        &self.runner
    }

    /// Returns a reference to the Tokio runtime.
    pub fn runtime(&self) -> &Runtime {
        &self.rt
    }
}

impl<R> SyncRunner<R>
where
    R: Run + Send + Sync + 'static,
    R::Prepared: Send + Sync,
    R::Error: Send,
{
    /// Synchronous version of [`Run::run`].
    pub fn run<I, O>(
        &self,
        program: &rr::TypedProgram<I, O>,
        input: &I,
    ) -> Result<lutra_bin::Result<O>, R::Error>
    where
        I: lutra_bin::Encode + Send,
        O: lutra_bin::Decode + Send,
    {
        let runner = Arc::clone(&self.runner);
        let program_inner = program.inner.clone();
        let input = input.encode();

        self.rt.block_on(async move {
            let handle = runner.prepare(program_inner).await?;
            let output = runner.execute(&handle, &input).await?;
            Ok(O::decode(&output))
        })
    }

    /// Synchronous version of [`Run::prepare`].
    pub fn prepare(&self, program: rr::Program) -> Result<R::Prepared, R::Error> {
        let runner = Arc::clone(&self.runner);
        self.rt
            .block_on(async move { runner.prepare(program).await })
    }

    /// Synchronous version of [`Run::execute`].
    pub fn execute(&self, program: &R::Prepared, input: &[u8]) -> Result<vec::Vec<u8>, R::Error>
    where
        R::Prepared: Sync,
    {
        let runner = Arc::clone(&self.runner);
        self.rt
            .block_on(async move { runner.execute(program, input).await })
    }

    /// Synchronous version of [`Run::get_interface`].
    pub fn get_interface(&self) -> Result<string::String, R::Error> {
        let runner = Arc::clone(&self.runner);
        self.rt
            .block_on(async move { runner.get_interface().await })
    }

    /// Synchronous version of [`Run::shutdown`].
    pub fn shutdown(&self) -> Result<(), R::Error> {
        let runner = Arc::clone(&self.runner);
        self.rt.block_on(async move { runner.shutdown().await })
    }
}

// Implement Clone if the underlying runner is cheap to clone (via Arc)
impl<R> Clone for SyncRunner<R> {
    fn clone(&self) -> Self {
        Self {
            runner: Arc::clone(&self.runner),
            rt: Runtime::new().expect("failed to create Tokio runtime"),
        }
    }
}

impl<R> std::fmt::Debug for SyncRunner<R>
where
    R: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SyncRunner")
            .field("runner", &self.runner)
            .finish_non_exhaustive()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Mock runner for testing
    struct MockRunner;

    impl Run for MockRunner {
        type Error = String;
        type Prepared = String;

        async fn prepare(&self, _program: rr::Program) -> Result<Self::Prepared, Self::Error> {
            Ok("prepared".to_string())
        }

        async fn execute(
            &self,
            _program: &Self::Prepared,
            input: &[u8],
        ) -> Result<vec::Vec<u8>, Self::Error> {
            // Echo back the input
            Ok(vec::Vec::from(input))
        }

        async fn get_interface(&self) -> Result<string::String, Self::Error> {
            Ok(string::String::from("mock interface"))
        }

        async fn shutdown(&self) -> Result<(), Self::Error> {
            Ok(())
        }
    }

    #[test]
    fn test_sync_runner_basic() {
        let mock = MockRunner;
        let sync_runner = SyncRunner::new(mock);

        // Test prepare with a bytecode program
        use lutra_bin::br;
        let program = rr::Program::BytecodeLt(br::Program {
            externals: vec::Vec::new(),
            main: br::Expr {
                kind: br::ExprKind::Literal(vec::Vec::new()),
            },
            defs: vec::Vec::new(),
        });
        let prepared = sync_runner.prepare(program).unwrap();
        assert_eq!(prepared, "prepared");

        // Test execute
        let input_data = b"hello world";
        let output = sync_runner.execute(&prepared, input_data).unwrap();
        assert_eq!(&output[..], input_data);

        // Test get_interface
        let interface = sync_runner.get_interface().unwrap();
        assert_eq!(&interface[..], "mock interface");

        // Test shutdown
        sync_runner.shutdown().unwrap();
    }
}
