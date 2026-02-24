//! Wraps [Run] to provide [RunSync].
//!
//! This module provides [SyncRunner], which wraps any async [Run] implementation
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
//! ```ignore
//! use lutra_runner::{Run, sync::SyncRunner};
//!
//! let async_runner = MyAsyncRunner;
//! let sync_runner = SyncRunner::new(async_runner);
//! ```
//!
//! # Design
//!
//! - Each `SyncRunner` owns a Tokio runtime and wraps the runner in an `Arc` for safe sharing
//! - All async methods are converted to blocking calls using `Runtime::block_on`
//! - The underlying runner is cloned (via `Arc`) for each blocking call to avoid lifetime issues
//! - Cloning a `SyncRunner` creates a new runtime but shares the same underlying runner via `Arc`

use crate::{Run, RunSync, proto};
use lutra_bin::{rr, string, vec};
use std::sync::Arc;
use tokio::runtime::Runtime;

/// A synchronous adapter for async [Run] implementations.
///
/// This wrapper contains a Tokio runtime and converts all async methods
/// into blocking calls. This is useful when you need to use an async runner
/// in a synchronous context.
///
/// # Example
///
/// ```ignore
/// use lutra_runner::{Run, sync::SyncRunner};
///
/// async fn get_runner() -> impl Run {
///     // ... create your async runner
/// #   todo!()
/// }
///
/// fn main() {
///     let async_runner = tokio::runtime::Runtime::new()
///         .unwrap()
///         .block_on(get_runner());
///     let sync_runner = SyncRunner::new(async_runner);
///     // Now you can use sync_runner in synchronous code
/// }
/// ```
pub struct SyncRunner<R> {
    runner: Arc<R>,
    rt: Runtime,
}

impl<R> SyncRunner<R> {
    /// Creates a new synchronous runner wrapper.
    ///
    /// This will create a new Tokio runtime with default configuration.
    /// If you need custom runtime configuration, use [SyncRunner::with_runtime].
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

// Implement RunSync for SyncRunner to enable use with channel protocol
impl<R> RunSync for SyncRunner<R>
where
    R: Run + Send + Sync + 'static,
{
    fn prepare_sync(&mut self, program: rr::Program) -> Result<u32, proto::Error> {
        let runner = Arc::clone(&self.runner);
        self.rt
            .block_on(async move { runner.prepare(program).await })
    }

    fn execute_sync(
        &mut self,
        program_id: u32,
        input: &[u8],
    ) -> Result<vec::Vec<u8>, proto::Error> {
        let runner = Arc::clone(&self.runner);
        self.rt
            .block_on(async move { runner.execute(program_id, input).await })
    }

    fn pull_schema_sync(&mut self) -> Result<string::String, proto::Error> {
        let runner = Arc::clone(&self.runner);
        self.rt.block_on(async move { runner.pull_schema().await })
    }

    fn release_sync(&mut self, program_id: u32) -> Result<(), proto::Error> {
        let runner = Arc::clone(&self.runner);
        self.rt
            .block_on(async move { runner.release(program_id).await })
    }

    fn shutdown_sync(&mut self) -> Result<(), proto::Error> {
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
        async fn prepare(&self, _program: rr::Program) -> Result<u32, proto::Error> {
            Ok(0)
        }

        async fn execute(
            &self,
            _program_id: u32,
            input: &[u8],
        ) -> Result<vec::Vec<u8>, proto::Error> {
            Ok(vec::Vec::from(input))
        }

        async fn pull_schema(&self) -> Result<string::String, proto::Error> {
            Ok(string::String::from("mock interface"))
        }

        async fn release(&self, _program_id: u32) -> Result<(), proto::Error> {
            Ok(())
        }
    }

    #[test]
    fn test_sync_runner_basic() {
        let mock = MockRunner;
        let mut sync_runner = SyncRunner::new(mock);

        use lutra_bin::br;
        let program = rr::Program::BytecodeLt(br::Program {
            externals: vec::Vec::new(),
            main: br::Expr {
                kind: br::ExprKind::Literal(vec::Vec::new()),
            },
            defs: vec::Vec::new(),
        });
        let program_id = sync_runner.prepare_sync(program).unwrap();

        let input_data = b"hello world";
        let output = sync_runner.execute_sync(program_id, input_data).unwrap();
        assert_eq!(&output[..], input_data);

        let interface = sync_runner.pull_schema_sync().unwrap();
        assert_eq!(&interface[..], "mock interface");

        sync_runner.release_sync(program_id).unwrap();
        sync_runner.shutdown_sync().unwrap();
    }
}
