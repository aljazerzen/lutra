//! Wraps [RunSync] to provide [Run].
//!
//! This module provides [AsyncRunner], which wraps any [RunSync] implementation
//! and provides async versions of all methods by managing an `Arc<Mutex<T>>` internally.
//!
//! # Usage
//!
//! ```ignore
//! use lutra_runner::{Run, RunSync, async::AsyncRunner};
//!
//! async fn use_in_async() {
//!     let sync_runner = get_sync_runner();
//!     let runner = AsyncRunner::new(sync_runner);
//!
//!     // Now you can use runner with .await
//!     let result = runner.run(&program, &input).await;
//! }
//! ```

use crate::{Run, RunSync, rr, string, vec};

/// Wrapper that adapts a `RunSync` implementation to work in async contexts.
///
/// This wrapper uses `Arc<Mutex<T>>` internally to provide thread-safe shared
/// access to a synchronous runner from async code.
///
/// # Example
///
/// ```ignore
/// use lutra_runner::async::AsyncRunner;
///
/// async fn use_in_async() {
///     let sync_runner = get_sync_runner();
///     let runner = AsyncRunner::new(sync_runner);
///
///     // Now you can use runner with .await
///     let result = runner.run(&program, &input).await;
/// }
/// ```
pub struct AsyncRunner<T> {
    inner: std::sync::Arc<std::sync::Mutex<T>>,
}

impl<T> AsyncRunner<T> {
    /// Create a new async wrapper around a synchronous runner.
    pub fn new(runner: T) -> Self
    where
        T: RunSync + Send + 'static,
    {
        Self {
            inner: std::sync::Arc::new(std::sync::Mutex::new(runner)),
        }
    }

    /// Get a reference to the inner Arc<Mutex<T>>.
    pub fn inner(&self) -> &std::sync::Arc<std::sync::Mutex<T>> {
        &self.inner
    }
}

impl<T> Clone for AsyncRunner<T> {
    fn clone(&self) -> Self {
        Self {
            inner: std::sync::Arc::clone(&self.inner),
        }
    }
}

impl<T> Run for AsyncRunner<T>
where
    T: RunSync + Send + 'static,
    T::Prepared: Clone + Send + Sync,
    T::Error: Send,
{
    type Error = T::Error;
    type Prepared = T::Prepared;

    async fn prepare(&self, program: rr::Program) -> Result<Self::Prepared, Self::Error> {
        let mut guard = self.inner.lock().unwrap();
        guard.prepare_sync(program)
    }

    async fn execute(
        &self,
        program: &Self::Prepared,
        input: &[u8],
    ) -> Result<vec::Vec<u8>, Self::Error> {
        let mut guard = self.inner.lock().unwrap();
        guard.execute_sync(program, input)
    }

    async fn get_interface(&self) -> Result<string::String, Self::Error> {
        let mut guard = self.inner.lock().unwrap();
        guard.get_interface_sync()
    }

    async fn shutdown(&self) -> Result<(), Self::Error> {
        let mut guard = self.inner.lock().unwrap();
        guard.shutdown_sync()
    }
}
