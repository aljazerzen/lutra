//! Bridge Tokio transports to the shared [`crate::r#async`] adapters.

use embedded_io_async::{ErrorType, Read, Write};
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt};

/// Adapt an owned or borrowed Tokio transport to `embedded_io_async`.
///
/// For duplex connections, keep one async reader and writer, and pass
/// `TokioIo(&mut connection)` to each operation. The adapters retain framing
/// state; this wrapper has no buffering or protocol state of its own.
pub struct TokioIo<T>(pub T);

impl<T> ErrorType for TokioIo<T> {
    type Error = std::io::Error;
}

impl<T: AsyncRead + Unpin> Read for TokioIo<T> {
    async fn read(&mut self, buf: &mut [u8]) -> Result<usize, Self::Error> {
        loop {
            match self.0.read(buf).await {
                Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
                result => return result,
            }
        }
    }
}

impl<T: AsyncWrite + Unpin> Write for TokioIo<T> {
    async fn write(&mut self, buf: &[u8]) -> Result<usize, Self::Error> {
        loop {
            match self.0.write(buf).await {
                Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
                result => return result,
            }
        }
    }

    async fn flush(&mut self) -> Result<(), Self::Error> {
        self.0.flush().await
    }
}
