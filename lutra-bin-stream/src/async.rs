//! Async adapters over [`crate::sans_io`], using `embedded_io_async` traits.
//!
//! These adapters borrow a transport for each operation, so one duplex connection
//! can have independent reader and writer state without splitting the transport.
//! Always pass the same transport for the lifetime of each adapter.
//!
//! Partial input/output progress survives cancellation, provided the transport's
//! individual `read`/`write` operations are cancellation-safe. After cancelling
//! `write_frame`, call `flush` to drain pending bytes; do not retry the same value
//! blindly, since it may already have been queued or sent. Cancellation before
//! the initial header is drained can occur before the value is queued. This is
//! framing safety, not transactional delivery of application messages.
//!
//! ```no_run
//! use embedded_io_async::{Read, Write};
//! use lutra_bin_stream::{Compression, r#async::{Error, Reader, Writer}};
//!
//! async fn exchange<C: Read + Write>(connection: &mut C) -> Result<(), Error<C::Error>> {
//!     let mut reader = Reader::new();
//!     let mut writer = Writer::new(Compression::None)?;
//!     writer.write_frame(connection, &[]).await?; // An empty tuple event.
//!     writer.flush(connection).await?;
//!     let _response = reader.read_frame(connection).await?;
//!     writer.finish(connection).await?; // End only our outgoing direction.
//!     while reader.read_frame(connection).await?.is_some() {}
//!     Ok(())
//! }
//! ```

use alloc::vec::Vec;
use embedded_io_async::{Read, Write};

use crate::{Compression, Limits, StreamHeader, sans_io};

/// Async reader retaining partial frames between calls.
pub struct Reader {
    state: sans_io::Reader,
}

impl Reader {
    /// Construct a reader. The header is read lazily by `read_frame`.
    pub fn new() -> Self {
        Self::with_limits(Limits::default())
    }

    pub fn with_limits(limits: Limits) -> Self {
        Self {
            state: sans_io::Reader::new(limits),
        }
    }

    pub fn header(&self) -> Option<StreamHeader> {
        self.state.header()
    }

    /// Return one decompressed encoded value, or `None` at explicit EOS.
    /// Reads only as far as the next frame/EOS, leaving trailing bytes untouched.
    pub async fn read_frame<R: Read>(&mut self, rx: &mut R) -> Result<Option<Vec<u8>>, R::Error> {
        // Check ended/poisoned states without touching the transport.
        if let (_, Some(sans_io::Event::End)) = self.state.feed(&[])? {
            return Ok(None);
        }
        let mut buffer = [0; 8192];
        loop {
            let needed = self.state.needed().min(buffer.len());
            let count = match rx.read(&mut buffer[..needed]).await {
                Ok(n) => n,
                Err(e) => {
                    self.state.fail();
                    return Err(Error::Io(e));
                }
            };
            if count == 0 {
                self.state.end_of_input()?;
                return Ok(None);
            }
            let (consumed, event) = self.state.feed(&buffer[..count])?;
            debug_assert_eq!(consumed, count);
            match event {
                Some(sans_io::Event::Frame(value)) => return Ok(Some(value)),
                Some(sans_io::Event::End) => return Ok(None),
                Some(sans_io::Event::Header(_)) | None => {}
            }
        }
    }
}

impl Default for Reader {
    fn default() -> Self {
        Self::new()
    }
}

/// Async writer retaining unsent bytes between calls.
///
/// Construction queues the header without I/O. `flush` can send it before
/// awaiting the peer, preventing handshake deadlocks with buffered transports.
/// Dropping this writer does not emit EOS.
pub struct Writer {
    state: sans_io::Writer,
}

impl Writer {
    pub fn new(compression: Compression) -> crate::Result<Self> {
        Ok(Self {
            state: sans_io::Writer::new(compression)?,
        })
    }

    /// Drain previous output, queue a value, and write its complete frame.
    /// Call `flush` afterwards if the transport buffers output.
    pub async fn write_frame<W: Write>(
        &mut self,
        tx: &mut W,
        value: &[u8],
    ) -> Result<(), W::Error> {
        self.drain(tx).await?;
        self.state.write_frame(value)?;
        self.drain(tx).await
    }

    /// Drain pending output and flush the transport without emitting EOS.
    pub async fn flush<W: Write>(&mut self, tx: &mut W) -> Result<(), W::Error> {
        self.drain(tx).await?;
        if let Err(e) = tx.flush().await {
            self.state.fail();
            return Err(Error::Io(e));
        }
        Ok(())
    }

    /// Emit EOS once, drain it, and flush. Safe to resume after cancellation.
    /// Does not close the duplex transport's opposite direction.
    pub async fn finish<W: Write>(&mut self, tx: &mut W) -> Result<(), W::Error> {
        self.drain(tx).await?;
        if !self.state.is_finished() {
            self.state.finish()?;
        }
        self.flush(tx).await
    }

    async fn drain<W: Write>(&mut self, tx: &mut W) -> Result<(), W::Error> {
        self.state.advance(0)?; // Check poisoned state even with no pending bytes.
        while !self.state.pending().is_empty() {
            match tx.write(self.state.pending()).await {
                Ok(0) => {
                    self.state.fail();
                    return Err(crate::Error::WriteZero.into());
                }
                Ok(count) => self.state.advance(count)?,
                Err(e) => {
                    self.state.fail();
                    return Err(Error::Io(e));
                }
            }
        }
        Ok(())
    }
}

/// Separates transport-specific errors from framing/compression errors.
#[derive(Debug, thiserror::Error)]
pub enum Error<E> {
    #[error("transport error: {0}")]
    Io(E),
    #[error("{0}")]
    Stream(#[from] crate::Error),
}

pub type Result<T, E> = core::result::Result<T, Error<E>>;
