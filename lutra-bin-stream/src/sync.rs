//! Blocking transport adapters for the sans-I/O state machines.
//!
//! ```
//! use lutra_bin::{Decode, Encode};
//! use lutra_bin_stream::{Compression, Reader, Writer};
//!
//! let mut writer = Writer::new(Vec::new(), Compression::None)?;
//! writer.write_frame(&vec![1_u64, 2, 3].encode())?;
//! writer.write_frame(&Vec::<u64>::new().encode())?;
//! let bytes = writer.finish()?;
//! let mut reader = Reader::new(bytes.as_slice())?;
//! assert_eq!(Vec::<u64>::decode(&reader.read_frame()?.unwrap()).unwrap(), vec![1, 2, 3]);
//! assert!(Vec::<u64>::decode(&reader.read_frame()?.unwrap()).unwrap().is_empty());
//! assert!(reader.read_frame()?.is_none());
//! # Ok::<(), lutra_bin_stream::Error>(())
//! ```

use std::io::{self, Read, Write};

use crate::{Compression, Error, Limits, Result, StreamHeader, sans_io};
use sans_io::Event;

/// A reader of complete, decompressed encoded values.
pub struct Reader<R> {
    inner: R,
    core: sans_io::Reader,
}

impl<R: Read> Reader<R> {
    pub fn new(inner: R) -> Result<Self> {
        Self::with_limits(inner, Limits::default())
    }

    pub fn with_limits(inner: R, limits: Limits) -> Result<Self> {
        let mut reader = Self {
            inner,
            core: sans_io::Reader::new(limits),
        };
        reader.next_event()?;
        Ok(reader)
    }

    pub fn header(&self) -> StreamHeader {
        self.core.header().unwrap()
    }

    /// Only explicit EOS returns `None`. Truncation is an I/O error.
    pub fn read_frame(&mut self) -> Result<Option<Vec<u8>>> {
        match self.next_event()? {
            Event::Frame(value) => Ok(Some(value)),
            Event::End => Ok(None),
            Event::Header(_) => unreachable!("header consumed by constructor"),
        }
    }

    pub fn into_inner(self) -> R {
        self.inner
    }

    fn next_event(&mut self) -> Result<Event> {
        let mut buffer = [0; 8192];
        loop {
            if let (_, Some(event)) = self.core.feed(&[])? {
                return Ok(event);
            }
            let length = self.core.needed().min(buffer.len());
            let count = match self.inner.read(&mut buffer[..length]) {
                Ok(0) => {
                    return self.core.end_of_input().map(|()| Event::End).map_err(
                        |error| match error {
                            Error::UnexpectedEof => {
                                io::Error::from(io::ErrorKind::UnexpectedEof).into()
                            }
                            other => other,
                        },
                    );
                }
                Ok(count) => count,
                Err(error) if error.kind() == io::ErrorKind::Interrupted => continue,
                Err(error) => {
                    self.core.fail();
                    return Err(error.into());
                }
            };
            let (consumed, event) = self.core.feed(&buffer[..count])?;
            debug_assert_eq!(consumed, count);
            if let Some(event) = event {
                return Ok(event);
            }
        }
    }
}

/// A writer of encoded values. Dropping does not emit EOS.
pub struct Writer<W> {
    inner: W,
    core: sans_io::Writer,
    failed: bool,
}

impl<W: Write> Writer<W> {
    pub fn new(inner: W, compression: Compression) -> Result<Self> {
        let mut writer = Self {
            inner,
            core: sans_io::Writer::new(compression)?,
            failed: false,
        };
        writer.drain()?;
        Ok(writer)
    }

    pub fn write_frame(&mut self, value: &[u8]) -> Result<()> {
        if let Err(error) = self.core.write_frame(value) {
            self.fail();
            return Err(error);
        }
        self.drain()
    }

    pub fn flush(&mut self) -> Result<()> {
        if self.failed {
            return Err(Error::Closed);
        }
        self.drain()?;
        if let Err(error) = self.inner.flush() {
            self.fail();
            return Err(error.into());
        }
        Ok(())
    }

    /// Emit EOS, flush the transport, and return it.
    pub fn finish(mut self) -> Result<W> {
        self.core.finish()?;
        self.flush()?;
        Ok(self.inner)
    }

    fn drain(&mut self) -> Result<()> {
        while !self.core.pending().is_empty() {
            match self.inner.write(self.core.pending()) {
                Ok(0) => {
                    self.fail();
                    return Err(io::Error::from(io::ErrorKind::WriteZero).into());
                }
                Ok(count) => self.core.advance(count)?,
                Err(error) if error.kind() == io::ErrorKind::Interrupted => continue,
                Err(error) => {
                    self.fail();
                    return Err(error.into());
                }
            }
        }
        Ok(())
    }

    fn fail(&mut self) {
        self.failed = true;
        self.core.fail();
    }
}
