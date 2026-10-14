use alloc::{borrow::Cow, vec::Vec};

use crate::{Compression, Error, Result};
#[cfg(feature = "compression-zstd")]
use std::io::{self, Read};

pub(crate) fn encode(compression: Compression, value: &[u8]) -> Result<Cow<'_, [u8]>> {
    check(compression)?;
    match compression {
        Compression::None => Ok(Cow::Borrowed(value)),
        Compression::Zstd => {
            #[cfg(feature = "compression-zstd")]
            {
                let encoder = zstd::stream::read::Encoder::with_buffer(value, 0)?;
                read_bounded(encoder, crate::MAX_FRAME_SIZE, false).map(Cow::Owned)
            }
            #[cfg(not(feature = "compression-zstd"))]
            Err(Error::UnsupportedCompression)
        }
    }
}

pub(crate) fn decode(compression: Compression, payload: Vec<u8>, limit: u32) -> Result<Vec<u8>> {
    check(compression)?;
    match compression {
        Compression::None => {
            if payload.len() > limit as usize {
                return Err(Error::DecompressedFrameTooLarge);
            }
            Ok(payload)
        }
        Compression::Zstd => {
            #[cfg(feature = "compression-zstd")]
            {
                if !payload.starts_with(&[0x28, 0xb5, 0x2f, 0xfd]) {
                    return Err(Error::InvalidZstdFrame);
                }
                let mut decoder =
                    zstd::stream::read::Decoder::with_buffer(payload.as_slice())?.single_frame();
                let value = read_bounded(&mut decoder, limit, true)?;
                if !decoder.finish().is_empty() {
                    return Err(Error::InvalidZstdFrame);
                }
                Ok(value)
            }
            #[cfg(not(feature = "compression-zstd"))]
            Err(Error::UnsupportedCompression)
        }
    }
}

pub(crate) fn check(compression: Compression) -> Result<()> {
    if compression == Compression::Zstd && !cfg!(feature = "compression-zstd") {
        return Err(Error::UnsupportedCompression);
    }
    Ok(())
}

#[cfg(feature = "compression-zstd")]
fn read_bounded(mut reader: impl Read, limit: u32, decompressed: bool) -> Result<Vec<u8>> {
    let mut result = Vec::new();
    reader
        .by_ref()
        .take(u64::from(limit))
        .read_to_end(&mut result)?;
    let mut extra = [0];
    loop {
        match reader.read(&mut extra) {
            Ok(0) => return Ok(result),
            Ok(_) => {
                return Err(if decompressed {
                    Error::DecompressedFrameTooLarge
                } else {
                    Error::FrameTooLarge
                });
            }
            Err(error) if error.kind() == io::ErrorKind::Interrupted => continue,
            Err(error) => return Err(error.into()),
        }
    }
}
