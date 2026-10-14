//! Framed streams of already-encoded Lutra values.
//! The sans-I/O core owns framing and compression; adapters own transport I/O.
#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

#[cfg(feature = "async")]
pub mod r#async;
mod compression;
pub mod sans_io;
#[cfg(feature = "std")]
pub mod sync;
#[cfg(feature = "tokio")]
pub mod tokio;

#[cfg(feature = "std")]
pub use sync::{Reader, Writer};

use bytes::{BufMut, BytesMut};
use lutra_bin::{Decode, Encode};

/// The Lutra binary header: `{type_digest: Prim64, compression: enum {None, Zstd}}`.
///
/// The digest is reserved and always zero. The encoded header occupies nine
/// bytes, without padding or a length prefix.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StreamHeader {
    pub compression: Compression,
}

impl Encode for StreamHeader {
    type HeadPtr = ();
    const HEAD_BITS: u32 = 72;

    fn encode_head(&self, buf: &mut BytesMut) {
        0_u64.encode_head(buf);
        buf.put_u8(match self.compression {
            Compression::None => 0,
            Compression::Zstd => 1,
        });
    }

    fn encode_body(&self, _: (), _: &mut BytesMut) {}
}

impl Decode for StreamHeader {
    fn decode(buf: &[u8]) -> lutra_bin::Result<Self> {
        let head = buf.get(..9).ok_or(lutra_bin::Error::InvalidData)?;
        if head[..8] != [0; 8] {
            return Err(lutra_bin::Error::InvalidData);
        }
        let compression = match head[8] {
            0 => Compression::None,
            1 => Compression::Zstd,
            _ => return Err(lutra_bin::Error::InvalidData),
        };
        Ok(Self { compression })
    }
}

/// Compression applied independently to every data frame.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Compression {
    None,
    Zstd,
}

/// Optional receiver policies, capped by the protocol's limits.
#[derive(Debug, Clone, Copy)]
pub struct Limits {
    pub max_frame_size: u32,
    pub max_decompressed_frame_size: u32,
}

impl Default for Limits {
    fn default() -> Self {
        Self {
            max_frame_size: MAX_FRAME_SIZE,
            max_decompressed_frame_size: MAX_FRAME_SIZE,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected end of stream")]
    UnexpectedEof,
    #[error("compression is not supported by this build")]
    UnsupportedCompression,
    #[error("pending output must be drained first")]
    Pending,
    #[error("transport wrote zero bytes")]
    WriteZero,
    #[cfg(feature = "std")]
    #[error("stream I/O or compression error: {0}")]
    Io(#[from] std::io::Error),
    #[error("invalid Lutra data stream magic")]
    InvalidMagic,
    #[error("unsupported Lutra data stream version: {0}")]
    UnsupportedVersion(u8),
    #[error("invalid stream header (expected zero digest and None or Zstd compression)")]
    InvalidHeader,
    #[error("unknown stream control word: {0:#010x}")]
    UnknownControl(u32),
    #[error("frame exceeds the payload size limit")]
    FrameTooLarge,
    #[error("decompressed frame exceeds the payload size limit")]
    DecompressedFrameTooLarge,
    #[error("payload must contain exactly one ordinary Zstandard frame")]
    InvalidZstdFrame,
    #[error("stream cannot be reused after an error or completion")]
    Closed,
}

pub type Result<T> = core::result::Result<T, Error>;

/// ASCII magic identifying a Lutra data stream.
pub const MAGIC: [u8; 4] = *b"LTDS";
/// Current envelope and value-encoding version.
pub const VERSION: u8 = 0;
/// Maximum wire and decompressed payload size, in bytes.
pub const MAX_FRAME_SIZE: u32 = 0x7fff_ffff;
/// End-of-stream control word.
pub const EOS: u32 = 0x8000_0000;
