#![cfg(feature = "std")]

use std::io::{self, Cursor, Read, Write};

use lutra_bin::{Decode, Encode};
use lutra_bin_stream::{Compression, EOS, Error, Limits, Reader, StreamHeader, Writer};

#[test]
fn exact_uncompressed_wire_bytes() {
    let bytes = stream(Compression::None, &[b"abc", b"", &[0, 255]]);
    assert_eq!(
        bytes,
        [
            b'L', b'T', b'D', b'S', 0, // magic and version
            0, 0, 0, 0, 0, 0, 0, 0, 0, // zero digest and None tag
            3, 0, 0, 0, b'a', b'b', b'c', 0, 0, 0, 0, // empty value, not EOS
            2, 0, 0, 0, 0, 255, 0, 0, 0, 128, // little-endian EOS
        ]
    );
    for compression in compressions() {
        let encoded = StreamHeader { compression }.encode();
        assert_eq!(encoded.as_slice(), &header(compression)[5..]);
        assert_eq!(
            StreamHeader::decode(&encoded).unwrap(),
            StreamHeader { compression }
        );
    }
}

#[test]
fn round_trips_empty_streams_and_multiple_values() {
    let binary: Vec<u8> = (0..=255).collect();
    let repeated = vec![42; 32_768];
    for compression in compressions() {
        for values in [
            vec![],
            vec![b"".as_slice()],
            vec![b"", b"hello", &binary, &repeated, b""],
        ] {
            let bytes = stream(compression, &values);
            let mut reader = Reader::new(bytes.as_slice()).unwrap();
            assert_eq!(reader.header().compression, compression);
            for value in values {
                assert_eq!(reader.read_frame().unwrap().as_deref(), Some(value));
            }
            assert_eq!(reader.read_frame().unwrap(), None);
            assert_eq!(reader.read_frame().unwrap(), None);
            assert!(reader.into_inner().is_empty());
        }
    }
}

#[test]
fn transports_real_lutra_binary_values() {
    let number = 0x1234_5678_90ab_cdef_u64;
    let text = "Lutra 🦦".to_owned();
    let numbers = vec![0_u64, 1, u64::MAX];
    let values = [number.encode(), text.encode(), numbers.encode()];
    for compression in compressions() {
        // Each stream has one type, even while its digest is a placeholder.
        let decoded: Vec<_> = values
            .iter()
            .map(|value| {
                let bytes = stream(compression, &[value, value]);
                let mut reader = Reader::new(bytes.as_slice()).unwrap();
                let first = reader.read_frame().unwrap().unwrap();
                assert_eq!(reader.read_frame().unwrap(), Some(first.clone()));
                assert_eq!(reader.read_frame().unwrap(), None);
                first
            })
            .collect();
        assert_eq!(u64::decode(&decoded[0]).unwrap(), number);
        assert_eq!(String::decode(&decoded[1]).unwrap(), text);
        assert_eq!(Vec::<u64>::decode(&decoded[2]).unwrap(), numbers);
    }
}

#[test]
fn rejects_invalid_headers() {
    for index in 0..4 {
        let mut bytes = header(Compression::None);
        bytes[index] ^= 255;
        assert!(matches!(
            Reader::new(bytes.as_slice()),
            Err(Error::InvalidMagic)
        ));
    }
    for version in [1, 127, 255] {
        let mut bytes = header(Compression::None);
        bytes[4] = version;
        assert!(
            matches!(Reader::new(bytes.as_slice()), Err(Error::UnsupportedVersion(v)) if v == version)
        );
    }
    for index in 5..13 {
        let mut bytes = header(Compression::None);
        bytes[index] = 1;
        assert!(matches!(
            Reader::new(bytes.as_slice()),
            Err(Error::InvalidHeader)
        ));
    }
    for tag in 2..=255 {
        let mut bytes = header(Compression::None);
        bytes[13] = tag;
        assert!(matches!(
            Reader::new(bytes.as_slice()),
            Err(Error::InvalidHeader)
        ));
    }
    for length in 0..9 {
        assert!(StreamHeader::decode(&[0; 9][..length]).is_err());
    }
}

#[test]
fn every_truncation_is_an_error_including_missing_eos() {
    for compression in compressions() {
        let bytes = stream(compression, &[b"", b"first", b"second"]);
        for end in 0..bytes.len() {
            let mut reader = match Reader::new(&bytes[..end]) {
                Ok(reader) => reader,
                Err(error) => {
                    assert!(end < 14);
                    assert_eof(error);
                    continue;
                }
            };
            loop {
                match reader.read_frame() {
                    Ok(Some(_)) => continue,
                    Ok(None) => panic!("accepted truncation at {end}/{}", bytes.len()),
                    Err(error) => {
                        assert_eof(error);
                        assert!(matches!(reader.read_frame(), Err(Error::Closed)));
                        break;
                    }
                }
            }
        }
    }
}

#[test]
fn reserved_controls_fail_without_consuming_payload() {
    for control in [EOS + 1, 0x8000_0100, 0xffff_ffff] {
        let mut bytes = header(Compression::None);
        bytes.extend_from_slice(&control.to_le_bytes());
        bytes.extend_from_slice(b"untouched");
        let mut reader = Reader::new(bytes.as_slice()).unwrap();
        assert!(matches!(reader.read_frame(), Err(Error::UnknownControl(c)) if c == control));
        assert!(matches!(reader.read_frame(), Err(Error::Closed)));
        assert_eq!(reader.into_inner(), b"untouched");
    }
}

#[test]
fn eos_never_reads_again_and_preserves_trailing_bytes() {
    let bytes = stream(Compression::None, &[]);
    let mut reader = Reader::new(NoReadPastEnd(Cursor::new(bytes.clone()))).unwrap();
    for _ in 0..3 {
        assert_eq!(reader.read_frame().unwrap(), None);
    }
    let mut with_trailer = bytes.clone();
    with_trailer.extend_from_slice(b"trailing bytes");
    let mut reader = Reader::new(Cursor::new(with_trailer)).unwrap();
    assert_eq!(reader.read_frame().unwrap(), None);
    assert_eq!(reader.read_frame().unwrap(), None);
    let mut inner = reader.into_inner();
    assert_eq!(inner.position(), bytes.len() as u64);
    let mut trailer = Vec::new();
    inner.read_to_end(&mut trailer).unwrap();
    assert_eq!(trailer, b"trailing bytes");
}

#[test]
fn wire_limits_reject_before_reading_payload_and_allow_exact_boundary() {
    for compression in compressions() {
        let bytes = stream(compression, &[b"value"]);
        let size = u32::from_le_bytes(bytes[14..18].try_into().unwrap());
        let limits = Limits {
            max_frame_size: size,
            max_decompressed_frame_size: 5,
        };
        let mut reader = Reader::with_limits(bytes.as_slice(), limits).unwrap();
        assert_eq!(
            reader.read_frame().unwrap().as_deref(),
            Some(b"value".as_slice())
        );
        assert_eq!(reader.read_frame().unwrap(), None);
        let mut reader = Reader::with_limits(
            bytes.as_slice(),
            Limits {
                max_frame_size: size - 1,
                ..limits
            },
        )
        .unwrap();
        assert!(matches!(reader.read_frame(), Err(Error::FrameTooLarge)));
        assert!(matches!(reader.read_frame(), Err(Error::Closed)));
        assert_eq!(reader.into_inner(), &bytes[18..]);
    }
}

#[test]
fn decompressed_limits_include_zero_and_exact_boundary() {
    for compression in compressions() {
        for size in [0, 1, 1024, 1_000_000] {
            let value = vec![0; size];
            let bytes = stream(compression, &[&value]);
            let limits = Limits {
                max_decompressed_frame_size: size as u32,
                ..Limits::default()
            };
            let mut reader = Reader::with_limits(bytes.as_slice(), limits).unwrap();
            assert_eq!(reader.read_frame().unwrap(), Some(value));
            assert_eq!(reader.read_frame().unwrap(), None);
            if size == 0 {
                continue;
            }
            if compression == Compression::Zstd && size == 1_000_000 {
                assert!(bytes.len() < 1024, "fixture must be a compressed bomb");
            }
            let mut reader = Reader::with_limits(
                bytes.as_slice(),
                Limits {
                    max_decompressed_frame_size: size as u32 - 1,
                    ..limits
                },
            )
            .unwrap();
            assert!(matches!(
                reader.read_frame(),
                Err(Error::DecompressedFrameTooLarge)
            ));
            assert!(matches!(reader.read_frame(), Err(Error::Closed)));
        }
    }
    let bytes = stream(Compression::None, &[b""]);
    let mut reader = Reader::with_limits(
        bytes.as_slice(),
        Limits {
            max_frame_size: 0,
            max_decompressed_frame_size: 0,
        },
    )
    .unwrap();
    assert_eq!(reader.read_frame().unwrap(), Some(vec![]));
    assert_eq!(reader.read_frame().unwrap(), None);
}

#[cfg(feature = "compression-zstd")]
#[test]
fn zstd_rejects_truncation_corruption_concatenation_and_garbage() {
    let compressed = zstd::stream::encode_all(b"a real encoded payload".as_slice(), 0).unwrap();
    for end in 0..compressed.len() {
        reject_zstd(&compressed[..end], end < 4);
    }
    let mut corrupt = compressed.clone();
    corrupt[4] |= 0x08; // reserved frame-header descriptor bit
    reject_zstd(&corrupt, false);
    let mut joined = compressed.clone();
    joined.extend_from_slice(&compressed);
    reject_zstd(&joined, true);
    let mut garbage = compressed.clone();
    garbage.extend_from_slice(b"garbage");
    reject_zstd(&garbage, true);
    reject_zstd(b"not zstandard", true);
    for tag in 0x50..=0x5f {
        let skippable = [tag, 0x2a, 0x4d, 0x18, 1, 0, 0, 0, 42];
        reject_zstd(&skippable, true);
        let mut appended = compressed.clone();
        appended.extend_from_slice(&skippable);
        reject_zstd(&appended, true);
    }
}

#[test]
fn supports_short_reads_and_writes() {
    for compression in compressions() {
        let mut writer = Writer::new(ShortIo(Vec::new()), compression).unwrap();
        writer.write_frame(b"short io").unwrap();
        writer.write_frame(b"").unwrap();
        let bytes = writer.finish().unwrap().0;
        let mut reader = Reader::new(ShortIo(bytes.as_slice())).unwrap();
        assert_eq!(
            reader.read_frame().unwrap().as_deref(),
            Some(b"short io".as_slice())
        );
        assert_eq!(reader.read_frame().unwrap(), Some(vec![]));
        assert_eq!(reader.read_frame().unwrap(), None);
    }
}

#[test]
fn writer_errors_close_writer_and_finish_propagates_errors() {
    for remaining in 0..14 {
        assert!(matches!(
            Writer::new(
                FailWriter {
                    remaining,
                    fail_flush: false
                },
                Compression::None
            ),
            Err(Error::Io(_))
        ));
    }
    for remaining in 14..21 {
        let mut writer = Writer::new(
            FailWriter {
                remaining,
                fail_flush: false,
            },
            Compression::None,
        )
        .unwrap();
        assert!(matches!(writer.write_frame(b"abc"), Err(Error::Io(_))));
        assert!(matches!(writer.write_frame(b""), Err(Error::Closed)));
        assert!(matches!(writer.finish(), Err(Error::Closed)));
    }
    for remaining in 14..18 {
        let writer = Writer::new(
            FailWriter {
                remaining,
                fail_flush: false,
            },
            Compression::None,
        )
        .unwrap();
        assert!(matches!(writer.finish(), Err(Error::Io(_))));
    }
    let writer = Writer::new(
        FailWriter {
            remaining: usize::MAX,
            fail_flush: true,
        },
        Compression::None,
    )
    .unwrap();
    assert!(matches!(writer.finish(), Err(Error::Io(_))));
}

#[test]
fn flush_emits_buffered_frames_without_ending_stream() {
    let mut bytes = Vec::new();
    {
        let buffer = io::BufWriter::new(&mut bytes);
        let mut writer = Writer::new(buffer, Compression::None).unwrap();
        writer.write_frame(b"event").unwrap();
        writer.flush().unwrap();
    }
    let mut reader = Reader::new(bytes.as_slice()).unwrap();
    assert_eq!(reader.read_frame().unwrap(), Some(b"event".to_vec()));
    assert_eof(reader.read_frame().unwrap_err());

    let mut writer = Writer::new(
        FailWriter {
            remaining: usize::MAX,
            fail_flush: true,
        },
        Compression::None,
    )
    .unwrap();
    assert!(matches!(writer.flush(), Err(Error::Io(_))));
    assert!(matches!(writer.flush(), Err(Error::Closed)));
    assert!(matches!(writer.write_frame(b""), Err(Error::Closed)));
    assert!(matches!(writer.finish(), Err(Error::Closed)));
}

#[test]
fn dropping_writer_does_not_emit_eos() {
    let mut bytes = Vec::new();
    {
        let mut writer = Writer::new(&mut bytes, Compression::None).unwrap();
        writer.write_frame(b"value").unwrap();
    }
    assert_eq!(&bytes[..14], header(Compression::None));
    assert_eq!(bytes.len(), 14 + 4 + 5);
    let mut reader = Reader::new(bytes.as_slice()).unwrap();
    assert_eq!(
        reader.read_frame().unwrap().as_deref(),
        Some(b"value".as_slice())
    );
    assert_eof(reader.read_frame().unwrap_err());
}

fn compressions() -> impl Iterator<Item = Compression> {
    [Compression::None]
        .into_iter()
        .chain(cfg!(feature = "compression-zstd").then_some(Compression::Zstd))
}

fn header(compression: Compression) -> Vec<u8> {
    let mut bytes = b"LTDS\0".to_vec();
    bytes.extend_from_slice(&[0; 8]);
    bytes.push(u8::from(compression == Compression::Zstd));
    bytes
}

#[cfg(feature = "compression-zstd")]
fn envelope(compression: Compression, payload: &[u8]) -> Vec<u8> {
    let mut bytes = header(compression);
    bytes.extend_from_slice(&(payload.len() as u32).to_le_bytes());
    bytes.extend_from_slice(payload);
    bytes.extend_from_slice(&EOS.to_le_bytes());
    bytes
}

fn stream(compression: Compression, values: &[&[u8]]) -> Vec<u8> {
    let mut writer = Writer::new(Vec::new(), compression).unwrap();
    for value in values {
        writer.write_frame(value).unwrap();
    }
    writer.finish().unwrap()
}

fn assert_eof(error: Error) {
    assert!(
        matches!(error, Error::Io(ref e) if e.kind() == io::ErrorKind::UnexpectedEof),
        "{error:?}"
    );
}

struct NoReadPastEnd(Cursor<Vec<u8>>);

impl Read for NoReadPastEnd {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        assert!(
            self.0.position() < self.0.get_ref().len() as u64,
            "read after EOS"
        );
        self.0.read(buf)
    }
}

#[cfg(feature = "compression-zstd")]
fn reject_zstd(payload: &[u8], framing_error: bool) {
    let bytes = envelope(Compression::Zstd, payload);
    let mut reader = Reader::new(bytes.as_slice()).unwrap();
    let error = reader.read_frame().unwrap_err();
    if framing_error {
        assert!(matches!(error, Error::InvalidZstdFrame), "{error:?}");
    } else {
        assert!(
            matches!(error, Error::Io(_) | Error::InvalidZstdFrame),
            "{error:?}"
        );
    }
    assert!(matches!(reader.read_frame(), Err(Error::Closed)));
    assert_eq!(reader.into_inner(), EOS.to_le_bytes());
}

struct ShortIo<T>(T);

impl<T: Read> Read for ShortIo<T> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let len = buf.len().min(1);
        self.0.read(&mut buf[..len])
    }
}

impl<T: Write> Write for ShortIo<T> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write(&buf[..buf.len().min(1)])
    }
    fn flush(&mut self) -> io::Result<()> {
        self.0.flush()
    }
}

struct FailWriter {
    remaining: usize,
    fail_flush: bool,
}

impl Write for FailWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        if self.remaining == 0 {
            return Err(io::ErrorKind::BrokenPipe.into());
        }
        let len = buf.len().min(self.remaining);
        self.remaining -= len;
        Ok(len)
    }
    fn flush(&mut self) -> io::Result<()> {
        if self.fail_flush {
            Err(io::ErrorKind::BrokenPipe.into())
        } else {
            Ok(())
        }
    }
}
