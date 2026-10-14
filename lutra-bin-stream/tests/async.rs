#![cfg(feature = "tokio")]

use std::future::Future;
use std::io;
use std::pin::{Pin, pin};
use std::task::{Context, Poll, Waker};
use std::time::Duration;

#[cfg(feature = "compression-zstd")]
use lutra_bin_stream::EOS;
use lutra_bin_stream::r#async::{Error as AsyncError, Reader, Writer};
use lutra_bin_stream::tokio::TokioIo;
use lutra_bin_stream::{Compression, Error, Limits};
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt, ReadBuf};

#[tokio::test]
async fn fragmented_wire_interoperates_with_sync_in_both_directions() {
    let large: Vec<_> = (0..20_000).map(|i| (i % 251) as u8).collect();
    let values: &[&[u8]] = &[b"", b"abc", &large, b""];
    for compression in compressions() {
        let expected = wire(compression, values);
        for chunk in [1, 3, 13, 8192] {
            let mut tx = TokioIo(Transport::new(Vec::new(), chunk));
            let mut writer = Writer::new(compression).unwrap();
            assert!(tx.0.output.is_empty());
            writer.flush(&mut tx).await.unwrap();
            assert_eq!(tx.0.output, expected[..14]);
            for value in values {
                writer.write_frame(&mut tx, value).await.unwrap();
            }
            writer.finish(&mut tx).await.unwrap();
            writer.finish(&mut tx).await.unwrap();
            assert_eq!(tx.0.output, expected);
            let mut sync = lutra_bin_stream::Reader::new(tx.0.output.as_slice()).unwrap();
            for value in values {
                assert_eq!(sync.read_frame().unwrap().as_deref(), Some(*value));
            }
            assert_eq!(sync.read_frame().unwrap(), None);

            let mut input = expected.clone();
            input.extend_from_slice(b"trailer");
            let mut rx = TokioIo(Transport::new(input, chunk));
            let mut reader = Reader::new();
            assert!(reader.header().is_none());
            for value in values {
                assert_eq!(
                    reader.read_frame(&mut rx).await.unwrap().as_deref(),
                    Some(*value)
                );
                assert_eq!(reader.header().unwrap().compression, compression);
            }
            assert_eq!(reader.read_frame(&mut rx).await.unwrap(), None);
            let calls = rx.0.calls;
            assert_eq!(reader.read_frame(&mut rx).await.unwrap(), None);
            assert_eq!(rx.0.calls, calls);
            assert_eq!(&rx.0.input[rx.0.position..], b"trailer");
        }
    }
}

#[tokio::test]
async fn tiny_duplex_supports_backpressure_and_opposite_direction_after_finish() {
    tokio::time::timeout(Duration::from_secs(5), async {
        for compression in compressions() {
            let (mut client, mut server) = tokio::io::duplex(1);
            let value = vec![42; 20_000];
            let send = async {
                let mut writer = Writer::new(compression).unwrap();
                writer.flush(&mut TokioIo(&mut client)).await.unwrap();
                writer
                    .write_frame(&mut TokioIo(&mut client), &value)
                    .await
                    .unwrap();
                writer.finish(&mut TokioIo(&mut client)).await.unwrap();
                let mut reply = [0; 3];
                client.read_exact(&mut reply).await.unwrap();
                assert_eq!(&reply, b"ack");
            };
            let receive = async {
                let mut reader = Reader::new();
                assert_eq!(
                    reader.read_frame(&mut TokioIo(&mut server)).await.unwrap(),
                    Some(value.clone())
                );
                assert_eq!(
                    reader.read_frame(&mut TokioIo(&mut server)).await.unwrap(),
                    None
                );
                server.write_all(b"ack").await.unwrap();
            };
            tokio::join!(send, receive);
        }
    })
    .await
    .expect("duplex transfer stalled");
}

#[tokio::test]
async fn cancelled_reads_resume_every_partial_header_length_payload_and_eos() {
    for compression in compressions() {
        let bytes = wire(compression, &[b"a payload"]);
        for stop in 0..bytes.len() {
            let mut rx = TokioIo(Transport::new(bytes.clone(), 1));
            rx.0.stop = stop;
            let mut reader = Reader::new();
            let frame_end = bytes.len() - 4;
            if stop >= frame_end {
                assert_eq!(
                    reader.read_frame(&mut rx).await.unwrap(),
                    Some(b"a payload".to_vec())
                );
            }
            cancel_pending(reader.read_frame(&mut rx));
            assert_eq!(rx.0.position, stop);
            assert_eq!(
                rx.0.pending_polls, 2,
                "must yield rather than retry Pending"
            );
            rx.0.stop = usize::MAX;
            if stop < frame_end {
                assert_eq!(
                    reader.read_frame(&mut rx).await.unwrap(),
                    Some(b"a payload".to_vec())
                );
            }
            assert_eq!(reader.read_frame(&mut rx).await.unwrap(), None);
        }
    }
}

#[tokio::test]
async fn cancelled_writes_flush_pending_frame_and_finish_exactly_once() {
    for compression in compressions() {
        let expected = wire(compression, &[b"payload"]);
        for stop in 14..expected.len() {
            let mut tx = TokioIo(Transport::new(Vec::new(), 1));
            let mut writer = Writer::new(compression).unwrap();
            // A cancelled write before the initial header drains need not queue its value.
            writer.flush(&mut tx).await.unwrap();
            tx.0.stop = stop;
            if stop < expected.len() - 4 {
                cancel_pending(writer.write_frame(&mut tx, b"payload"));
            } else {
                writer.write_frame(&mut tx, b"payload").await.unwrap();
                cancel_pending(writer.finish(&mut tx));
            }
            assert_eq!(tx.0.output, expected[..stop]);
            assert_eq!(tx.0.pending_polls, 2);
            tx.0.stop = usize::MAX;
            writer.flush(&mut tx).await.unwrap();
            writer.finish(&mut tx).await.unwrap();
            writer.finish(&mut tx).await.unwrap();
            assert_eq!(tx.0.output, expected);
            let calls = tx.0.calls;
            assert!(matches!(
                writer.write_frame(&mut tx, b"late").await,
                Err(AsyncError::Stream(Error::Closed))
            ));
            assert_eq!(tx.0.calls, calls);
        }
        for stop in 0..14 {
            let mut tx = TokioIo(Transport::new(Vec::new(), 1));
            tx.0.stop = stop;
            let mut writer = Writer::new(compression).unwrap();
            cancel_pending(writer.write_frame(&mut tx, b"not queued"));
            tx.0.stop = usize::MAX;
            writer.finish(&mut tx).await.unwrap();
            assert_eq!(tx.0.output, wire(compression, &[]));
        }
    }
}

#[tokio::test]
async fn empty_stream_and_every_truncation_have_distinct_terminal_states() {
    for compression in compressions() {
        let mut rx = TokioIo(Transport::new(wire(compression, &[]), 1));
        assert_eq!(Reader::new().read_frame(&mut rx).await.unwrap(), None);
        let bytes = wire(compression, &[b"", b"abc"]);
        for end in 0..bytes.len() {
            let mut rx = TokioIo(Transport::new(bytes[..end].to_vec(), 1));
            let mut reader = Reader::new();
            loop {
                match reader.read_frame(&mut rx).await {
                    Ok(Some(_)) => continue,
                    Err(AsyncError::Stream(Error::UnexpectedEof)) => break,
                    other => panic!("truncation at {end}: {other:?}"),
                }
            }
            let calls = rx.0.calls;
            assert!(matches!(
                reader.read_frame(&mut rx).await,
                Err(AsyncError::Stream(Error::Closed))
            ));
            assert_eq!(rx.0.calls, calls);
        }
    }
}

#[tokio::test]
async fn validation_errors_stop_at_boundary_and_poison_reader() {
    let good = wire(Compression::None, &[b"abc"]);
    for (index, byte, boundary) in [(0, b'X', 4), (4, 1, 5), (13, 255, 14), (17, 255, 18)] {
        let mut bytes = good.clone();
        bytes[index] = byte;
        let mut rx = TokioIo(Transport::new(bytes, 1));
        let mut reader = Reader::new();
        let error = reader.read_frame(&mut rx).await.unwrap_err();
        assert!(match index {
            0 => matches!(error, AsyncError::Stream(Error::InvalidMagic)),
            4 => matches!(error, AsyncError::Stream(Error::UnsupportedVersion(1))),
            13 => matches!(error, AsyncError::Stream(Error::InvalidHeader)),
            _ => matches!(error, AsyncError::Stream(Error::UnknownControl(_))),
        });
        assert_eq!(rx.0.position, boundary);
        let calls = rx.0.calls;
        assert!(matches!(
            reader.read_frame(&mut rx).await,
            Err(AsyncError::Stream(Error::Closed))
        ));
        assert_eq!(rx.0.calls, calls);
    }
    let mut reader = Reader::with_limits(Limits {
        max_frame_size: 2,
        ..Limits::default()
    });
    let mut rx = TokioIo(Transport::new(good, 1));
    assert!(matches!(
        reader.read_frame(&mut rx).await,
        Err(AsyncError::Stream(Error::FrameTooLarge))
    ));
    assert_eq!(rx.0.position, 18);
}

#[tokio::test]
async fn transport_errors_and_write_zero_poison_without_retrying() {
    let bytes = wire(Compression::None, &[b"abc"]);
    for stop in 0..bytes.len() {
        let mut rx = TokioIo(Transport::new(bytes.clone(), 1));
        rx.0.stop = stop;
        rx.0.failure = Failure::Error;
        let mut reader = Reader::new();
        loop {
            match reader.read_frame(&mut rx).await {
                Ok(Some(_)) => continue,
                Err(AsyncError::Io(e)) => {
                    assert_eq!(e.kind(), io::ErrorKind::BrokenPipe);
                    break;
                }
                other => panic!("{other:?}"),
            }
        }
        let calls = rx.0.calls;
        assert!(matches!(
            reader.read_frame(&mut rx).await,
            Err(AsyncError::Stream(Error::Closed))
        ));
        assert_eq!(rx.0.calls, calls);

        for failure in [Failure::Error, Failure::Zero] {
            let mut tx = TokioIo(Transport::new(Vec::new(), 1));
            tx.0.stop = stop;
            tx.0.failure = failure;
            let mut writer = Writer::new(Compression::None).unwrap();
            let result = match writer.write_frame(&mut tx, b"abc").await {
                Ok(()) => writer.finish(&mut tx).await,
                result => result,
            };
            match failure {
                Failure::Error => assert!(matches!(result, Err(AsyncError::Io(_)))),
                Failure::Zero => {
                    assert!(matches!(result, Err(AsyncError::Stream(Error::WriteZero))))
                }
                _ => unreachable!(),
            }
            let calls = tx.0.calls;
            assert!(matches!(
                writer.flush(&mut tx).await,
                Err(AsyncError::Stream(Error::Closed))
            ));
            assert!(matches!(
                writer.finish(&mut tx).await,
                Err(AsyncError::Stream(Error::Closed))
            ));
            assert!(matches!(
                writer.write_frame(&mut tx, b"").await,
                Err(AsyncError::Stream(Error::Closed))
            ));
            assert_eq!(tx.0.calls, calls);
        }
    }
}

#[tokio::test]
async fn cancelled_finish_during_transport_flush_does_not_duplicate_eos() {
    let mut tx = TokioIo(Transport::new(Vec::new(), 1));
    tx.0.block_flush = true;
    let mut writer = Writer::new(Compression::None).unwrap();
    cancel_pending(writer.finish(&mut tx));
    assert_eq!(tx.0.output, wire(Compression::None, &[]));
    assert_eq!(tx.0.pending_polls, 2);
    tx.0.block_flush = false;
    writer.finish(&mut tx).await.unwrap();
    assert_eq!(tx.0.output, wire(Compression::None, &[]));
    tx.0.fail_flush = true;
    assert!(matches!(
        writer.flush(&mut tx).await,
        Err(AsyncError::Io(_))
    ));
    let calls = tx.0.calls;
    assert!(matches!(
        writer.finish(&mut tx).await,
        Err(AsyncError::Stream(Error::Closed))
    ));
    assert_eq!(tx.0.calls, calls);
}

#[cfg(feature = "compression-zstd")]
#[tokio::test]
async fn corrupt_zstd_is_terminal_and_preserves_eos() {
    let mut bytes = wire(Compression::Zstd, &[]);
    bytes.truncate(14);
    bytes.extend_from_slice(&4_u32.to_le_bytes());
    bytes.extend_from_slice(b"oops");
    bytes.extend_from_slice(&EOS.to_le_bytes());
    let mut rx = TokioIo(Transport::new(bytes, 1));
    let mut reader = Reader::new();
    assert!(matches!(
        reader.read_frame(&mut rx).await,
        Err(AsyncError::Stream(Error::InvalidZstdFrame))
    ));
    assert_eq!(&rx.0.input[rx.0.position..], EOS.to_le_bytes());
    let calls = rx.0.calls;
    assert!(matches!(
        reader.read_frame(&mut rx).await,
        Err(AsyncError::Stream(Error::Closed))
    ));
    assert_eq!(rx.0.calls, calls);
}

#[cfg(not(feature = "compression-zstd"))]
#[tokio::test]
async fn unsupported_compression_fails_before_reading_frames() {
    assert!(matches!(
        Writer::new(Compression::Zstd),
        Err(Error::UnsupportedCompression)
    ));
    let mut bytes = wire(Compression::None, &[]);
    bytes[13] = 1;
    let mut rx = TokioIo(Transport::new(bytes, 1));
    let mut reader = Reader::new();
    assert!(matches!(
        reader.read_frame(&mut rx).await,
        Err(AsyncError::Stream(Error::UnsupportedCompression))
    ));
    assert_eq!(rx.0.position, 14);
    let calls = rx.0.calls;
    assert!(matches!(
        reader.read_frame(&mut rx).await,
        Err(AsyncError::Stream(Error::Closed))
    ));
    assert_eq!(rx.0.calls, calls);
}

fn compressions() -> Vec<Compression> {
    vec![
        Compression::None,
        #[cfg(feature = "compression-zstd")]
        Compression::Zstd,
    ]
}

fn wire(compression: Compression, values: &[&[u8]]) -> Vec<u8> {
    let mut writer = lutra_bin_stream::Writer::new(Vec::new(), compression).unwrap();
    for value in values {
        writer.write_frame(value).unwrap();
    }
    writer.finish().unwrap()
}

fn cancel_pending(future: impl Future) {
    let mut future = pin!(future);
    let mut context = Context::from_waker(Waker::noop());
    for _ in 0..2 {
        assert!(future.as_mut().poll(&mut context).is_pending());
    }
}

#[derive(Clone, Copy)]
enum Failure {
    Pending,
    Error,
    Zero,
}

// Poll-based I/O commits bytes only when returning Ready. Pending never changes
// byte positions: cancelling any read/write future is therefore safe. The gate
// is reopened only between futures, so it deliberately needs no wakeup.
struct Transport {
    input: Vec<u8>,
    position: usize,
    output: Vec<u8>,
    chunk: usize,
    stop: usize,
    failure: Failure,
    calls: usize,
    pending_polls: usize,
    block_flush: bool,
    fail_flush: bool,
}

impl Transport {
    fn new(input: Vec<u8>, chunk: usize) -> Self {
        Self {
            input,
            position: 0,
            output: Vec::new(),
            chunk,
            stop: usize::MAX,
            failure: Failure::Pending,
            calls: 0,
            pending_polls: 0,
            block_flush: false,
            fail_flush: false,
        }
    }

    fn blocked(&mut self) -> Poll<io::Result<usize>> {
        match self.failure {
            Failure::Pending => {
                self.pending_polls += 1;
                assert!(self.pending_polls <= 2, "adapter busy-loops on Pending");
                Poll::Pending
            }
            Failure::Error => Poll::Ready(Err(io::ErrorKind::BrokenPipe.into())),
            Failure::Zero => Poll::Ready(Ok(0)),
        }
    }
}

impl AsyncRead for Transport {
    fn poll_read(
        mut self: Pin<&mut Self>,
        _: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<io::Result<()>> {
        self.calls += 1;
        assert!(buf.remaining() > 0, "zero-length read can busy-loop");
        if self.position == self.stop {
            return self.blocked().map(|result| result.map(|_| ()));
        }
        let count = buf
            .remaining()
            .min(self.chunk)
            .min(self.stop - self.position)
            .min(self.input.len() - self.position);
        buf.put_slice(&self.input[self.position..self.position + count]);
        self.position += count;
        Poll::Ready(Ok(()))
    }
}

impl AsyncWrite for Transport {
    fn poll_write(
        mut self: Pin<&mut Self>,
        _: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<io::Result<usize>> {
        self.calls += 1;
        assert!(!buf.is_empty());
        if self.output.len() == self.stop {
            return self.blocked();
        }
        let count = buf.len().min(self.chunk).min(self.stop - self.output.len());
        self.output.extend_from_slice(&buf[..count]);
        Poll::Ready(Ok(count))
    }

    fn poll_flush(mut self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<io::Result<()>> {
        self.calls += 1;
        if self.block_flush {
            return self.blocked().map(|result| result.map(|_| ()));
        }
        Poll::Ready(if self.fail_flush {
            Err(io::ErrorKind::BrokenPipe.into())
        } else {
            Ok(())
        })
    }

    fn poll_shutdown(self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<io::Result<()>> {
        panic!("framing must not shut down the duplex transport")
    }
}
