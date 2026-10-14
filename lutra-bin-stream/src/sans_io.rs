//! Incremental framing without transport I/O.

use alloc::vec::Vec;
use lutra_bin::{Decode, Encode};

use crate::{
    Compression, EOS, Error, Limits, MAGIC, MAX_FRAME_SIZE, Result, StreamHeader, VERSION,
    compression,
};

/// Incremental reader. Input beyond an event remains owned by the caller.
pub struct Reader {
    header: Option<StreamHeader>,
    limits: Limits,
    phase: Phase,
    buffer: Vec<u8>,
}

impl Reader {
    pub fn new(limits: Limits) -> Self {
        Self {
            header: None,
            limits: Limits {
                max_frame_size: limits.max_frame_size.min(MAX_FRAME_SIZE),
                max_decompressed_frame_size: limits.max_decompressed_frame_size.min(MAX_FRAME_SIZE),
            },
            phase: Phase::Header,
            buffer: Vec::new(),
        }
    }

    pub fn header(&self) -> Option<StreamHeader> {
        self.header
    }

    /// Bytes needed to reach the next validation boundary or event.
    pub fn needed(&self) -> usize {
        let target = match self.phase {
            Phase::Header => match self.buffer.len() {
                0..=3 => 4,
                4 => 5,
                _ => 14,
            },
            Phase::Word => 4,
            Phase::Payload(length) => length,
            Phase::Ended | Phase::Failed => return 0,
        };
        target - self.buffer.len()
    }

    pub fn feed(&mut self, input: &[u8]) -> Result<(usize, Option<Event>)> {
        let result = self.feed_inner(input);
        if result.is_err() {
            self.fail();
        }
        result
    }

    pub fn end_of_input(&mut self) -> Result<()> {
        match self.phase {
            Phase::Ended => Ok(()),
            Phase::Failed => Err(Error::Closed),
            _ => {
                self.fail();
                Err(Error::UnexpectedEof)
            }
        }
    }

    pub fn fail(&mut self) {
        self.phase = Phase::Failed;
    }

    pub fn is_ended(&self) -> bool {
        matches!(self.phase, Phase::Ended)
    }

    fn feed_inner(&mut self, input: &[u8]) -> Result<(usize, Option<Event>)> {
        match self.phase {
            Phase::Failed => return Err(Error::Closed),
            Phase::Ended => return Ok((0, Some(Event::End))),
            _ => {}
        }
        let mut consumed = 0;
        while consumed < input.len() {
            let count = self.needed().min(input.len() - consumed);
            self.buffer
                .extend_from_slice(&input[consumed..consumed + count]);
            consumed += count;
            match self.phase {
                Phase::Header => {
                    if self.buffer.len() >= 4 && self.buffer[..4] != MAGIC {
                        return Err(Error::InvalidMagic);
                    }
                    if self.buffer.len() >= 5 && self.buffer[4] != VERSION {
                        return Err(Error::UnsupportedVersion(self.buffer[4]));
                    }
                    if self.buffer.len() == 14 {
                        let header = StreamHeader::decode(&self.buffer[5..])
                            .map_err(|_| Error::InvalidHeader)?;
                        compression::check(header.compression)?;
                        self.header = Some(header);
                        self.buffer.clear();
                        self.phase = Phase::Word;
                        return Ok((consumed, Some(Event::Header(header))));
                    }
                }
                Phase::Word if self.buffer.len() == 4 => {
                    let length = u32::from_le_bytes(self.buffer[..].try_into().unwrap());
                    self.buffer.clear();
                    if length == EOS {
                        self.phase = Phase::Ended;
                        return Ok((consumed, Some(Event::End)));
                    }
                    if length > MAX_FRAME_SIZE {
                        return Err(Error::UnknownControl(length));
                    }
                    if length > self.limits.max_frame_size {
                        return Err(Error::FrameTooLarge);
                    }
                    if self.header.unwrap().compression == Compression::None
                        && length > self.limits.max_decompressed_frame_size
                    {
                        return Err(Error::DecompressedFrameTooLarge);
                    }
                    self.phase = Phase::Payload(length as usize);
                    if length == 0 {
                        return self.complete_frame(consumed);
                    }
                }
                Phase::Payload(length) if self.buffer.len() == length => {
                    return self.complete_frame(consumed);
                }
                _ => {}
            }
        }
        Ok((consumed, None))
    }

    fn complete_frame(&mut self, consumed: usize) -> Result<(usize, Option<Event>)> {
        let payload = core::mem::take(&mut self.buffer);
        let value = compression::decode(
            self.header.unwrap().compression,
            payload,
            self.limits.max_decompressed_frame_size,
        )?;
        self.phase = Phase::Word;
        Ok((consumed, Some(Event::Frame(value))))
    }
}

/// Incremental writer. Drain `pending()` with `advance()` before queueing more.
pub struct Writer {
    compression: Compression,
    output: Vec<u8>,
    position: usize,
    finished: bool,
    failed: bool,
}

impl Writer {
    pub fn new(compression: Compression) -> Result<Self> {
        compression::check(compression)?;
        let mut output = Vec::with_capacity(14);
        output.extend_from_slice(&MAGIC);
        output.push(VERSION);
        output.extend_from_slice(&StreamHeader { compression }.encode());
        Ok(Self {
            compression,
            output,
            position: 0,
            finished: false,
            failed: false,
        })
    }

    pub fn pending(&self) -> &[u8] {
        &self.output[self.position..]
    }

    /// Acknowledge output bytes. EOS may be drained after `finish()`.
    pub fn advance(&mut self, n: usize) -> Result<()> {
        if self.failed {
            return Err(Error::Closed);
        }
        if n > self.pending().len() {
            self.fail();
            return Err(Error::Closed);
        }
        self.position += n;
        if self.position == self.output.len() {
            self.output.clear();
            self.position = 0;
        }
        Ok(())
    }

    pub fn write_frame(&mut self, value: &[u8]) -> Result<()> {
        self.ready()?;
        let result = self.queue_frame(value);
        if result.is_err() {
            self.fail();
        }
        result
    }

    pub fn finish(&mut self) -> Result<()> {
        self.ready()?;
        self.output.extend_from_slice(&EOS.to_le_bytes());
        self.finished = true;
        Ok(())
    }

    /// Whether EOS has been queued, even if it has not yet been drained.
    pub fn is_finished(&self) -> bool {
        self.finished
    }

    pub fn fail(&mut self) {
        self.failed = true;
    }

    fn ready(&self) -> Result<()> {
        if self.failed || self.finished {
            return Err(Error::Closed);
        }
        if !self.pending().is_empty() {
            return Err(Error::Pending);
        }
        Ok(())
    }

    fn queue_frame(&mut self, value: &[u8]) -> Result<()> {
        if value.len() > MAX_FRAME_SIZE as usize {
            return Err(Error::DecompressedFrameTooLarge);
        }
        let payload = compression::encode(self.compression, value)?;
        self.output
            .extend_from_slice(&(payload.len() as u32).to_le_bytes());
        self.output.extend_from_slice(&payload);
        Ok(())
    }
}

/// One completed stream event.
#[derive(Debug, PartialEq, Eq)]
pub enum Event {
    Header(StreamHeader),
    Frame(Vec<u8>),
    End,
}

#[derive(Clone, Copy)]
enum Phase {
    Header,
    Word,
    Payload(usize),
    Ended,
    Failed,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fragmented_input_and_trailing_bytes() {
        let mut writer = Writer::new(Compression::None).unwrap();
        let mut bytes = writer.pending().to_vec();
        writer.advance(bytes.len()).unwrap();
        writer.write_frame(b"abc").unwrap();
        bytes.extend_from_slice(writer.pending());
        writer.advance(writer.pending().len()).unwrap();
        writer.write_frame(b"").unwrap();
        bytes.extend_from_slice(writer.pending());
        writer.advance(writer.pending().len()).unwrap();
        writer.finish().unwrap();
        bytes.extend_from_slice(writer.pending());
        for chunk_size in 1..=bytes.len() {
            let mut reader = Reader::new(Limits::default());
            let mut remaining = bytes.as_slice();
            let mut events = Vec::new();
            while !remaining.is_empty() {
                assert!(reader.needed() > 0);
                let (count, event) = reader
                    .feed(&remaining[..chunk_size.min(remaining.len())])
                    .unwrap();
                assert!(count > 0);
                remaining = &remaining[count..];
                if let Some(event) = event {
                    events.push(event);
                }
            }
            assert_eq!(
                events,
                alloc::vec![
                    Event::Header(StreamHeader {
                        compression: Compression::None
                    }),
                    Event::Frame(b"abc".to_vec()),
                    Event::Frame(Vec::new()),
                    Event::End,
                ]
            );
            assert_eq!(reader.needed(), 0);
            assert_eq!(reader.feed(b"trailer").unwrap(), (0, Some(Event::End)));
            reader.end_of_input().unwrap();
        }
    }

    #[test]
    fn empty_frame_emits_on_word_and_eos_leaves_trailer() {
        let writer = Writer::new(Compression::None).unwrap();
        let mut reader = Reader::new(Limits::default());
        reader.feed(writer.pending()).unwrap();
        assert_eq!(
            reader.feed(&[0; 4]).unwrap(),
            (4, Some(Event::Frame(Vec::new())))
        );
        let mut bytes = EOS.to_le_bytes().to_vec();
        bytes.extend_from_slice(b"trailer");
        assert_eq!(reader.feed(&bytes).unwrap(), (4, Some(Event::End)));
    }

    #[test]
    fn writer_backpressure_and_eos_draining() {
        let mut writer = Writer::new(Compression::None).unwrap();
        let header = writer.pending().to_vec();
        assert!(matches!(writer.write_frame(b"abc"), Err(Error::Pending)));
        assert!(matches!(writer.finish(), Err(Error::Pending)));
        assert_eq!(writer.pending(), header);
        writer.advance(3).unwrap();
        assert_eq!(writer.pending(), &header[3..]);
        writer.advance(11).unwrap();
        writer.finish().unwrap();
        assert!(writer.is_finished());
        assert!(matches!(writer.write_frame(b""), Err(Error::Closed)));
        assert_eq!(writer.pending(), EOS.to_le_bytes());
        writer.advance(4).unwrap();
        assert!(writer.pending().is_empty());
    }

    #[test]
    fn errors_poison_without_large_allocation() {
        let writer = Writer::new(Compression::None).unwrap();
        let mut reader = Reader::new(Limits::default());
        reader.feed(writer.pending()).unwrap();
        reader.feed(&MAX_FRAME_SIZE.to_le_bytes()).unwrap();
        assert_eq!(reader.needed(), MAX_FRAME_SIZE as usize);
        assert!(reader.buffer.capacity() < 1024);
        assert!(matches!(reader.end_of_input(), Err(Error::UnexpectedEof)));
        assert_eq!(reader.needed(), 0);
        assert!(matches!(reader.feed(b""), Err(Error::Closed)));
        let mut reader = Reader::new(Limits::default());
        assert!(matches!(reader.feed(b"bad!"), Err(Error::InvalidMagic)));
        assert!(matches!(reader.feed(b""), Err(Error::Closed)));
    }

    #[cfg(not(feature = "compression-zstd"))]
    #[test]
    fn unsupported_compression_rejected_before_frames() {
        assert!(matches!(
            Writer::new(Compression::Zstd),
            Err(Error::UnsupportedCompression)
        ));
        let mut bytes = MAGIC.to_vec();
        bytes.push(VERSION);
        bytes.extend_from_slice(
            &StreamHeader {
                compression: Compression::Zstd,
            }
            .encode(),
        );
        let mut reader = Reader::new(Limits::default());
        assert!(matches!(
            reader.feed(&bytes),
            Err(Error::UnsupportedCompression)
        ));
        assert!(matches!(reader.feed(b""), Err(Error::Closed)));
    }
}
