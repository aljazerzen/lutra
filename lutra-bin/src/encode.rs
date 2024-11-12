use std::io::Write;

use crate::Result;

pub trait Encode {
    type HeadPtr;

    fn encode(&self, w: &mut Vec<u8>) -> Result<()> {
        let ptr = self.encode_head(w)?;
        self.encode_body(ptr, w)?;
        Ok(())
    }

    fn encode_head(&self, w: &mut Vec<u8>) -> Result<Self::HeadPtr>;

    fn encode_body(&self, head: Self::HeadPtr, w: &mut Vec<u8>) -> Result<()>;
}

impl Encode for i64 {
    type HeadPtr = ();

    fn encode_head(&self, w: &mut Vec<u8>) -> Result<Self::HeadPtr> {
        Ok(w.write_all(&self.to_le_bytes())?)
    }

    fn encode_body(&self, _head: Self::HeadPtr, _w: &mut Vec<u8>) -> Result<()> {
        Ok(())
    }
}
impl Encode for f64 {
    type HeadPtr = ();

    fn encode_head(&self, w: &mut Vec<u8>) -> Result<Self::HeadPtr> {
        Ok(w.write_all(&self.to_le_bytes())?)
    }

    fn encode_body(&self, _head: Self::HeadPtr, _w: &mut Vec<u8>) -> Result<()> {
        Ok(())
    }
}
impl Encode for bool {
    type HeadPtr = ();

    fn encode_head(&self, w: &mut Vec<u8>) -> Result<Self::HeadPtr> {
        Ok(w.write_all(&[(*self as u8)])?)
    }

    fn encode_body(&self, _head: Self::HeadPtr, _w: &mut Vec<u8>) -> Result<()> {
        Ok(())
    }
}
impl Encode for String {
    type HeadPtr = OffsetPointer;

    fn encode_head(&self, w: &mut Vec<u8>) -> Result<OffsetPointer> {
        let offset = OffsetPointer::new(w);
        w.write_all(&(self.len() as u32).to_le_bytes())?;
        Ok(offset)
    }

    fn encode_body(&self, head: Self::HeadPtr, w: &mut Vec<u8>) -> Result<()> {
        head.write(w);
        w.write_all(self.as_bytes())?;
        Ok(())
    }
}
impl<E: Encode> Encode for Vec<E> {
    type HeadPtr = OffsetPointer;

    fn encode_head(&self, w: &mut Vec<u8>) -> Result<OffsetPointer> {
        let offset = OffsetPointer::new(w);
        w.write_all(&(self.len() as u32).to_le_bytes())?;
        Ok(offset)
    }

    fn encode_body(&self, offset_ptr: Self::HeadPtr, w: &mut Vec<u8>) -> Result<()> {
        offset_ptr.write(w);

        let mut heads: Vec<_> = Vec::with_capacity(self.len());
        for i in self {
            heads.push(i.encode_head(w)?);
        }

        for (i, h) in self.iter().zip(heads.into_iter()) {
            i.encode_body(h, w)?;
        }
        Ok(())
    }
}

/// Pointer to a location where an offset should be written to.
pub struct OffsetPointer {
    /// Location offset within the buffer.
    ptr: usize,
}

impl OffsetPointer {
    pub fn new(w: &mut Vec<u8>) -> OffsetPointer {
        let r = OffsetPointer { ptr: w.len() };
        w.extend([0_u8; 4]);
        r
    }

    pub fn write(self, w: &mut [u8]) {
        let offset = w.len() - self.ptr;
        w[self.ptr..(self.ptr + 4)].copy_from_slice(&(offset as u32).to_le_bytes());
    }
}

#[derive(Clone)]
pub struct Reader<'a> {
    buf: &'a [u8],
    current_pos: usize,
}

impl<'a> Reader<'a> {
    pub fn new(buf: &'a [u8], current_pos: usize) -> Self {
        Reader { buf, current_pos }
    }

    pub fn copy_n(&mut self, n: usize) -> Vec<u8> {
        let r = self.buf[self.current_pos..][..n].to_vec();
        self.current_pos += n;
        r
    }

    pub fn copy_const<const N: usize>(&mut self) -> [u8; N] {
        let r = self.buf[self.current_pos..][..N].try_into().unwrap();
        self.current_pos += N;
        r
    }

    pub fn rewind(&mut self, byte_count: usize) {
        self.current_pos -= byte_count;
    }

    pub fn skip(&mut self, byte_count: usize) {
        self.current_pos += byte_count;
    }
}

pub trait Decode: Sized {
    fn decode_buffer(bytes: &[u8]) -> Result<Self> {
        let mut reader = Reader::new(bytes, 0);
        Self::decode(&mut reader)
    }

    fn decode(r: &mut Reader<'_>) -> Result<Self>;
}

impl Decode for i64 {
    fn decode(r: &mut Reader<'_>) -> Result<Self> {
        let bytes = r.copy_const::<8>();
        Ok(i64::from_le_bytes(bytes))
    }
}
impl Decode for f64 {
    fn decode(r: &mut Reader<'_>) -> Result<Self> {
        let bytes = r.copy_const::<8>();
        Ok(f64::from_le_bytes(bytes))
    }
}
impl Decode for bool {
    fn decode(r: &mut Reader<'_>) -> Result<Self> {
        let [v] = r.copy_const::<1>();
        Ok(v != 0)
    }
}
impl Decode for String {
    fn decode(r: &mut Reader<'_>) -> Result<Self> {
        let mut body = r.clone();

        let offset = r.copy_const::<4>();
        let offset = u32::from_le_bytes(offset) as usize;
        body.skip(offset);

        let len = r.copy_const::<4>();
        let len = u32::from_le_bytes(len) as usize;

        let buf = body.copy_n(len);
        Ok(String::from_utf8(buf).unwrap())
    }
}
impl<E: Decode> Decode for Vec<E> {
    fn decode(r: &mut Reader<'_>) -> Result<Self> {
        let mut body = r.clone();

        let offset = r.copy_const::<4>();
        let offset = u32::from_le_bytes(offset) as usize;
        body.skip(offset);

        let len = r.copy_const::<4>();
        let len = u32::from_le_bytes(len) as usize;

        let mut buf = Vec::with_capacity(len);
        for _ in 0..len {
            buf.push(E::decode(&mut body)?);
        }

        Ok(buf)
    }
}
