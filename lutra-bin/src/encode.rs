use std::io::Write;

use crate::reader::{self};
use crate::{Layout, Reader, Result};

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
impl Encode for bool {
    type HeadPtr = ();

    fn encode_head(&self, w: &mut Vec<u8>) -> Result<Self::HeadPtr> {
        Ok(w.write_all(&[(*self as u8)])?)
    }

    fn encode_body(&self, _head: Self::HeadPtr, _w: &mut Vec<u8>) -> Result<()> {
        Ok(())
    }
}

impl Encode for i8 {
    type HeadPtr = ();

    fn encode_head(&self, w: &mut Vec<u8>) -> Result<Self::HeadPtr> {
        Ok(w.write_all(&self.to_le_bytes())?)
    }

    fn encode_body(&self, _head: Self::HeadPtr, _w: &mut Vec<u8>) -> Result<()> {
        Ok(())
    }
}
impl Encode for i16 {
    type HeadPtr = ();

    fn encode_head(&self, w: &mut Vec<u8>) -> Result<Self::HeadPtr> {
        Ok(w.write_all(&self.to_le_bytes())?)
    }

    fn encode_body(&self, _head: Self::HeadPtr, _w: &mut Vec<u8>) -> Result<()> {
        Ok(())
    }
}
impl Encode for i32 {
    type HeadPtr = ();

    fn encode_head(&self, w: &mut Vec<u8>) -> Result<Self::HeadPtr> {
        Ok(w.write_all(&self.to_le_bytes())?)
    }

    fn encode_body(&self, _head: Self::HeadPtr, _w: &mut Vec<u8>) -> Result<()> {
        Ok(())
    }
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
impl Encode for u8 {
    type HeadPtr = ();

    fn encode_head(&self, w: &mut Vec<u8>) -> Result<Self::HeadPtr> {
        Ok(w.write_all(&self.to_le_bytes())?)
    }

    fn encode_body(&self, _head: Self::HeadPtr, _w: &mut Vec<u8>) -> Result<()> {
        Ok(())
    }
}
impl Encode for u16 {
    type HeadPtr = ();

    fn encode_head(&self, w: &mut Vec<u8>) -> Result<Self::HeadPtr> {
        Ok(w.write_all(&self.to_le_bytes())?)
    }

    fn encode_body(&self, _head: Self::HeadPtr, _w: &mut Vec<u8>) -> Result<()> {
        Ok(())
    }
}
impl Encode for u32 {
    type HeadPtr = ();

    fn encode_head(&self, w: &mut Vec<u8>) -> Result<Self::HeadPtr> {
        Ok(w.write_all(&self.to_le_bytes())?)
    }

    fn encode_body(&self, _head: Self::HeadPtr, _w: &mut Vec<u8>) -> Result<()> {
        Ok(())
    }
}
impl Encode for u64 {
    type HeadPtr = ();

    fn encode_head(&self, w: &mut Vec<u8>) -> Result<Self::HeadPtr> {
        Ok(w.write_all(&self.to_le_bytes())?)
    }

    fn encode_body(&self, _head: Self::HeadPtr, _w: &mut Vec<u8>) -> Result<()> {
        Ok(())
    }
}
impl Encode for f32 {
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

impl Encode for String {
    type HeadPtr = ReversePointer;

    fn encode_head(&self, w: &mut Vec<u8>) -> Result<ReversePointer> {
        let offset = ReversePointer::new(w);
        w.write_all(&(self.len() as u32).to_le_bytes())?;
        Ok(offset)
    }

    fn encode_body(&self, head: Self::HeadPtr, w: &mut Vec<u8>) -> Result<()> {
        head.write_cur_len(w);
        w.write_all(self.as_bytes())?;
        Ok(())
    }
}
impl<E: Encode> Encode for Vec<E> {
    type HeadPtr = ReversePointer;

    fn encode_head(&self, w: &mut Vec<u8>) -> Result<ReversePointer> {
        let offset = ReversePointer::new(w);
        w.write_all(&(self.len() as u32).to_le_bytes())?;
        Ok(offset)
    }

    fn encode_body(&self, offset_ptr: Self::HeadPtr, w: &mut Vec<u8>) -> Result<()> {
        offset_ptr.write_cur_len(w);

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
impl<E: Encode + Layout> Encode for Option<E> {
    type HeadPtr = Option<Result<E::HeadPtr, ReversePointer>>;

    fn encode_head(&self, w: &mut Vec<u8>) -> Result<Self::HeadPtr> {
        let tag: u8 = if self.is_none() { 0 } else { 1 };
        w.write_all(&[tag])?;

        let inner_head_size = E::head_size();
        let is_inline = inner_head_size <= 4;

        Ok(if let Some(inner) = self {
            if is_inline {
                let ptr = inner.encode_head(w)?;
                if inner_head_size < 4 {
                    w.write_all(&vec![0; 4 - inner_head_size])?;
                }
                Some(Ok(ptr))
            } else {
                let offset = ReversePointer::new(w);

                Some(Err(offset))
            }
        } else {
            w.write_all(&[0, 0, 0, 0])?;
            None
        })
    }

    fn encode_body(&self, head: Self::HeadPtr, w: &mut Vec<u8>) -> Result<()> {
        let Some(inner) = self else { return Ok(()) };

        let inner_head_size = E::head_size();
        let is_inline = inner_head_size <= 4;

        let head = head.unwrap();

        if is_inline {
            let Ok(head) = head else { unreachable!() };
            inner.encode_body(head, w)
        } else {
            let Err(offset_ptr) = head else {
                unreachable!()
            };
            offset_ptr.write_cur_len(w);

            let head_ptr = inner.encode_head(w)?;
            inner.encode_body(head_ptr, w)
        }
    }
}

/// Pointer to a location where an offset should be written to.
#[derive(Debug, Clone, Copy)]
pub struct ReversePointer {
    /// Location offset within the buffer.
    location: usize,
}

impl ReversePointer {
    pub fn new(w: &mut Vec<u8>) -> ReversePointer {
        let r = ReversePointer { location: w.len() };
        w.extend([0_u8; 4]);
        r
    }

    pub fn new_at(location: usize) -> ReversePointer {
        ReversePointer { location }
    }

    pub fn unwrap(&self) -> usize {
        self.location
    }

    pub fn write_cur_len(self, w: &mut [u8]) {
        self.write(w, w.len())
    }

    pub fn write(self, w: &mut [u8], absolute_ptr: usize) {
        let relative = absolute_ptr - self.location;
        w[self.location..(self.location + 4)].copy_from_slice(&(relative as u32).to_le_bytes());
    }
}

pub trait Decode: Sized + Layout {
    fn decode_buffer(bytes: &[u8]) -> Result<Self> {
        let mut reader = Reader::new(bytes);
        let res = Self::decode(&mut reader);

        // sanity check that correct number of bytes have been read
        #[cfg(debug_assertions)]
        {
            let bytes_read = bytes.len() - reader.remaining();
            assert_eq!(bytes_read, Self::head_size().div_ceil(8));
        }

        res
    }

    fn decode(r: &mut Reader<'_>) -> Result<Self>;
}
impl Decode for bool {
    fn decode(r: &mut Reader<'_>) -> Result<Self> {
        let [v] = r.read_const::<1>();
        Ok(v != 0)
    }
}

impl Decode for i8 {
    fn decode(r: &mut Reader<'_>) -> Result<Self> {
        let bytes = r.read_const();
        Ok(i8::from_le_bytes(bytes))
    }
}
impl Decode for i16 {
    fn decode(r: &mut Reader<'_>) -> Result<Self> {
        let bytes = r.read_const();
        Ok(i16::from_le_bytes(bytes))
    }
}
impl Decode for i32 {
    fn decode(r: &mut Reader<'_>) -> Result<Self> {
        let bytes = r.read_const();
        Ok(i32::from_le_bytes(bytes))
    }
}
impl Decode for i64 {
    fn decode(r: &mut Reader<'_>) -> Result<Self> {
        let bytes = r.read_const();
        Ok(i64::from_le_bytes(bytes))
    }
}
impl Decode for u8 {
    fn decode(r: &mut Reader<'_>) -> Result<Self> {
        let bytes = r.read_const();
        Ok(u8::from_le_bytes(bytes))
    }
}
impl Decode for u16 {
    fn decode(r: &mut Reader<'_>) -> Result<Self> {
        let bytes = r.read_const();
        Ok(u16::from_le_bytes(bytes))
    }
}
impl Decode for u32 {
    fn decode(r: &mut Reader<'_>) -> Result<Self> {
        let bytes = r.read_const();
        Ok(u32::from_le_bytes(bytes))
    }
}
impl Decode for u64 {
    fn decode(r: &mut Reader<'_>) -> Result<Self> {
        let bytes = r.read_const();
        Ok(u64::from_le_bytes(bytes))
    }
}
impl Decode for f32 {
    fn decode(r: &mut Reader<'_>) -> Result<Self> {
        let bytes = r.read_const();
        Ok(f32::from_le_bytes(bytes))
    }
}
impl Decode for f64 {
    fn decode(r: &mut Reader<'_>) -> Result<Self> {
        let bytes = r.read_const();
        Ok(f64::from_le_bytes(bytes))
    }
}

impl Decode for String {
    fn decode(r: &mut Reader<'_>) -> Result<Self> {
        let mut body = r.clone();

        let offset = r.read_const::<4>();
        let offset = u32::from_le_bytes(offset) as usize;
        body.skip(offset);

        let len = r.read_const::<4>();
        let len = u32::from_le_bytes(len) as usize;

        let buf = body.read_n(len).to_vec();
        Ok(String::from_utf8(buf).unwrap())
    }
}
impl<E: Decode> Decode for Vec<E> {
    fn decode(r: &mut Reader<'_>) -> Result<Self> {
        let mut body = r.clone();
        let (offset, len) = reader::ArrayReader::read_head(r);
        body.skip(offset);

        let mut buf = Vec::with_capacity(len);
        for _ in 0..len {
            buf.push(E::decode(&mut body)?);
        }
        Ok(buf)
    }
}
impl<E: Decode> Decode for Option<E> {
    fn decode(r: &mut Reader<'_>) -> Result<Self> {
        let [tag] = r.read_const::<1>();
        if tag == 0 {
            r.skip(4);
            Ok(None)
        } else {
            let inner_head_size = E::head_size();
            let is_inline = inner_head_size <= 4;

            Ok(Some(if is_inline {
                let res = E::decode(r)?;
                if inner_head_size < 4 {
                    r.skip(4 - inner_head_size);
                }
                res
            } else {
                let mut body = r.clone();
                let offset = r.read_const::<4>();
                let offset = u32::from_le_bytes(offset) as usize;
                body.skip(offset);
                E::decode(&mut body)?
            }))
        }
    }
}
