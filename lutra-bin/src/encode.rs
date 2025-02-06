use crate::string;
use crate::vec;

use bytes::{BufMut, BytesMut};

use crate::{Layout, Result};

pub trait Encode {
    type HeadPtr;

    fn encode(&self, buf: &mut BytesMut) {
        let ptr = self.encode_head(buf);
        self.encode_body(ptr, buf);
    }

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr;

    fn encode_body(&self, head: Self::HeadPtr, buf: &mut BytesMut);
}
impl Encode for bool {
    type HeadPtr = ();

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_u8(*self as u8);
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}

impl Encode for i8 {
    type HeadPtr = ();

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_i8(*self)
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}
impl Encode for i16 {
    type HeadPtr = ();

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_i16_le(*self)
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}
impl Encode for i32 {
    type HeadPtr = ();

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_i32_le(*self);
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}
impl Encode for i64 {
    type HeadPtr = ();

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_i64_le(*self);
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}
impl Encode for u8 {
    type HeadPtr = ();

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_u8(*self);
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}
impl Encode for u16 {
    type HeadPtr = ();

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_u16_le(*self);
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}
impl Encode for u32 {
    type HeadPtr = ();

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_u32_le(*self);
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}
impl Encode for u64 {
    type HeadPtr = ();

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_u64_le(*self);
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}
impl Encode for f32 {
    type HeadPtr = ();

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_f32_le(*self);
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}
impl Encode for f64 {
    type HeadPtr = ();

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_f64_le(*self);
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}

impl Encode for string::String {
    type HeadPtr = ReversePointer;

    fn encode_head(&self, buf: &mut BytesMut) -> ReversePointer {
        let offset = ReversePointer::new(buf);
        buf.put_u32_le(self.len() as u32);
        offset
    }

    fn encode_body(&self, offset: Self::HeadPtr, buf: &mut BytesMut) {
        offset.write_cur_len(buf);
        buf.put_slice(self.as_bytes());
    }
}
impl<E: Encode> Encode for vec::Vec<E> {
    type HeadPtr = ReversePointer;

    fn encode_head(&self, buf: &mut BytesMut) -> ReversePointer {
        let offset = ReversePointer::new(buf);
        buf.put_u32_le(self.len() as u32);
        offset
    }

    fn encode_body(&self, offset_ptr: Self::HeadPtr, buf: &mut BytesMut) {
        offset_ptr.write_cur_len(buf);

        let mut heads: vec::Vec<_> = vec::Vec::with_capacity(self.len());
        for i in self {
            heads.push(i.encode_head(buf));
        }

        for (i, h) in self.iter().zip(heads.into_iter()) {
            i.encode_body(h, buf);
        }
    }
}
impl<E: Encode + Layout> Encode for Option<E> {
    type HeadPtr = Option<Result<E::HeadPtr, ReversePointer>>;

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        let tag: u8 = if self.is_none() { 0 } else { 1 };
        buf.put_u8(tag);

        let inner_head_size = E::head_size();
        let is_inline = inner_head_size <= 4;

        if let Some(inner) = self {
            if is_inline {
                let ptr = inner.encode_head(buf);
                if inner_head_size < 4 {
                    buf.put_bytes(0, 4 - inner_head_size);
                }
                Some(Ok(ptr))
            } else {
                let offset = ReversePointer::new(buf);

                Some(Err(offset))
            }
        } else {
            buf.put_bytes(0, 4);
            None
        }
    }

    fn encode_body(&self, head: Self::HeadPtr, buf: &mut BytesMut) {
        let Some(inner) = self else { return };

        let inner_head_size = E::head_size();
        let is_inline = inner_head_size <= 4;

        let head = head.unwrap();

        if is_inline {
            let Ok(head) = head else { unreachable!() };
            inner.encode_body(head, buf)
        } else {
            let Err(offset_ptr) = head else {
                unreachable!()
            };
            offset_ptr.write_cur_len(buf);

            let head_ptr = inner.encode_head(buf);
            inner.encode_body(head_ptr, buf)
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
    pub fn new(buf: &mut BytesMut) -> ReversePointer {
        let r = ReversePointer {
            location: buf.len(),
        };
        buf.put_bytes(0, 4);
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
