use crate::layout;
use crate::string;
use crate::vec;

use bytes::{BufMut, BytesMut};

use crate::Result;

pub trait Encode {
    type HeadPtr;

    /// Size of the head of this type, in bits.
    const HEAD_BITS: u32;

    fn encode(&self) -> vec::Vec<u8> {
        let mut buf = BytesMut::new();
        let ptr = self.encode_head(&mut buf);
        self.encode_body(ptr, &mut buf);
        buf.to_vec()
    }

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr;

    fn encode_body(&self, head: Self::HeadPtr, buf: &mut BytesMut);
}

impl<T> Encode for &T
where
    T: Encode,
{
    type HeadPtr = T::HeadPtr;
    const HEAD_BITS: u32 = T::HEAD_BITS;

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        T::encode_head(self, buf)
    }

    fn encode_body(&self, head: Self::HeadPtr, buf: &mut BytesMut) {
        T::encode_body(self, head, buf)
    }
}

impl Encode for bool {
    type HeadPtr = ();
    const HEAD_BITS: u32 = 8;

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_u8(*self as u8);
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}

impl Encode for i8 {
    type HeadPtr = ();
    const HEAD_BITS: u32 = 8;

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_i8(*self)
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}
impl Encode for i16 {
    type HeadPtr = ();
    const HEAD_BITS: u32 = 16;

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_i16_le(*self)
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}
impl Encode for i32 {
    type HeadPtr = ();
    const HEAD_BITS: u32 = 32;

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_i32_le(*self);
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}
impl Encode for i64 {
    type HeadPtr = ();
    const HEAD_BITS: u32 = 64;

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_i64_le(*self);
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}
impl Encode for u8 {
    type HeadPtr = ();
    const HEAD_BITS: u32 = 8;

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_u8(*self);
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}
impl Encode for u16 {
    type HeadPtr = ();
    const HEAD_BITS: u32 = 16;

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_u16_le(*self);
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}
impl Encode for u32 {
    type HeadPtr = ();
    const HEAD_BITS: u32 = 32;

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_u32_le(*self);
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}
impl Encode for u64 {
    type HeadPtr = ();
    const HEAD_BITS: u32 = 64;

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_u64_le(*self);
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}
impl Encode for f32 {
    type HeadPtr = ();
    const HEAD_BITS: u32 = 32;

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_f32_le(*self);
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}
impl Encode for f64 {
    type HeadPtr = ();
    const HEAD_BITS: u32 = 64;

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        buf.put_f64_le(*self);
    }

    fn encode_body(&self, _head: Self::HeadPtr, _buf: &mut BytesMut) {}
}

impl Encode for str {
    type HeadPtr = ReversePointer;
    const HEAD_BITS: u32 = layout::ARRAY_HEAD_BYTES * 8;

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
impl Encode for string::String {
    type HeadPtr = ReversePointer;
    const HEAD_BITS: u32 = <str as Encode>::HEAD_BITS;

    fn encode_head(&self, buf: &mut BytesMut) -> ReversePointer {
        (self as &str).encode_head(buf)
    }

    fn encode_body(&self, offset: Self::HeadPtr, buf: &mut BytesMut) {
        (self as &str).encode_body(offset, buf)
    }
}
impl<E: Encode> Encode for vec::Vec<E> {
    type HeadPtr = ReversePointer;
    const HEAD_BITS: u32 = <[E] as Encode>::HEAD_BITS;

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        self.as_slice().encode_head(buf)
    }

    fn encode_body(&self, head: Self::HeadPtr, buf: &mut BytesMut) {
        self.as_slice().encode_body(head, buf)
    }
}
impl<E: Encode> Encode for [E] {
    type HeadPtr = ReversePointer;
    const HEAD_BITS: u32 = layout::ARRAY_HEAD_BYTES * 8;

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

        for (i, h) in self.iter().zip(heads) {
            i.encode_body(h, buf);
        }
    }
}
impl<E: Encode> Encode for Option<E> {
    type HeadPtr = Option<Result<E::HeadPtr, ReversePointer>>;
    const HEAD_BITS: u32 = layout::option_head_format::<E>().head_size();

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        let format = layout::option_head_format::<E>();

        let Some(inner) = self else {
            // the `none` variant is a unit variant: tag, then padding
            let variant = format.variant(0, 0);
            variant.encode_tag(buf);
            variant.encode_padding(buf);
            return None;
        };

        let variant = format.variant(1, E::HEAD_BITS);
        let head_ptr = match variant.encode_tag(buf) {
            Some(ptr) => Err(ptr),
            None => Ok(inner.encode_head(buf)),
        };
        variant.encode_padding(buf);
        Some(head_ptr)
    }

    fn encode_body(&self, head: Self::HeadPtr, buf: &mut BytesMut) {
        let Some(inner) = self else { return };

        match head.unwrap() {
            Err(offset_ptr) => {
                offset_ptr.write_cur_len(buf);

                let head_ptr = inner.encode_head(buf);
                inner.encode_body(head_ptr, buf)
            }
            Ok(head) => inner.encode_body(head, buf),
        }
    }
}

impl<T: Encode, E: Encode> Encode for core::result::Result<T, E> {
    type HeadPtr =
        core::result::Result<core::result::Result<T::HeadPtr, E::HeadPtr>, ReversePointer>;
    const HEAD_BITS: u32 = layout::result_head_format::<T, E>().head_size();

    fn encode_head(&self, buf: &mut BytesMut) -> Self::HeadPtr {
        let format = layout::result_head_format::<T, E>();

        let variant = match self {
            Ok(_) => format.variant(0, T::HEAD_BITS),
            Err(_) => format.variant(1, E::HEAD_BITS),
        };

        let head_ptr = match variant.encode_tag(buf) {
            Some(ptr) => Err(ptr),
            None => Ok(match self {
                Ok(inner) => Ok(inner.encode_head(buf)),
                Err(inner) => Err(inner.encode_head(buf)),
            }),
        };
        variant.encode_padding(buf);
        head_ptr
    }

    fn encode_body(&self, head: Self::HeadPtr, buf: &mut BytesMut) {
        match head {
            Err(ptr) => {
                ptr.write_cur_len(buf);
                match self {
                    Ok(inner) => {
                        let hp = inner.encode_head(buf);
                        inner.encode_body(hp, buf);
                    }
                    Err(inner) => {
                        let hp = inner.encode_head(buf);
                        inner.encode_body(hp, buf);
                    }
                }
            }
            Ok(Ok(head)) => {
                let Ok(inner) = self else { unreachable!() };
                inner.encode_body(head, buf);
            }
            Ok(Err(head)) => {
                let Err(inner) = self else { unreachable!() };
                inner.encode_body(head, buf);
            }
        }
    }
}

impl Encode for () {
    type HeadPtr = ();
    const HEAD_BITS: u32 = 0;

    fn encode_head(&self, _buf: &mut BytesMut) {}

    fn encode_body(&self, _: (), _buf: &mut BytesMut) {}
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
        buf.put_bytes(0, layout::PTR_BYTES as usize);
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
        let end = self.location + layout::PTR_BYTES as usize;
        w[self.location..end].copy_from_slice(&(relative as u32).to_le_bytes());
    }
}
