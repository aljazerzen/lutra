use crate::{string, vec};

use crate::{reader, Layout, ReaderExt, Result};

pub trait Decode: Sized + Layout {
    fn decode(buffer: &[u8]) -> Result<Self>;
}
impl Decode for bool {
    fn decode(r: &[u8]) -> Result<Self> {
        let [v] = r.read_const::<1>();
        Ok(v != 0)
    }
}

impl Decode for i8 {
    fn decode(r: &[u8]) -> Result<Self> {
        Ok(i8::from_le_bytes(r.read_const()))
    }
}
impl Decode for i16 {
    fn decode(r: &[u8]) -> Result<Self> {
        Ok(i16::from_le_bytes(r.read_const()))
    }
}
impl Decode for i32 {
    fn decode(r: &[u8]) -> Result<Self> {
        Ok(i32::from_le_bytes(r.read_const()))
    }
}
impl Decode for i64 {
    fn decode(r: &[u8]) -> Result<Self> {
        Ok(i64::from_le_bytes(r.read_const()))
    }
}
impl Decode for u8 {
    fn decode(r: &[u8]) -> Result<Self> {
        Ok(u8::from_le_bytes(r.read_const()))
    }
}
impl Decode for u16 {
    fn decode(r: &[u8]) -> Result<Self> {
        Ok(u16::from_le_bytes(r.read_const()))
    }
}
impl Decode for u32 {
    fn decode(r: &[u8]) -> Result<Self> {
        Ok(u32::from_le_bytes(r.read_const()))
    }
}
impl Decode for u64 {
    fn decode(r: &[u8]) -> Result<Self> {
        Ok(u64::from_le_bytes(r.read_const()))
    }
}
impl Decode for f32 {
    fn decode(r: &[u8]) -> Result<Self> {
        Ok(f32::from_le_bytes(r.read_const()))
    }
}
impl Decode for f64 {
    fn decode(r: &[u8]) -> Result<Self> {
        Ok(f64::from_le_bytes(r.read_const()))
    }
}

impl Decode for string::String {
    fn decode(r: &[u8]) -> Result<Self> {
        let offset = u32::from_le_bytes(r.read_const()) as usize;
        let len = u32::from_le_bytes(r.skip(4).read_const()) as usize;

        let buf = r.skip(offset).read_n(len).to_vec();
        Ok(string::String::from_utf8(buf).unwrap())
    }
}
impl<E: Decode> Decode for vec::Vec<E> {
    fn decode(r: &[u8]) -> Result<Self> {
        let (offset, len) = reader::ArrayReader::read_head(r);
        let mut buf = r.skip(offset);

        let mut out = vec::Vec::with_capacity(len);
        let item_head_bytes = E::head_size().div_ceil(8);
        for _ in 0..len {
            out.push(E::decode(buf)?);
            buf = buf.skip(item_head_bytes);
        }
        Ok(out)
    }
}
impl<E: Decode> Decode for Option<E> {
    fn decode(buf: &[u8]) -> Result<Self> {
        let [tag] = buf.read_const::<1>();
        if tag == 0 {
            Ok(None)
        } else {
            let buf = buf.skip(1);
            let inner_head_size = E::head_size();
            let has_ptr = inner_head_size > 32;

            Ok(Some(if has_ptr {
                let offset = u32::from_le_bytes(buf.read_const::<4>()) as usize;
                E::decode(buf.skip(offset))?
            } else {
                E::decode(buf)?
            }))
        }
    }
}
impl Decode for () {
    fn decode(_buf: &[u8]) -> Result<Self> {
        Ok(())
    }
}
