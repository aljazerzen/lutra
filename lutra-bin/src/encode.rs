use std::io::Write;

use crate::layout::Layout;
use crate::Result;

pub trait Encode {
    type BodyMeta;

    fn encode(&self, w: &mut Vec<u8>) -> Result<()> {
        let meta = self.encode_body(w)?;
        self.encode_head(meta, w)?;
        Ok(())
    }

    fn encode_head(&self, body_meta: Self::BodyMeta, w: &mut Vec<u8>) -> Result<()>;

    fn encode_body(&self, w: &mut Vec<u8>) -> Result<Self::BodyMeta>;
}

impl Encode for i64 {
    type BodyMeta = ();

    fn encode_body(&self, _w: &mut Vec<u8>) -> Result<Self::BodyMeta> {
        Ok(())
    }

    fn encode_head(&self, _: (), w: &mut Vec<u8>) -> Result<()> {
        Ok(w.write_all(&self.to_le_bytes())?)
    }
}
impl Encode for f64 {
    type BodyMeta = ();

    fn encode_body(&self, _w: &mut Vec<u8>) -> Result<Self::BodyMeta> {
        Ok(())
    }

    fn encode_head(&self, _: (), w: &mut Vec<u8>) -> Result<()> {
        Ok(w.write_all(&self.to_le_bytes())?)
    }
}
impl Encode for bool {
    type BodyMeta = ();

    fn encode_body(&self, _w: &mut Vec<u8>) -> Result<Self::BodyMeta> {
        Ok(())
    }

    fn encode_head(&self, _: (), w: &mut Vec<u8>) -> Result<()> {
        Ok(w.write_all(&[(*self as u8)])?)
    }
}
impl Encode for String {
    type BodyMeta = usize;

    fn encode_body(&self, w: &mut Vec<u8>) -> Result<Self::BodyMeta> {
        let bytes_start = w.len();
        w.write_all(self.as_bytes())?;
        Ok(bytes_start)
    }

    fn encode_head(&self, bytes_start: usize, w: &mut Vec<u8>) -> Result<()> {
        let offset = w.len() - bytes_start;

        w.write_all(&(offset as u32).to_le_bytes())?;
        w.write_all(&(self.len() as u32).to_le_bytes())?;
        Ok(())
    }
}
impl<E: Encode> Encode for Vec<E> {
    type BodyMeta = usize;

    fn encode_body(&self, w: &mut Vec<u8>) -> Result<Self::BodyMeta> {
        let mut body_metas: Vec<_> = Vec::with_capacity(self.len());
        for i in self {
            body_metas.push(i.encode_body(w)?);
        }

        let bytes_start = w.len();
        for (i, b) in self.iter().zip(body_metas.into_iter()) {
            i.encode_head(b, w)?;
        }
        Ok(bytes_start)
    }

    fn encode_head(&self, bytes_start: usize, w: &mut Vec<u8>) -> Result<()> {
        let offset = w.len() - bytes_start;

        w.write_all(&(offset as u32).to_le_bytes())?;
        w.write_all(&(self.len() as u32).to_le_bytes())?;
        Ok(())
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

pub trait Decode: Layout + Sized {
    fn decode_buffer(bytes: &[u8]) -> Result<Self> {
        let s = Self::head_size() / 8;
        let head_offset = bytes.len() - s;
        let mut reader = Reader::new(bytes, head_offset);

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
        body.rewind(offset);

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
        body.rewind(offset);

        let len = r.copy_const::<4>();
        let len = u32::from_le_bytes(len) as usize;

        let mut buf = Vec::with_capacity(len);
        for _ in 0..len {
            buf.push(E::decode(&mut body)?);
        }

        Ok(buf)
    }
}
