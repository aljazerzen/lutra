use crate::borrow;

use crate::ir;
use crate::layout;

pub trait ReaderExt {
    fn read_n(&self, n: usize) -> &[u8];

    fn read_const<const N: usize>(&self) -> [u8; N];

    #[must_use]
    fn skip(self, bytes: usize) -> Self;
}

impl ReaderExt for &[u8] {
    fn read_n(&self, n: usize) -> &[u8] {
        &self[..n]
    }

    fn read_const<const N: usize>(&self) -> [u8; N] {
        self[0..N].try_into().unwrap()
    }

    fn skip(self, bytes: usize) -> Self {
        &self[bytes..]
    }
}

#[derive(Clone)]
pub struct ArrayReader<B: bytes::Buf + Clone> {
    item_head_bytes: usize,
    buf_next: B,
    remaining: usize,
}

impl<B> ArrayReader<B>
where
    B: bytes::Buf + Clone,
{
    pub fn new_for_ty(buf: B, ty: &ir::Ty) -> Self {
        let ir::TyKind::Array(items_ty) = &ty.kind else {
            panic!()
        };
        let item_head_size = items_ty.layout.as_ref().unwrap().head_size as usize;

        Self::new(buf, item_head_size.div_ceil(8))
    }

    pub fn new(buf: B, item_head_bytes: usize) -> Self {
        let (offset, len) = Self::read_head(buf.chunk());

        let mut body = buf.clone();
        body.advance(offset);

        ArrayReader {
            item_head_bytes,
            buf_next: body,
            remaining: len,
        }
    }

    pub fn remaining(&self) -> usize {
        self.remaining
    }

    pub fn read_head(buffer: &[u8]) -> (usize, usize) {
        let offset = u32::from_le_bytes(buffer.read_const::<4>());

        let len = u32::from_le_bytes(buffer.skip(4).read_const::<4>());
        (offset as usize, len as usize)
    }

    pub fn get(&self, index: usize) -> Option<B> {
        if index >= self.remaining {
            return None;
        }

        let mut data = self.buf_next.clone();
        data.advance(self.item_head_bytes * index);
        Some(data)
    }
}

impl<B> Iterator for ArrayReader<B>
where
    B: bytes::Buf + Clone,
{
    type Item = B;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining > 0 {
            let this = self.buf_next.clone();
            self.buf_next.advance(self.item_head_bytes);
            self.remaining -= 1;
            Some(this)
        } else {
            None
        }
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        let n = if n > self.remaining {
            self.remaining
        } else {
            n
        };
        self.buf_next.advance(self.item_head_bytes * n);
        self.remaining -= n;
        self.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }

    fn count(self) -> usize
    where
        Self: Sized,
    {
        self.remaining
    }
}

pub struct TupleReader<'t, B> {
    buf: B,
    field_offsets: borrow::Cow<'t, [u32]>,
}

impl<'t, B> TupleReader<'t, B>
where
    B: bytes::Buf + Clone,
{
    pub fn new(buf: B, field_offsets: borrow::Cow<'t, [u32]>) -> Self {
        TupleReader { buf, field_offsets }
    }

    pub fn new_for_ty(buf: B, ty: &ir::Ty) -> Self {
        let field_offsets = layout::tuple_field_offsets(ty);
        Self::new(buf, field_offsets.into())
    }

    pub fn get_field(&self, index: usize) -> B {
        let mut r = self.buf.clone();
        r.advance(self.field_offsets[index] as usize);
        r
    }
}
