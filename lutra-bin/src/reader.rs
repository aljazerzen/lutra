use bytes::Buf;

use crate::{borrow, vec};

use crate::ir;
use crate::layout;
use crate::Data;

pub trait ReaderExt {
    fn read_n(&self, n: usize) -> &[u8];

    fn read_const<const N: usize>(&self) -> [u8; N];

    fn skip(self, bytes: usize) -> Self;
}

impl<'a> ReaderExt for &'a [u8] {
    fn read_n(&self, n: usize) -> &[u8] {
        &self[..n]
    }

    fn read_const<const N: usize>(&self) -> [u8; N] {
        self[0..N].try_into().unwrap()
    }

    #[must_use]
    fn skip(self, bytes: usize) -> Self {
        &self[bytes..]
    }
}

#[derive(Clone)]
pub struct ArrayReader {
    item_head_bytes: usize,
    next: Data,
    remaining: usize,
}

impl ArrayReader {
    pub fn new_for_ty(data: Data, ty: &ir::Ty) -> Self {
        let ir::TyKind::Array(items_ty) = &ty.kind else {
            panic!()
        };
        let item_head_size = items_ty.layout.as_ref().unwrap().head_size as usize;

        Self::new(data, item_head_size.div_ceil(8))
    }

    pub fn new(data: Data, item_head_bytes: usize) -> Self {
        let (offset, len) = Self::read_head(data.slice(8));

        let mut body = data.clone();
        body.skip(offset);

        ArrayReader {
            item_head_bytes,
            next: body,
            remaining: len,
        }
    }

    pub fn read_head(buffer: &[u8]) -> (usize, usize) {
        let offset = u32::from_le_bytes(buffer.read_const::<4>());

        let len = u32::from_le_bytes(buffer.skip(4).read_const::<4>());
        (offset as usize, len as usize)
    }

    pub fn remaining(&self) -> usize {
        self.remaining
    }

    pub fn get(&self, index: usize) -> Option<Data> {
        if index >= self.remaining {
            return None;
        }

        let mut data = self.next.clone();
        data.skip(self.item_head_bytes * index);
        Some(data)
    }
}

impl Iterator for ArrayReader {
    type Item = Data;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining > 0 {
            let this = self.next.clone();
            self.next.skip(self.item_head_bytes);
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
        self.next.skip(self.item_head_bytes * n);
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

pub struct TupleReader<'d, 't> {
    data: &'d Data,
    field_offsets: borrow::Cow<'t, [u32]>,
}

impl<'d, 't> TupleReader<'d, 't> {
    pub fn new(data: &'d Data, field_offsets: borrow::Cow<'t, [u32]>) -> Self {
        TupleReader {
            data,
            field_offsets,
        }
    }

    pub fn new_for_ty(data: &'d Data, ty: &ir::Ty) -> Self {
        let field_offsets = layout::tuple_field_offsets(ty);
        Self::new(data, field_offsets.into())
    }

    pub fn get_field(&self, index: usize) -> Data {
        let mut r = self.data.clone();
        r.skip(self.field_offsets[index] as usize);
        r
    }
}

pub struct EnumReader {
    data: Data,
    tag: u64,
}

impl EnumReader {
    pub fn new(data: Data, tag_bytes: u32, has_ptr: bool) -> Self {
        let tag_bytes = tag_bytes as usize;

        let mut tag = vec![0; 8];
        data.slice(tag_bytes).copy_to_slice(&mut tag[0..tag_bytes]);
        let tag = u64::from_le_bytes(tag.try_into().unwrap()) as u64;

        let mut data = data;
        data.skip(tag_bytes);

        if has_ptr {
            // read ptr and dereference
            let offset = u32::from_le_bytes(data.slice(4).read_const::<4>());
            data.skip(offset as usize);
        } else {
            // inner is right after the tag
        }

        EnumReader { data, tag }
    }

    pub fn new_for_ty(data: Data, ty: &ir::Ty) -> Self {
        let ir::TyKind::Enum(variants) = &ty.kind else {
            panic!()
        };
        let head = layout::enum_head_format(variants);
        Self::new(data, head.tag_bytes, head.has_ptr)
    }

    pub fn get_tag(&self) -> u64 {
        self.tag
    }

    pub fn get_inner(self) -> Data {
        self.data
    }
}
