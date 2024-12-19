use std::borrow::Cow;
use std::rc::Rc;
use std::sync::Mutex;

use crate::ir;
use crate::Data;

#[derive(Clone, Debug)]
pub struct Reader<'a> {
    buf: &'a [u8],
    most_read: Rc<Mutex<&'a [u8]>>,
}

impl<'a> Reader<'a> {
    pub fn new(buf: &'a [u8]) -> Self {
        Reader {
            buf,
            most_read: Rc::new(Mutex::new(buf)),
        }
    }

    pub fn read_n(&mut self, n: usize) -> &[u8] {
        let r = &self.buf[..n];
        self.buf = &self.buf[n..];
        r
    }

    pub fn read_const<const N: usize>(&mut self) -> [u8; N] {
        let r = self.buf[..N].try_into().unwrap();
        self.buf = &self.buf[N..];
        r
    }

    pub fn skip(&mut self, byte_count: usize) {
        self.buf = &self.buf[byte_count..];
    }

    pub fn skip_read<'b: 'a>(&mut self) {
        let mut most_read = self.most_read.lock().unwrap();
        if most_read.len() > self.buf.len() {
            *most_read = self.buf;
        }
        if most_read.len() < self.buf.len() {
            self.buf = *most_read;
        }
    }

    fn update_most_read(&self) {
        let mut most_read = self.most_read.lock().unwrap();
        if most_read.len() > self.buf.len() {
            *most_read = self.buf;
        }
    }

    pub fn remaining(&self) -> usize {
        self.buf.len()
    }

    pub fn to_owned(&self) -> Vec<u8> {
        self.buf.to_owned()
    }
}

impl<'a> Drop for Reader<'a> {
    fn drop(&mut self) {
        self.update_most_read();
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

        Self::new(data, item_head_size / 8)
    }

    pub fn new(data: Data, item_head_bytes: usize) -> Self {
        let mut head = Reader::new(data.slice(8));
        let (offset, len) = Self::read_head(&mut head);

        let mut body = data.clone();
        body.skip(offset);

        ArrayReader {
            item_head_bytes,
            next: body,
            remaining: len,
        }
    }

    pub fn read_head(reader: &mut Reader) -> (usize, usize) {
        let offset = reader.read_const::<4>();
        let offset = u32::from_le_bytes(offset);

        let len = reader.read_const::<4>();
        let len = u32::from_le_bytes(len);
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
    field_offsets: Cow<'t, [u32]>,
}

impl<'d, 't> TupleReader<'d, 't> {
    pub fn new(data: &'d Data, field_offsets: Cow<'t, [u32]>) -> Self {
        TupleReader {
            data,
            field_offsets,
        }
    }

    pub fn new_for_ty(data: &'d Data, ty: &ir::Ty) -> Self {
        let field_offsets = Self::compute_field_offsets(ty);

        Self::new(data, field_offsets.into())
    }

    pub fn compute_field_offsets(ty: &ir::Ty) -> Vec<u32> {
        let ir::TyKind::Tuple(ty_fields) = &ty.kind else {
            panic!()
        };

        let mut field_offsets = Vec::with_capacity(ty_fields.len());
        let mut offset = 0_u32;
        for field in ty_fields {
            field_offsets.push(offset);

            let layout = field.ty.layout.as_ref().unwrap();
            offset += (layout.head_size).div_ceil(8);
        }
        field_offsets
    }

    pub fn compute_field_offset(ty: &ir::Ty, position: u16) -> u32 {
        *Self::compute_field_offsets(ty)
            .get(position as usize)
            .unwrap()
    }

    pub fn get_field(&self, index: usize) -> Data {
        let mut r = self.data.clone();
        r.skip(self.field_offsets[index] as usize);
        r
    }
}
