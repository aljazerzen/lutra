use std::ops::Div;

#[derive(Clone)]
pub struct Reader<'a> {
    buf: &'a [u8],
}

impl<'a> Reader<'a> {
    pub fn new(buf: &'a [u8]) -> Self {
        Reader { buf }
    }

    pub fn copy_n(&mut self, n: usize) -> &[u8] {
        let r = &self.buf[..n];
        self.buf = &self.buf[n..];
        r
    }

    pub fn copy_const<const N: usize>(&mut self) -> [u8; N] {
        let r = self.buf[..N].try_into().unwrap();
        self.buf = &self.buf[N..];
        r
    }

    pub fn skip(&mut self, byte_count: usize) {
        self.buf = &self.buf[byte_count..];
    }

    pub fn remaining(&self) -> usize {
        self.buf.len()
    }
}

pub struct ArrayReader<'b> {
    item_head_bytes: usize,
    next: Reader<'b>,
    remaining: usize,
}

impl<'b> ArrayReader<'b> {
    pub fn new(head: &mut Reader<'b>, item_head_size: usize) -> Self {
        let mut body = head.clone();

        let offset = head.copy_const::<4>();
        let offset = u32::from_le_bytes(offset) as usize;
        body.skip(offset);

        let len = head.copy_const::<4>();
        let len = u32::from_le_bytes(len) as usize;
        ArrayReader {
            item_head_bytes: item_head_size.div(8),
            next: body,
            remaining: len,
        }
    }
}

impl<'b> Iterator for ArrayReader<'b> {
    type Item = Reader<'b>;

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
