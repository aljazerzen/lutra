use lutra_frontend::pr;

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

    pub fn to_owned(&self) -> Vec<u8> {
        self.buf.to_owned()
    }
}

pub struct ArrayReader<'b> {
    item_head_bytes: usize,
    next: Reader<'b>,
    remaining: usize,
}

impl<'b> ArrayReader<'b> {
    pub fn new_for_ty(head: &mut Reader<'b>, ty: &pr::Ty) -> Self {
        let pr::TyKind::Array(items_ty) = &ty.kind else {
            panic!()
        };
        let item_head_size = items_ty.layout.as_ref().unwrap().head_size as usize;

        Self::new(head, item_head_size / 8)
    }

    pub fn new(head: &mut Reader<'b>, item_head_bytes: usize) -> Self {
        let mut body = head.clone();

        let (offset, len) = Self::read_head(head);
        body.skip(offset);

        ArrayReader {
            item_head_bytes,
            next: body,
            remaining: len,
        }
    }

    pub fn read_head(reader: &mut Reader<'b>) -> (usize, usize) {
        let offset = reader.copy_const::<4>();
        let offset = u32::from_le_bytes(offset);

        let len = reader.copy_const::<4>();
        let len = u32::from_le_bytes(len);
        (offset as usize, len as usize)
    }

    pub fn remaining(&self) -> usize {
        self.remaining
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

pub struct TupleReader<'b, 't> {
    tuple: Reader<'b>,
    ty_fields: &'t [pr::TyTupleField],
}

impl<'b, 't> TupleReader<'b, 't> {
    pub fn new(tuple: Reader<'b>, ty: &'t pr::Ty) -> Self {
        let pr::TyKind::Tuple(ty_fields) = &ty.kind else {
            panic!()
        };
        TupleReader { tuple, ty_fields }
    }

    pub fn get_field(&self, index: usize) -> Reader<'b> {
        let mut bytes_offset = 0;
        for i in 0..index {
            let layout = self.ty_fields[i].ty.layout.as_ref().unwrap();
            bytes_offset += layout.head_size / 8;
        }
        let mut r = self.tuple.clone();
        r.skip(bytes_offset);
        r
    }
}
