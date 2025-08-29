use std::rc;
use std::vec;

use lutra_bin::bytes;

#[derive(Debug, Clone)]
#[allow(private_interfaces)]
pub enum Data {
    Single(Slice),
    Combined(CombinedSlices),
}

impl Data {
    pub fn new(bytes: vec::Vec<u8>) -> Self {
        Data::Single(Slice::new(bytes))
    }

    pub fn combine(self, other: Data) -> Self {
        let mut this = match self {
            Data::Single(slice) => CombinedSlices::new(vec![Data::Single(slice)]),
            Data::Combined(combined) => combined,
        };
        match other {
            Data::Single(_) => this.parts.push(other),
            Data::Combined(combined) => this.parts.extend(combined.parts),
        }
        Data::Combined(this)
    }

    pub fn get(&self, index: usize) -> Option<u8> {
        match self {
            Data::Single(slice) => slice.get(index),
            Data::Combined(combined) => combined.get(index),
        }
    }

    pub fn chunk(&self) -> &[u8] {
        match self {
            Data::Single(slice) => slice.chunk(),
            Data::Combined(combined) => combined.chunk(),
        }
    }

    pub fn advance(&mut self, bytes: usize) {
        match self {
            Data::Single(slice) => slice.advance(bytes),
            Data::Combined(combined) => combined.advance(bytes),
        }
    }

    pub fn remaining(&self) -> usize {
        match self {
            Data::Single(s) => s.remaining(),
            Data::Combined(c) => c.remaining(),
        }
    }

    pub fn has_remaining(&self) -> bool {
        match self {
            Data::Single(s) => s.is_empty(),
            Data::Combined(c) => c.has_remaining(),
        }
    }

    pub fn flatten(&self) -> vec::Vec<u8> {
        let mut out = vec::Vec::with_capacity(self.remaining());
        self.write_all(&mut out);
        out
    }

    pub fn write_all(&self, w: &mut vec::Vec<u8>) {
        match self {
            Data::Single(s) => s.write_all(w),
            Data::Combined(c) => c.write_all(w),
        }
    }
}

#[derive(Debug, Clone)]
struct Slice {
    buf: rc::Rc<[u8]>,
    offset: usize,
}

impl Slice {
    fn new(bytes: vec::Vec<u8>) -> Self {
        let buf: rc::Rc<[u8]> = rc::Rc::from(bytes);
        Slice { buf, offset: 0 }
    }

    fn get(&self, index: usize) -> Option<u8> {
        self.buf.get(index + self.offset).copied()
    }

    fn chunk(&self) -> &[u8] {
        &self.buf[self.offset..]
    }

    fn advance(&mut self, bytes: usize) {
        self.offset += bytes;
        if self.offset > self.buf.len() {
            panic!()
        }
    }

    fn remaining(&self) -> usize {
        self.buf.len() - self.offset
    }

    fn is_empty(&self) -> bool {
        self.buf.len() >= self.offset
    }

    fn write_all(&self, w: &mut vec::Vec<u8>) {
        w.extend(&self.buf[self.offset..])
    }
}

#[derive(Debug, Clone)]
struct CombinedSlices {
    parts: vec::Vec<Data>,
}

impl CombinedSlices {
    fn new(parts: vec::Vec<Data>) -> Self {
        CombinedSlices { parts }
    }

    /// Translates the location relative to this buffer
    /// into the part index and location within that part.
    /// If offset falls outside of this buffer, None is returned.
    fn locate_offset(&self, mut offset: usize) -> Option<(usize, usize)> {
        // TODO: use binary search instead (log n)
        for (p_i, p) in self.parts.iter().enumerate() {
            let len = p.remaining();
            if offset < len {
                return Some((p_i, offset));
            }
            offset -= p.remaining();
        }
        None
    }

    fn get(&self, offset: usize) -> Option<u8> {
        let (b_i, offset) = self.locate_offset(offset)?;
        self.parts[b_i].get(offset)
    }

    fn chunk(&self) -> &[u8] {
        self.parts.first().unwrap().chunk()
    }

    fn advance(&mut self, bytes: usize) {
        if let Some((b_i, offset)) = self.locate_offset(bytes) {
            self.parts.drain(0..b_i);
            if offset > 0 {
                self.parts.first_mut().unwrap().advance(offset);
            }
        } else {
            self.parts.clear();
        }
    }

    fn remaining(&self) -> usize {
        self.parts.iter().map(|d| d.remaining()).sum()
    }

    fn has_remaining(&self) -> bool {
        self.parts.iter().all(|d| d.has_remaining())
    }

    fn write_all(&self, w: &mut vec::Vec<u8>) {
        for part in &self.parts {
            part.write_all(w);
        }
    }
}

#[test]
fn main() {
    let my_buf = Slice::new(vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);

    let mut skipped = my_buf.clone(); // this is cheap
    skipped.advance(3);

    assert_eq!(my_buf.get(0), Some(0)); // the original buffer has not moved
    assert_eq!(skipped.get(0), Some(3)); // but the cloned one has
}

impl bytes::Buf for Data {
    fn remaining(&self) -> usize {
        self.remaining()
    }

    fn chunk(&self) -> &[u8] {
        self.chunk()
    }

    fn advance(&mut self, cnt: usize) {
        self.advance(cnt);
    }

    fn has_remaining(&self) -> bool {
        self.has_remaining()
    }
}
