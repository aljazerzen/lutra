#[derive(Debug, Clone)]
#[allow(private_interfaces)]
pub enum Data {
    Single(Slice),
    Combined(CombinedSlices),
}

impl Data {
    pub fn new(bytes: Vec<u8>) -> Self {
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

    pub fn slice(&self, index: usize) -> &[u8] {
        match self {
            Data::Single(slice) => slice.slice(index),
            Data::Combined(combined) => combined.slice(index),
        }
    }

    pub fn skip(&mut self, bytes: usize) {
        match self {
            Data::Single(slice) => slice.skip(bytes),
            Data::Combined(combined) => combined.skip(bytes),
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Data::Single(s) => s.len(),
            Data::Combined(c) => c.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Data::Single(s) => s.is_empty(),
            Data::Combined(c) => c.is_empty(),
        }
    }

    pub fn flatten(&self) -> Vec<u8> {
        let mut out = Vec::with_capacity(self.len());
        self.write_all(&mut out);
        out
    }

    pub fn write_all(&self, w: &mut Vec<u8>) {
        match self {
            Data::Single(s) => s.write_all(w),
            Data::Combined(c) => c.write_all(w),
        }
    }
}

#[derive(Debug, Clone)]
struct Slice {
    buf: std::rc::Rc<[u8]>,
    offset: usize,
}

impl Slice {
    fn new(bytes: Vec<u8>) -> Self {
        let buf: std::rc::Rc<[u8]> = std::rc::Rc::from(bytes);
        Slice { buf, offset: 0 }
    }

    fn get(&self, index: usize) -> Option<u8> {
        self.buf.get(index + self.offset).copied()
    }

    fn slice(&self, index: usize) -> &[u8] {
        &self.buf[self.offset..][..index]
    }

    fn skip(&mut self, bytes: usize) {
        self.offset += bytes;
        if self.offset > self.buf.len() {
            panic!()
        }
    }

    fn len(&self) -> usize {
        self.buf.len() - self.offset
    }

    fn is_empty(&self) -> bool {
        self.buf.len() >= self.offset
    }

    fn write_all(&self, w: &mut Vec<u8>) {
        w.extend(&self.buf[self.offset..])
    }
}

#[derive(Debug, Clone)]
struct CombinedSlices {
    parts: Vec<Data>,
}

impl CombinedSlices {
    fn new(parts: Vec<Data>) -> Self {
        CombinedSlices { parts }
    }

    fn locate_offset(&self, mut offset: usize) -> Option<(usize, usize)> {
        // TODO: use binary search instead (log n)
        for (b_i, b) in self.parts.iter().enumerate() {
            let len = b.len();
            if offset < len {
                return Some((b_i, offset));
            }
            offset -= b.len();
        }
        None
    }

    fn get(&self, offset: usize) -> Option<u8> {
        let (b_i, offset) = self.locate_offset(offset)?;
        self.parts[b_i].get(offset)
    }

    fn slice(&self, index: usize) -> &[u8] {
        self.parts.first().unwrap().slice(index)
    }

    fn skip(&mut self, bytes: usize) {
        let (b_i, offset) = self.locate_offset(bytes).unwrap();
        self.parts.drain(0..b_i);
        if offset > 0 {
            self.parts.last_mut().unwrap().skip(offset);
        }
    }

    fn len(&self) -> usize {
        self.parts.iter().map(|d| d.len()).sum()
    }
    fn is_empty(&self) -> bool {
        self.parts.iter().all(|d| d.is_empty())
    }

    fn write_all(&self, w: &mut Vec<u8>) {
        for part in &self.parts {
            part.write_all(w);
        }
    }
}

#[test]
fn main() {
    let my_buf = Slice::new(vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);

    let mut skipped = my_buf.clone(); // this is cheap
    skipped.skip(3);

    assert_eq!(my_buf.get(0), Some(0)); // the original buffer has not moved
    assert_eq!(skipped.get(0), Some(3)); // but the cloned one has
}
