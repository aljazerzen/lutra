use std::fmt::{self, Debug, Formatter};

/// A span of source code
#[derive(Clone, PartialEq, Eq, Copy)]
pub struct Span {
    /// Byte offset from the start of the source. 0 indexed.
    pub start: u32,
    /// Length of the span in bytes.
    pub len: u16,

    /// A key representing the path of the source file.
    /// Full path is stored in [crate::SourceTree::source_ids].
    pub source_id: u16,
}

impl Span {
    pub fn set_end_of(&mut self, other: &Span) {
        assert_eq!(self.source_id, other.source_id);
        self.set_end(other.end());
    }

    pub fn set_end(&mut self, end: u32) {
        self.len = end.saturating_sub(self.start) as u16;
    }

    pub fn end(&self) -> u32 {
        self.start + self.len as u32
    }
}

impl From<Span> for std::ops::Range<usize> {
    fn from(a: Span) -> Self {
        a.start as usize..(a.start as usize + a.len as usize)
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}-{}", self.source_id, self.start, self.end())
    }
}

impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        // We could expand this to compare source_id too, starting with minimum surprise
        match other.source_id.partial_cmp(&self.source_id) {
            Some(std::cmp::Ordering::Equal) => self.start.partial_cmp(&other.start),
            _ => None,
        }
    }
}

impl chumsky::Span for Span {
    type Context = u16;

    type Offset = u32;

    fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Self {
            start: range.start,
            len: (range.end - range.start) as u16,
            source_id: context,
        }
    }

    fn context(&self) -> Self::Context {
        self.source_id
    }

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.start + self.len as u32
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_span_partial_cmp() {
        let span1 = Span {
            start: 10,
            len: 20,
            source_id: 1,
        };
        let span2 = Span {
            start: 15,
            len: 25,
            source_id: 1,
        };
        let span3 = Span {
            start: 5,
            len: 15,
            source_id: 2,
        };

        // span1 and span2 have the same source_id, so their start values are compared
        assert_eq!(span1.partial_cmp(&span2), Some(std::cmp::Ordering::Less));
        assert_eq!(span2.partial_cmp(&span1), Some(std::cmp::Ordering::Greater));

        // span1 and span3 have different source_id, so their source_id values are compared
        assert_eq!(span1.partial_cmp(&span3), None);
        assert_eq!(span3.partial_cmp(&span1), None);
    }
}
