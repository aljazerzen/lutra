use std::fmt::{self, Debug, Formatter};
use std::ops::{Add, Range, Sub};

use chumsky::Stream;

#[derive(Clone, PartialEq, Eq, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,

    /// A key representing the path of the source. Value is stored in prqlc's SourceTree::source_ids.
    pub source_id: u16,
}

impl Span {
    pub fn merge(a: Span, b: Span) -> Span {
        assert_eq!(a.source_id, b.source_id);
        Span {
            start: usize::min(a.start, b.start),
            end: usize::max(a.end, b.end),

            source_id: a.source_id,
        }
    }

    pub fn merge_opt(a: Option<Span>, b: Option<Span>) -> Option<Span> {
        match (a, b) {
            (Some(a), Some(b)) => Some(Self::merge(a, b)),
            (Some(s), None) | (None, Some(s)) => Some(s),
            (None, None) => None,
        }
    }
}

impl From<Span> for Range<usize> {
    fn from(a: Span) -> Self {
        a.start..a.end
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}-{}", self.source_id, self.start, self.end)
    }
}


impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        // We could expand this to compare source_id too, starting with minimum surprise
        match other.source_id.partial_cmp(&self.source_id) {
            Some(std::cmp::Ordering::Equal) => {
                debug_assert!((self.start <= other.start) == (self.end <= other.end));
                self.start.partial_cmp(&other.start)
            }
            _ => None,
        }
    }
}

impl chumsky::Span for Span {
    type Context = u16;

    type Offset = usize;

    fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Self {
            start: range.start,
            end: range.end,
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
        self.end
    }
}

impl Add<usize> for Span {
    type Output = Span;

    fn add(self, rhs: usize) -> Span {
        Self {
            start: self.start + rhs,
            end: self.end + rhs,
            source_id: self.source_id,
        }
    }
}

impl Sub<usize> for Span {
    type Output = Span;

    fn sub(self, rhs: usize) -> Span {
        Self {
            start: self.start - rhs,
            end: self.end - rhs,
            source_id: self.source_id,
        }
    }
}

pub(crate) fn string_stream<'a>(
    s: String,
    span_base: Span,
) -> Stream<'a, char, Span, Box<dyn Iterator<Item = (char, Span)>>> {
    let chars = s.chars().collect::<Vec<_>>();

    Stream::from_iter(
        Span {
            start: span_base.start + chars.len(),
            end: span_base.start + chars.len(),
            source_id: span_base.source_id,
        },
        Box::new(chars.into_iter().enumerate().map(move |(i, c)| {
            (
                c,
                Span {
                    start: span_base.start + i,
                    end: span_base.start + i + 1,
                    source_id: span_base.source_id,
                },
            )
        })),
    )
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_span_partial_cmp() {
        let span1 = Span {
            start: 10,
            end: 20,
            source_id: 1,
        };
        let span2 = Span {
            start: 15,
            end: 25,
            source_id: 1,
        };
        let span3 = Span {
            start: 5,
            end: 15,
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
