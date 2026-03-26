use std::fmt::Debug;

pub type PError = chumsky::error::Cheap<Span>;

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
    pub span: Span,
}

impl Error {
    pub fn from_parser(p: PError, source: &str) -> Error {
        let s = p.span();
        let start = s.start;
        let end = s.end;
        let span = Span { start, end };

        let snippet: String = source
            .chars()
            .skip(span.start)
            .take(span.end - span.start)
            .collect();

        let message = format!("unexpected '{snippet}'");

        Error { message, span }
    }
}

impl From<lutra_compiler::_lexer::Diagnostic> for Error {
    fn from(d: lutra_compiler::_lexer::Diagnostic) -> Self {
        let span = d.span.unwrap();
        Self {
            message: d.message,
            span: Span {
                start: span.start as usize,
                end: span.start as usize + span.len as usize,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl chumsky::span::Span for Span {
    type Context = ();

    type Offset = usize;

    fn new(_: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Span {
            start: range.start,
            end: range.end,
        }
    }

    fn context(&self) -> Self::Context {}

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}
impl From<std::ops::Range<usize>> for Span {
    fn from(value: std::ops::Range<usize>) -> Self {
        Span {
            start: value.start,
            end: value.end,
        }
    }
}
