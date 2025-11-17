mod edit;
mod line_numbers;
mod span;

pub use edit::{TextEdit, apply_text_edits, minimize_text_edits, offset_text_edits};
pub use line_numbers::LineNumbers;
pub use span::Span;

/// A position within a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineColumn {
    /// 0-index of the line
    pub line: u32,
    /// 0-index of the column in UTF16 code units.
    /// (number of UTF16 code units before this position on the current line)
    pub column: u32,
}

/// A range of a source file.
#[derive(Debug, Clone)]
pub struct Range {
    pub start: LineColumn,
    pub end: LineColumn,
}
