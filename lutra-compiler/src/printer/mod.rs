#![allow(unused_variables)]
//! Printer that converts AST to Lutra source code.
//!
//! Goal is to provide full code formatting, with indentation, wrapping to
//! prevent long lines and preservation of whitespace and comments.
//!
//! The printer uses a recursive descent approach to traverse the AST and
//! generate the corresponding source code. It defines a [PrintSource] trait
//! that is implemented for most AST node types.
//! The [PrintSource::print] method takes a [Printer] object which contains
//! printing configuration (ident size, max line width), buffer for generated
//! source code and constraints for further code printing.
//!
//! For example, when `max_width` is 80 and we insert `let a = ` (8 chars) into
//! the buffer, constrains are updated to reflect that current line has only
//! 72 remaining unused chars.
//!
//! When printing nodes, we first try to print them inline (without new lines).
//! If that succeeds, it will return `Some(Printer)` that contains generated
//! code and new constrains. This printer is then merged into parent printer.
//! If that fails, it will return `None` and we will retry by splitting expr
//! into multiple lines.

mod common;
mod defs;
mod expr;
mod test;
mod types;

use crate::codespan;
use crate::parser::lexer::{Token, TokenKind};
use crate::pr;

pub fn print_ty(ty: &pr::Ty) -> String {
    let mut p = Printer::new(&CONFIG_NO_WRAP, None);
    ty.print(&mut p).unwrap();

    assert!(p.edits.is_empty());
    p.buffer
}

pub fn print_source(source: &pr::Source, trivia: Option<&[Token]>) -> Vec<codespan::TextEdit> {
    let mut p = Printer::new(&CONFIG_PRETTY, trivia);
    p.buffer_span.source_id = source.span.source_id;

    source.print(&mut p).unwrap();

    p.finish_edit();
    p.edits
}

/// Print source code of an AST node, within constrains of a printer
/// (e.g. remaining line width). If this is not possible, return `None`.
trait PrintSource {
    #[must_use]
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()>;

    /// Return span of the AST node.
    /// Used for emitting leading & following trivia (comments, new lines).
    fn span(&self) -> Option<crate::Span>;
}

struct Printer<'c> {
    /// Printer configuration (indent size, max line width)
    config: &'c Config,

    /// Buffer for generated code.
    buffer: String,

    /// Span of the original code that the buffer contains code for.
    /// Note that this is not consistent at every step - only when *particular*
    /// nodes are finished, we increase this value.
    buffer_span: crate::Span,

    /// Remaining width of the current line.
    rem_width: u16,

    /// Number of indentation steps the next line should be prefixed with.
    indent: u16,

    /// When true, generated code must not contain new lines.
    single_line: bool,

    /// Width of suffix that is needed to complete current node.
    /// When using single_line mode, this width can be subtracted before
    /// printing sub-nodes.
    pending_suffix: u16,

    /// Trivia tokens from original source (e.g. comments, new lines)
    trivia: Option<&'c [Token]>,

    /// Finished edits of preceding nodes, whose code might not be contingent to
    /// the code of the nodes in the current [Self::buffer].
    edits: Vec<codespan::TextEdit>,
}

struct Config {
    indent_size: u16,
    max_width: u16,
}

const CONFIG_PRETTY: Config = Config {
    indent_size: 2,
    max_width: 80,
};

const CONFIG_NO_WRAP: Config = Config {
    indent_size: 2,
    max_width: u16::MAX,
};

impl<'c> Printer<'c> {
    fn new(config: &'c Config, trivia: Option<&'c [Token]>) -> Printer<'c> {
        Printer {
            config,

            buffer: String::new(),
            buffer_span: crate::Span {
                source_id: 0, // this is incorrect, but it is overridden later
                start: 0,
                len: 0,
            },
            indent: 0,
            rem_width: config.max_width,
            single_line: false,
            pending_suffix: 0,

            trivia,
            edits: Vec::new(),
        }
    }

    /// Appends a code snippet to generated code buffer.
    ///
    /// It will calculate `rem_width` after the snippet is applied and return
    /// `None` if the snippet overflows the line.
    #[must_use]
    fn push<S: AsRef<str>>(&mut self, snippet: S) -> Option<()> {
        for c in snippet.as_ref().chars() {
            if c == '\n' {
                self.rem_width = self.config.max_width;
                continue;
            }
            if self.rem_width == 0 {
                return None;
            }
            self.rem_width -= 1;
        }
        self.buffer += snippet.as_ref();
        Some(())
    }

    /// Appends a code snippet to generated code buffer, but does not consume
    /// width for it. This is to be used only when width was already consumed in
    /// advance.
    fn push_unchecked<S: AsRef<str>>(&mut self, snippet: S) {
        self.buffer += snippet.as_ref();
    }

    /// Mark a span as fully printed (its printed code is in [Self::buffer]).
    /// This is used for producing [codespan::TextEdit].
    fn mark_printed(&mut self, span: &Option<crate::Span>) {
        if let Some(span) = span {
            self.buffer_span.set_end_of(span);
        }
    }

    /// Mark the buffer to contain code for nodes up to some offset of the
    /// original code.
    fn mark_printed_up_to(&mut self, offset: u32) {
        self.buffer_span.set_end(offset);
    }

    /// Creates a new [Printer] for trying printing for a sub-node without
    /// affecting the original [Printer].
    /// When successful, the forked [Printer] has to be [Printer::merge]ed back.
    fn fork(&self) -> Printer<'c> {
        Printer {
            config: self.config,
            buffer: String::new(),
            buffer_span: crate::Span {
                source_id: self.buffer_span.source_id,
                start: self.buffer_span.end(),
                len: 0,
            },
            rem_width: self.rem_width,
            indent: self.indent,
            single_line: self.single_line,
            pending_suffix: self.pending_suffix,

            trivia: self.trivia,
            edits: Vec::new(),
        }
    }

    /// Merges a forked printer back into the parent.
    fn merge(&mut self, forked: Printer<'c>) {
        self.rem_width = forked.rem_width;
        self.trivia = forked.trivia;
        self.buffer += &forked.buffer;
        self.buffer_span.set_end_of(&forked.buffer_span);
        self.edits.extend(forked.edits);
    }

    /// Subtracts remaining widths. Returns `None` when there is not enough
    /// space on the line.
    #[must_use]
    fn consume(&mut self, width: usize) -> Option<()> {
        self.rem_width = self.rem_width.checked_sub(width as u16)?;
        Some(())
    }

    /// Increase indentation level.
    fn indent(&mut self) {
        self.indent += 1;
    }

    /// Decrease indentation level.
    fn dedent(&mut self) {
        self.indent -= 1;
    }

    /// Resets rem_width and produces chars for a new line + indentation of the
    /// next line.
    fn new_line(&mut self) {
        let indent_width = self.indent * self.config.indent_size;
        self.rem_width = self.config.max_width.saturating_sub(indent_width);

        // When we insert a new line, we can assume that pending suffix will be
        // placed on another line and thus does need to be considered when
        // producing single-line outputs.
        self.pending_suffix = 0;

        let len_without_trailing_whitespace = self.buffer.trim_end_matches(' ').len();
        self.buffer.truncate(len_without_trailing_whitespace);

        self.buffer.push('\n');
        self.buffer.push_str(&" ".repeat(indent_width as usize));
    }

    /// Enables single line mode. Also consumes width of pending node suffix.
    #[must_use]
    fn require_single_line(&mut self, span: Option<crate::Span>) -> Option<()> {
        self.single_line = true;
        self.consume(self.pending_suffix as usize)?;
        self.pending_suffix = 0;

        self.validate_no_comments(span.map(|s| s.end()))?;
        Some(())
    }

    fn take_trivia(&mut self, until: u32) -> Option<&'c Token> {
        let trivia = self.trivia.as_mut()?;

        let t = trivia.first().filter(|t| t.span.end() <= until)?;

        *trivia = &trivia[1..];
        Some(t)
    }

    fn take_trivia_comment(&mut self, until: u32) -> Option<&'c str> {
        let trivia = self.trivia.as_mut()?;

        let t = trivia.first().filter(|t| t.span.end() <= until)?;

        let TokenKind::Comment(s) = &t.kind else {
            return None;
        };
        *trivia = &trivia[1..];
        Some(s)
    }

    fn take_trivia_new_line(&mut self, until: u32) -> Option<()> {
        let trivia = self.trivia.as_mut()?;

        let t = trivia.first().filter(|t| t.span.end() <= until)?;

        let TokenKind::NewLine = &t.kind else {
            return None;
        };
        *trivia = &trivia[1..];
        Some(())
    }

    fn inject_trivia_leading(&mut self, until: Option<u32>) {
        let Some(until) = until else { return };

        let mut nl_count = 0;

        while let Some(trivia) = self.take_trivia(until) {
            match &trivia.kind {
                TokenKind::NewLine => {
                    // print double empty lines
                    nl_count += 1;
                    if nl_count == 2 {
                        self.new_line();
                    }
                }
                TokenKind::Comment(comment) => {
                    self.push_unchecked("# ");
                    self.push_unchecked(comment);
                    self.new_line();
                    nl_count = 0;
                }

                _ => (),
            }
        }
    }

    fn inject_trivia_prev_inline(&mut self, until: Option<u32>) {
        let Some(until) = until else { return };

        while let Some(comment) = self.take_trivia_comment(until) {
            self.push_unchecked(" # ");
            self.push_unchecked(comment);
        }
    }

    fn inject_trivia_trailing(&mut self, until: Option<u32>) {
        let Some(until) = until else { return };

        let mut had_empty_line = false;
        while let Some(token) = self.take_trivia(until) {
            let TokenKind::Comment(comment) = &token.kind else {
                continue;
            };

            self.new_line();
            if !had_empty_line {
                self.new_line();
                had_empty_line = true;
            }

            self.push_unchecked("# ");
            self.push_unchecked(comment);
        }
    }

    fn validate_no_comments(&mut self, until: Option<u32>) -> Option<()> {
        let Some(until) = until else {
            return Some(());
        };

        while let Some(trivia) = self.take_trivia(until) {
            if let TokenKind::Comment(comment) = &trivia.kind {
                return None;
            }
        }
        Some(())
    }

    /// Convert current buffer into an [codespan::TextEdit] and save it.
    /// This is used to skip formatting of some nodes, i.e. not produce
    /// any [codespan::TextEdit] that would change it.
    fn finish_edit(&mut self) {
        let code = std::mem::take(&mut self.buffer);
        self.edits.push(codespan::TextEdit {
            span: self.buffer_span,
            new_text: code,
        });
        self.buffer_span.start = self.buffer_span.end();
        self.buffer_span.len = 0;
    }
}

impl std::fmt::Debug for Printer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Printer")
            .field("rem_width", &self.rem_width)
            .field("single_line", &self.single_line)
            .field("pending_suffix", &self.pending_suffix)
            .finish()
    }
}
