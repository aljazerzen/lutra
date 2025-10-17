use crate::printer::{PrintSource, Printer};

/// A construct that can print nodes separated by some delimiter.
/// If possible, nodes will be printed in a single line, separated by `sep_inline`.
/// Otherwise, they will be printed one-per-line, each line ending with `sep_line_end`.
pub(super) struct Separated<'a, N: PrintSource> {
    pub nodes: &'a [N],
    pub sep_inline: &'static str,
    pub sep_line_end: &'static str,
}

impl<'a, N: PrintSource> PrintSource for Separated<'a, N> {
    #[tracing::instrument(name = "sep", skip_all)]
    fn print<'c>(&self, mut p: Printer<'c>) -> Option<Printer<'c>> {
        tracing::trace!("try inline {p:?}");

        // try inline
        if let Some(inline) = self.print_inline(p.sub()) {
            tracing::trace!("inline");
            p.merge(inline);
            return Some(p);
        }

        if p.single_line {
            // when required to print to a single line only, fail early
            return None;
        }

        tracing::trace!("try one per line");

        // one per line
        let suffix_width = self.sep_line_end.chars().count() as u16;
        for (i, node) in self.nodes.iter().enumerate() {
            if i > 0 {
                p.inject_trivia_inline(node.span().map(|s| s.start));

                p.new_line();

                p.inject_trivia_leading(node.span().map(|s| s.start), true);
            }

            p.pending_suffix += suffix_width;
            p.merge(node.print(p.sub())?);

            p.push_unchecked(self.sep_line_end);
        }
        tracing::trace!("one per line");
        Some(p)
    }

    fn span(&self) -> Option<crate::Span> {
        let (mut first, last) = Option::zip(
            self.nodes.first().and_then(|n| n.span()),
            self.nodes.last().and_then(|n| n.span()),
        )
        .filter(|(a, b)| a.source_id == b.source_id)?;
        first.with_end(&last);
        Some(first)
    }
}

impl<'a, N: PrintSource> Separated<'a, N> {
    fn print_inline<'c>(&self, mut p: Printer<'c>) -> Option<Printer<'c>> {
        p.require_single_line(self.span())?;

        // optimization: consume separator width in advance
        let sep_width = self.sep_inline.chars().count();
        let sep_count = self.nodes.len().checked_sub(1).unwrap_or_default();
        p.consume(sep_width * sep_count)?;

        for (i, expr) in self.nodes.iter().enumerate() {
            if i > 0 {
                p.push_unchecked(self.sep_inline);
            }
            p.merge(expr.print(p.sub())?);
        }

        Some(p)
    }
}

/// A construct that can print nodes wrapped into a prefix and a suffix.
/// If possible, all three parts will be printed in a single line.
/// Otherwise, node will be placed on a separate line.
pub(super) struct Between<'a, N: PrintSource> {
    pub prefix: &'static str,
    pub node: &'a N,
    pub suffix: &'static str,
    pub span: Option<crate::Span>,
}

impl<'a, N: PrintSource> PrintSource for Between<'a, N> {
    #[tracing::instrument(name = "btwn", skip_all)]
    fn print<'c>(&self, mut p: Printer<'c>) -> Option<Printer<'c>> {
        tracing::trace!("try inline {p:?}");

        // try inline
        if let Some(inline) = self.print_inline(p.sub()) {
            tracing::trace!("inline");
            p.merge(inline);
            return Some(p);
        }

        if p.single_line {
            // when required to print to a single line only, fail early
            return None;
        }

        tracing::trace!("try separate line");

        // separate line
        p.push(self.prefix)?;

        p.indent();
        p.new_line();
        p.inject_trivia_leading(self.node.span().map(|s| s.start), false);

        p.pending_suffix += self.suffix.chars().count() as u16;
        p.merge(self.node.print(p.sub())?);

        p.inject_trivia_inline(self.span().map(|s| s.end()));

        p.inject_trivia_trailing(self.span().map(|s| s.end()));

        p.dedent();
        p.new_line();

        p.push(self.suffix)?;
        tracing::trace!("separate line");
        Some(p)
    }

    fn span(&self) -> Option<crate::Span> {
        self.span
    }
}

impl<'a, N: PrintSource> Between<'a, N> {
    fn print_inline<'c>(&self, mut p: Printer<'c>) -> Option<Printer<'c>> {
        p.require_single_line(self.span())?;

        p.push(self.prefix)?;
        p.merge(self.node.print(p.sub())?);
        p.push(self.suffix)?;
        Some(p)
    }
}
