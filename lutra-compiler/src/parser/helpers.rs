use chumsky::input::ValueInput;
use chumsky::prelude::*;

use super::{PError, PExtra, TokenKind};

use crate::Span;
use crate::diagnostic::{Diagnostic, DiagnosticCode};

pub(crate) fn ident_part<'src, I>() -> impl Parser<'src, I, String, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    select! {
        TokenKind::Ident(ident) => ident
    }
    .labelled("identifier")
}

pub(crate) fn ident_keyword<'src, I>(
    kw: &'static str,
) -> impl Parser<'src, I, (), PExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    select! {
        TokenKind::Ident(ident) if ident == kw => ()
    }
}

pub(crate) fn keyword<'src, I>(kw: &'static str) -> impl Parser<'src, I, (), PExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    just(TokenKind::Keyword(kw)).ignored()
}

pub(crate) fn ctrl<'src, I>(char: char) -> impl Parser<'src, I, (), PExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
{
    just(TokenKind::Control(char)).ignored()
}

/// Parse a sequence allowing commas between items.
/// Doesn't include the surrounding delimiters.
pub(crate) fn sequence<'src, I, O>(
    parser: impl Parser<'src, I, O, PExtra<'src>> + Clone + 'src,
) -> impl Parser<'src, I, Vec<O>, PExtra<'src>> + Clone + 'src
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
    O: 'src,
{
    parser.separated_by(ctrl(',')).allow_trailing().collect()
}

pub(crate) fn delimited_by_parenthesis<'src, I, O>(
    parser: impl Parser<'src, I, O, PExtra<'src>> + Clone + 'src,
    fallback: impl Fn(Span) -> O + Clone + 'src,
) -> Boxed<'src, 'src, I, O, PExtra<'src>>
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
    O: 'src,
{
    parser
        .delimited_by(ctrl('('), ctrl(')'))
        .recover_with(via_parser(nested_delimiters(
            TokenKind::Control('('),
            TokenKind::Control(')'),
            [
                (TokenKind::Control('['), TokenKind::Control(']')),
                (TokenKind::Control('('), TokenKind::Control(')')),
            ],
            fallback,
        )))
        .boxed()
}

pub(crate) fn delimited_by_braces<'src, I, O>(
    content: impl Parser<'src, I, O, PExtra<'src>> + Clone + 'src,
    fallback: impl Fn(Span) -> O + Clone + 'src,
) -> Boxed<'src, 'src, I, O, PExtra<'src>>
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
    O: 'src,
{
    content
        .delimited_by(ctrl('{'), ctrl('}'))
        .recover_with(via_parser(nested_delimiters(
            TokenKind::Control('{'),
            TokenKind::Control('}'),
            [
                (TokenKind::Control('('), TokenKind::Control(')')),
                (TokenKind::Control('['), TokenKind::Control(']')),
            ],
            fallback,
        )))
        .boxed()
}

pub(crate) fn delimited_by_brackets<'src, I, O>(
    content: impl Parser<'src, I, O, PExtra<'src>> + Clone + 'src,
    fallback: impl Fn(Span) -> O + Clone + 'src,
) -> Boxed<'src, 'src, I, O, PExtra<'src>>
where
    I: ValueInput<'src, Token = TokenKind, Span = Span>,
    O: 'src,
{
    content
        .delimited_by(ctrl('['), ctrl(']'))
        .recover_with(via_parser(nested_delimiters(
            TokenKind::Control('['),
            TokenKind::Control(']'),
            [
                (TokenKind::Control('{'), TokenKind::Control('}')),
                (TokenKind::Control('('), TokenKind::Control(')')),
            ],
            fallback,
        )))
        .boxed()
}

impl<'src> From<PError<'src>> for Diagnostic {
    fn from(p: PError<'src>) -> Diagnostic {
        use chumsky::error::RichReason;

        let span = *p.span();

        let diagnostic = match p.reason() {
            RichReason::Custom(msg) => Diagnostic::new(msg.clone(), DiagnosticCode::PARSER),

            RichReason::ExpectedFound { .. } => {
                use chumsky::error::RichPattern;

                fn token_to_string(t: Option<&TokenKind>) -> String {
                    t.map(|t| t.to_string())
                        .unwrap_or_else(|| "end of input".to_string())
                }

                let is_expecting_eof = p
                    .expected()
                    .all(|pat| matches!(pat, RichPattern::EndOfInput));

                let expected: Vec<String> = if is_expecting_eof {
                    vec![token_to_string(None)]
                } else {
                    p.expected()
                        .filter(|pat| !matches!(pat, RichPattern::EndOfInput))
                        .map(|pat| match pat {
                            RichPattern::Token(t) => token_to_string(Some(&**t)),
                            RichPattern::Label(l) => l.to_string(),
                            RichPattern::EndOfInput => token_to_string(None),
                            _ => pat.to_string(),
                        })
                        .collect()
                };

                if expected.is_empty() || expected.len() > 10 {
                    let label = token_to_string(p.found());
                    Diagnostic::new(format!("unexpected {label}"), DiagnosticCode::PARSER)
                } else {
                    let mut expected = expected;
                    expected.sort();
                    expected.dedup();

                    let expected_str = match expected.len() {
                        1 => expected.remove(0),
                        2 => expected.join(" or "),
                        _ => {
                            let last = expected.pop().unwrap();
                            format!("one of {} or {last}", expected.join(", "))
                        }
                    };

                    let message = match p.found() {
                        Some(found) => format!("expected {expected_str}, but found {found}"),
                        None => {
                            format!("expected {expected_str}, but encountered the end of the file.")
                        }
                    };
                    Diagnostic::new(message, DiagnosticCode::PARSER)
                }
            }
        };

        diagnostic.with_span(Some(span))
    }
}
