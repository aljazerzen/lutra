#[cfg(test)]
mod test;
mod token;

pub use token::{SpanInSource, Token, TokenKind};

use chumsky::error::Cheap;
use chumsky::prelude::*;
use chumsky::text::newline;

use crate::diagnostic::{Diagnostic, DiagnosticCode};

use crate::pr::Literal;
use crate::span::Span;

type LError = Cheap<char, SpanInSource>;

/// Split source into tokens.
pub fn lex_source_recovery(source: &str, source_id: u16) -> (Option<Vec<Token>>, Vec<Diagnostic>) {
    let stream = prepare_stream(source);

    let (tokens, lex_errors) = lexer().parse_recovery(stream);

    let errors = lex_errors
        .into_iter()
        .map(|e| convert_lexer_error(source, e, source_id))
        .collect();

    tracing::debug!("lex errors: {:?}", errors);
    (tokens, errors)
}

#[cfg(test)]
pub fn lex_source(source: &str) -> Result<Vec<Token>, Vec<Diagnostic>> {
    let stream = prepare_stream(source);
    lexer().parse(stream).map_err(|e| {
        e.into_iter()
            .map(|x| convert_lexer_error(source, x, 0))
            .collect()
    })
}

pub fn prepare_stream(source: &str) -> chumsky::Stream<'_, char, SpanInSource, CharIterator<'_>> {
    chumsky::Stream::from_iter(
        SpanInSource {
            start: source.len() as u32,
            len: 0,
        },
        CharIterator::new(source),
    )
}

pub(crate) struct CharIterator<'a> {
    last_start: u32,
    chars: std::str::Chars<'a>,
}

impl<'a> CharIterator<'a> {
    pub fn new(string: &'a str) -> Self {
        Self {
            last_start: 0,
            chars: string.chars(),
        }
    }
}

impl<'a> Iterator for CharIterator<'a> {
    type Item = (char, SpanInSource);

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.chars.next()?;
        let span = SpanInSource {
            start: self.last_start,
            len: c.len_utf8() as u16,
        };
        self.last_start += span.len as u32;
        Some((c, span))
    }
}

fn convert_lexer_error(source: &str, e: LError, source_id: u16) -> Diagnostic {
    let start = e.span().start as usize;
    let len = e.span().len as usize;

    let found: &str = &source[start..start + len];
    let span = Some(Span {
        start: e.span().start,
        len: e.span().len,
        source_id,
    });

    Diagnostic::new(format!("unexpected {found}"), DiagnosticCode::PARSER).with_span(span)
}

/// Lex chars to tokens until the end of the input
fn lexer() -> impl Parser<char, Vec<Token>, Error = LError> {
    let control_multi = choice((
        just("->").to(TokenKind::ArrowThin),
        just("=>").to(TokenKind::ArrowFat),
        just("==").to(TokenKind::Eq),
        just("!=").to(TokenKind::Ne),
        just(">=").to(TokenKind::Gte),
        just("<=").to(TokenKind::Lte),
        just("~=").to(TokenKind::RegexSearch),
        just("&&").to(TokenKind::And),
        just("||").to(TokenKind::Or),
        just("??").to(TokenKind::Coalesce),
        just("//").to(TokenKind::DivInt),
        just("**").to(TokenKind::Pow),
        just("::").to(TokenKind::PathSep),
        just("..").to(TokenKind::Range),
    ));

    let control = one_of("></%=+-*[]().,:|!{};@").map(TokenKind::Control);

    let literal = literal().map(TokenKind::Literal);

    let keyword = choice((
        just("as"),
        just("const"),
        just("do"),
        just("else"),
        just("enum"),
        just("for"),
        just("func"),
        just("if"),
        just("import"),
        just("in"),
        just("internal"),
        just("let"),
        just("match"),
        just("module"),
        just("then"),
        just("type"),
        just("where"),
    ))
    .then_ignore(non_ident())
    .map(TokenKind::Keyword);

    let ident = ident_part().map(TokenKind::Ident);

    let interpolation = one_of("sf")
        .then(quoted_string(true))
        .map(|(c, s)| TokenKind::Interpolation(c, s));

    let token = choice((
        control_multi,
        interpolation,
        literal,
        control,
        keyword,
        ident,
        doc_comment(),
    ))
    .recover_with(skip_then_retry_until([]).skip_start())
    .map_with_span(|kind, span| Token { kind, span });

    token
        .separated_by(ignored())
        .allow_leading()
        .allow_trailing()
        .then_ignore(end())
}

fn doc_comment() -> impl Parser<char, TokenKind, Error = LError> {
    just('#')
        .ignore_then(choice((just('#').to(false), just('!').to(true))))
        .then(newline().not().repeated().collect::<String>())
        .map(|(is_self, text)| {
            if is_self {
                TokenKind::DocCommentSelf(text)
            } else {
                TokenKind::DocComment(text)
            }
        })
}

fn ignored() -> impl Parser<char, (), Error = LError> {
    let comment = just('#')
        .ignore_then(just('#').or(just('!')).not().rewind())
        .ignore_then(newline().not().repeated())
        .ignored();

    let whitespace = filter::<char, _, LError>(|c: &char| c.is_whitespace()).ignored();

    comment.or(whitespace).repeated().ignored()
}

pub(crate) fn ident_part() -> impl Parser<char, String, Error = LError> + Clone {
    let plain = filter(|c: &char| c.is_alphabetic() || *c == '_')
        .chain(filter(|c: &char| c.is_alphanumeric() || *c == '_').repeated());

    let backticks = none_of('`').repeated().delimited_by(just('`'), just('`'));

    plain.or(backticks).collect()
}

fn literal() -> impl Parser<char, Literal, Error = LError> {
    let binary_notation = just("0b")
        .then_ignore(just("_").or_not())
        .ignore_then(
            filter(|c: &char| *c == '0' || *c == '1')
                .repeated()
                .at_least(1)
                .at_most(32)
                .collect::<String>()
                .try_map(|digits, _| {
                    Ok(Literal::Integer(i64::from_str_radix(&digits, 2).unwrap()))
                }),
        )
        .labelled("number");

    let hexadecimal_notation = just("0x")
        .then_ignore(just("_").or_not())
        .ignore_then(
            filter(|c: &char| c.is_ascii_hexdigit())
                .repeated()
                .at_least(1)
                .at_most(12)
                .collect::<String>()
                .try_map(|digits, _| {
                    Ok(Literal::Integer(i64::from_str_radix(&digits, 16).unwrap()))
                }),
        )
        .labelled("number");

    let octal_notation = just("0o")
        .then_ignore(just("_").or_not())
        .ignore_then(
            filter(|&c| ('0'..='7').contains(&c))
                .repeated()
                .at_least(1)
                .at_most(12)
                .collect::<String>()
                .try_map(|digits, _| {
                    Ok(Literal::Integer(i64::from_str_radix(&digits, 8).unwrap()))
                }),
        )
        .labelled("number");

    let exp = one_of("eE").chain(one_of("+-").or_not().chain::<char, _, _>(text::digits(10)));

    let integer = just('-')
        .or_not()
        .then(filter(|c: &char| c.is_ascii_digit() && *c != '0'))
        .chain::<_, Vec<char>, _>(filter(|c: &char| c.is_ascii_digit() || *c == '_').repeated())
        .or(just('0').map(|c| vec![c]));

    let frac = just('.')
        .chain::<char, _, _>(filter(|c: &char| c.is_ascii_digit()))
        .chain::<char, _, _>(filter(|c: &char| c.is_ascii_digit() || *c == '_').repeated());

    let number = integer
        .chain::<char, _, _>(frac.or_not().flatten())
        .chain::<char, _, _>(exp.or_not().flatten())
        .try_map(|chars, span| {
            let str = chars.into_iter().filter(|c| *c != '_').collect::<String>();

            if let Ok(i) = str.parse::<i64>() {
                Ok(Literal::Integer(i))
            } else if let Ok(f) = str.parse::<f64>() {
                Ok(Literal::Float(f))
            } else {
                Err(Cheap::expected_input_found(span, None, None))
            }
        })
        .labelled("number");

    let string = quoted_string(true).map(Literal::Text);

    let raw_string = just("r")
        .ignore_then(quoted_string(false))
        .map(Literal::Text);

    let bool = (just("true").to(true))
        .or(just("false").to(false))
        .then_ignore(non_ident())
        .map(Literal::Boolean);

    let date_inner = digits(4)
        .chain(just('-'))
        .chain::<char, _, _>(digits(2))
        .chain::<char, _, _>(just('-'))
        .chain::<char, _, _>(digits(2))
        .boxed();

    let time_inner = digits(2)
        // minutes
        .chain::<char, _, _>(just(':').chain(digits(2)).or_not().flatten())
        // seconds
        .chain::<char, _, _>(just(':').chain(digits(2)).or_not().flatten())
        // milliseconds
        .chain::<char, _, _>(
            just('.')
                .chain(
                    filter(|c: &char| c.is_ascii_digit())
                        .repeated()
                        .at_least(1)
                        .at_most(6),
                )
                .or_not()
                .flatten(),
        )
        // timezone offset
        .chain::<char, _, _>(
            choice((
                // Either just `Z`
                just('Z').map(|x| vec![x]),
                // Or an offset, such as `-05:00` or `-0500`
                one_of("-+").chain(
                    digits(2)
                        .then_ignore(just(':').or_not())
                        .chain::<char, _, _>(digits(2)),
                ),
            ))
            .or_not(),
        )
        .boxed();

    // Not an annotation
    let dt_prefix = just('@').then(just('{').not().rewind());

    let date = dt_prefix
        .ignore_then(date_inner.clone())
        .then_ignore(non_ident())
        .collect::<String>()
        .map(Literal::Date);

    let time = dt_prefix
        .ignore_then(time_inner.clone())
        .then_ignore(non_ident())
        .collect::<String>()
        .map(Literal::Time);

    let datetime = dt_prefix
        .ignore_then(date_inner)
        .chain(just('T'))
        .chain::<char, _, _>(time_inner)
        .then_ignore(non_ident())
        .collect::<String>()
        .map(Literal::Timestamp);

    choice((
        binary_notation,
        hexadecimal_notation,
        octal_notation,
        string,
        raw_string,
        number,
        bool,
        datetime,
        date,
        time,
    ))
}

fn quoted_string(escaped: bool) -> impl Parser<char, String, Error = LError> {
    choice((quoted_string_of_quote(&'"', escaped),))
        .collect::<String>()
        .labelled("string")
}

fn quoted_string_of_quote(
    quote: &char,
    escaping: bool,
) -> impl Parser<char, Vec<char>, Error = LError> + '_ {
    let opening = just(*quote).repeated().at_least(1);

    opening.then_with(move |opening| {
        if opening.len() % 2 == 0 {
            // If we have an even number of quotes, it's an empty string.
            return (just(vec![])).boxed();
        }
        let delimiter = just(*quote).repeated().exactly(opening.len());

        let inner = if escaping {
            choice((
                // If we're escaping, don't allow consuming a backslash
                // We need the `vec` to satisfy the type checker
                (delimiter.or(just(vec!['\\']))).not(),
                escaped_character(),
                // Or escape the quote char of the current string
                just('\\').ignore_then(just(*quote)),
            ))
            .boxed()
        } else {
            delimiter.not().boxed()
        };

        inner.repeated().then_ignore(delimiter).boxed()
    })
}

fn escaped_character() -> impl Parser<char, char, Error = LError> {
    just('\\').ignore_then(choice((
        just('\\'),
        just('/'),
        just('b').to('\x08'),
        just('f').to('\x0C'),
        just('n').to('\n'),
        just('r').to('\r'),
        just('t').to('\t'),
        (just("u{").ignore_then(
            filter(|c: &char| c.is_ascii_hexdigit())
                .repeated()
                .at_least(1)
                .at_most(6)
                .collect::<String>()
                .validate(|digits, span, emit| {
                    char::from_u32(u32::from_str_radix(&digits, 16).unwrap()).unwrap_or_else(|| {
                        emit(Cheap::expected_input_found(span, None, None));
                        '\u{FFFD}' // Unicode replacement character
                    })
                })
                .then_ignore(just('}')),
        )),
        (just('x').ignore_then(
            filter(|c: &char| c.is_ascii_hexdigit())
                .repeated()
                .exactly(2)
                .collect::<String>()
                .validate(|digits, span, emit| {
                    char::from_u32(u32::from_str_radix(&digits, 16).unwrap()).unwrap_or_else(|| {
                        emit(Cheap::expected_input_found(span, None, None));
                        '\u{FFFD}'
                    })
                }),
        )),
    )))
}

fn digits(count: usize) -> impl Parser<char, Vec<char>, Error = LError> {
    filter(|c: &char| c.is_ascii_digit())
        .repeated()
        .exactly(count)
}

fn non_ident() -> impl Parser<char, (), Error = LError> {
    filter(|c: &char| c.is_alphanumeric() || *c == '_')
        .not()
        .ignored()
        .or(end())
        .rewind()
}
