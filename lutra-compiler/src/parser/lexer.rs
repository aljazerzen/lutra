#[cfg(test)]
mod test;
mod token;

use std::str::FromStr;

pub use token::{SpanInSource, Token, TokenKind};

use chumsky::prelude::*;

use crate::Span;
use crate::diagnostic::{Diagnostic, DiagnosticCode};
use crate::pr::{Date, Literal, Time};

/// Split source into tokens.
pub fn lex_source_recovery(source: &str, source_id: u16) -> (Option<Tokens>, Vec<Diagnostic>) {
    let (tokens, lex_errors) = lexer().parse(source).into_output_errors();

    let errors = lex_errors
        .into_iter()
        .map(|e| convert_lexer_error(source, e, source_id))
        .collect();

    let tokens = tokens.map(|tok_vec| {
        let (semantic, trivia) = tok_vec.into_iter().partition(is_semantic);
        Tokens { semantic, trivia }
    });

    tracing::debug!("lex errors: {:?}", errors);
    (tokens, errors)
}

pub fn lex_source(source: &str) -> Result<impl Iterator<Item = Token>, Vec<Diagnostic>> {
    let (tokens, errors) = lexer().parse(source).into_output_errors();
    if !errors.is_empty() {
        return Err(errors
            .into_iter()
            .map(|e| convert_lexer_error(source, e, 0))
            .collect());
    }
    Ok(tokens.unwrap_or_default().into_iter().filter(is_semantic))
}

#[derive(Debug, Default)]
pub struct Tokens {
    pub semantic: Vec<Token>,
    pub trivia: Vec<Token>,
}

// --- shorthands ---

/// Error type for the char-level lexer.
type LError = Cheap<SimpleSpan>;

/// Shorthand for a chumsky parser over our input.
pub(crate) trait LtLexer<'src, T>:
    Parser<'src, &'src str, T, extra::Err<LError>> + Clone + 'src
{
}

impl<'src, T, P> LtLexer<'src, T> for P where
    P: Parser<'src, &'src str, T, extra::Err<LError>> + Clone + 'src
{
}

/// Lex chars to tokens until the end of the input.
fn lexer<'src>() -> impl LtLexer<'src, Vec<Token>> {
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

    let control = one_of("></%=+-*[]().,:|!{};@?").map(TokenKind::Control);

    let literal = literal().map(TokenKind::Literal);

    let keyword = choice((
        just("as").to(TokenKind::Keyword("as")),
        just("const").to(TokenKind::Keyword("const")),
        just("do").to(TokenKind::Keyword("do")),
        just("else").to(TokenKind::Keyword("else")),
        just("enum").to(TokenKind::Keyword("enum")),
        just("for").to(TokenKind::Keyword("for")),
        just("func").to(TokenKind::Keyword("func")),
        just("if").to(TokenKind::Keyword("if")),
        just("import").to(TokenKind::Keyword("import")),
        just("in").to(TokenKind::Keyword("in")),
        just("internal").to(TokenKind::Keyword("internal")),
        just("let").to(TokenKind::Keyword("let")),
        just("match").to(TokenKind::Keyword("match")),
        just("module").to(TokenKind::Keyword("module")),
        just("then").to(TokenKind::Keyword("then")),
        just("type").to(TokenKind::Keyword("type")),
        just("submodule").to(TokenKind::Keyword("submodule")),
        just("where").to(TokenKind::Keyword("where")),
    ))
    .then_ignore(non_ident());

    let ident = ident_part().map(TokenKind::Ident);

    let interpolation = one_of("sf")
        .then(quoted_string(true))
        .map(|(c, s)| TokenKind::Interpolation(c, s));

    let new_lines = just('\r').or_not().then(just('\n')).to(TokenKind::NewLine);

    let token = choice((
        control_multi,
        interpolation,
        literal,
        control,
        keyword,
        ident,
        comment(),
        new_lines,
    ))
    .recover_with(skip_then_retry_until(any().ignored(), end()))
    .map_with(|kind, e| Token {
        kind,
        span: span_to_span_in_source(e.span()),
    });

    just('\u{feff}')
        .or_not()
        .ignore_then(
            token
                .separated_by(
                    any()
                        .filter(|c: &char| *c == ' ' || *c == '\t')
                        .ignored()
                        .repeated(),
                )
                .allow_leading()
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(end())
}

fn comment<'src>() -> impl LtLexer<'src, TokenKind> {
    just('#')
        .ignore_then(choice((just('#').to(false), just('!').to(true))).or_not())
        .then_ignore(just(' ').or_not())
        .then(
            any()
                .filter(|c: &char| *c != '\n' && *c != '\r')
                .repeated()
                .collect::<String>(),
        )
        .map(|(kind, text)| match kind {
            None => TokenKind::Comment(text),
            Some(false) => TokenKind::DocComment(text),
            Some(true) => TokenKind::DocCommentSelf(text),
        })
}

fn ident_part<'src>() -> impl LtLexer<'src, String> {
    let plain = any()
        .filter(|c: &char| c.is_alphabetic() || *c == '_')
        .then(
            any()
                .filter(|c: &char| c.is_alphanumeric() || *c == '_')
                .repeated()
                .collect::<String>(),
        )
        .map(|(first, rest): (char, String)| {
            let mut s = first.to_string();
            s.push_str(&rest);
            s
        });

    let backticks = none_of('`')
        .repeated()
        .collect::<String>()
        .delimited_by(just('`'), just('`'));

    plain.or(backticks)
}

fn literal<'src>() -> impl LtLexer<'src, Literal> {
    let string = quoted_string(true).map(Literal::Text);
    let raw_string = just("r")
        .ignore_then(quoted_string(false))
        .map(Literal::Text);

    // All numeric alternatives produce () (via .ignored()) so they can be
    // combined with `choice`, then `to_slice` captures the matched text.

    let hex = just("0x")
        .then(
            any()
                .filter(|c: &char| c.is_ascii_hexdigit() || *c == '_')
                .repeated()
                .at_least(1)
                .at_most(16),
        )
        .ignored();

    let oct = just("0o")
        .then(
            any()
                .filter(|c: &char| ('0'..='7').contains(c) || *c == '_')
                .repeated()
                .at_least(1)
                .at_most(22),
        )
        .ignored();

    let bin = just("0b")
        .then(
            any()
                .filter(|c: &char| *c == '0' || *c == '1' || *c == '_')
                .repeated()
                .at_least(1)
                .at_most(64),
        )
        .ignored();

    let dec_int = any()
        .filter(|c: &char| c.is_ascii_digit() && *c != '0')
        .then(
            any()
                .filter(|c: &char| c.is_ascii_digit() || *c == '_')
                .repeated(),
        )
        .ignored()
        .or(just('0').ignored());

    let frac = just('.')
        .then(any().filter(|c: &char| c.is_ascii_digit()))
        .then(
            any()
                .filter(|c: &char| c.is_ascii_digit() || *c == '_')
                .repeated(),
        )
        .ignored();

    let exp = one_of("eE")
        .then(one_of("+-").or_not())
        .then(
            any()
                .filter(|c: &char| c.is_ascii_digit())
                .repeated()
                .at_least(1),
        )
        .ignored();

    // Decimal: optional sign, integer part, optional fraction, optional exponent.
    let dec = just('-')
        .or_not()
        .then(dec_int)
        .then(frac.or_not())
        .then(exp.or_not())
        .ignored();

    let number = choice((hex, oct, bin, dec))
        .to_slice()
        .map(|s: &str| Literal::Number(s.to_string()))
        .labelled("number");

    let boolean = (just("true").to(true))
        .or(just("false").to(false))
        .then_ignore(non_ident())
        .map(Literal::Boolean);

    let sign_opt = just('+').or(just('-')).or_not();

    // Date / DateTime
    let year = sign_opt
        .then(
            any()
                .filter(|c: &char| c.is_ascii_digit())
                .repeated()
                .at_least(1)
                .collect::<String>(),
        )
        .map(|(s, d)| {
            let mut full = String::new();
            if let Some(c) = s {
                full.push(c);
            }
            full.push_str(&d);
            str_parse::<i32>(full)
        });

    let d2 = any()
        .filter(|c: &char| c.is_ascii_digit())
        .repeated()
        .exactly(2)
        .collect::<String>();

    let date_part = year
        .then_ignore(just('-'))
        .then(d2.map(str_parse::<u8>))
        .then_ignore(just('-'))
        .then(d2.map(str_parse::<u8>))
        .map(|((year, month), day)| Date { year, month, day });

    let hour = sign_opt
        .then(
            any()
                .filter(|c: &char| c.is_ascii_digit())
                .repeated()
                .at_least(1)
                .collect::<String>(),
        )
        .map(|(s, d)| {
            let mut full = String::new();
            if let Some(c) = s {
                full.push(c);
            }
            full.push_str(&d);
            str_parse::<i32>(full)
        });

    let time_part = hour
        .then_ignore(just(':'))
        .then(d2.map(str_parse::<u8>))
        .then_ignore(just(':'))
        .then(d2.map(str_parse::<u8>))
        .then(
            just('.')
                .ignore_then(
                    any()
                        .filter(|c: &char| c.is_ascii_digit())
                        .repeated()
                        .at_least(1)
                        .at_most(6)
                        .collect::<String>()
                        .map(|s| str_parse_fraction::<u32>(s, 6)),
                )
                .or_not(),
        )
        .map(|(((hours, min), sec), micros)| Time {
            hours,
            min,
            sec,
            micros,
        });

    let date_or_datetime = just('@')
        .ignore_then(date_part)
        .then(just('T').ignore_then(time_part).or_not())
        .then_ignore(non_ident())
        .map(|(date, time)| {
            if let Some(time) = time {
                Literal::DateTime(date, time)
            } else {
                Literal::Date(date)
            }
        });

    let time = just('@')
        .ignore_then(time_part)
        .then_ignore(non_ident())
        .map(Literal::Time);

    choice((string, raw_string, number, boolean, date_or_datetime, time))
}

fn quoted_string<'src>(escaped: bool) -> impl LtLexer<'src, String> {
    custom(move |inp| quoted_string_of_quote(inp, '"', escaped))
}

fn quoted_string_of_quote<'src, 'p>(
    inp: &mut chumsky::input::InputRef<'src, 'p, &'src str, extra::Full<Cheap, (), ()>>,
    quote: char,
    escaping: bool,
) -> Result<String, LError> {
    // Count opening quotes.
    let mut count = 0usize;
    while inp.peek() == Some(quote) {
        inp.skip();
        count += 1;
    }

    if count == 0 {
        let cur = inp.cursor();
        return Err(Cheap::new(inp.span_since(&cur)));
    }

    // Even count → empty string (e.g. "" or """""").
    if count.is_multiple_of(2) {
        return Ok(String::new());
    }

    let mut result = String::new();

    loop {
        // Try to match the closing delimiter: exactly `count` consecutive quotes.
        let before_quotes = inp.cursor();
        let mut q = 0usize;
        while q < count && inp.peek() == Some(quote) {
            inp.skip();
            q += 1;
        }
        if q == count {
            return Ok(result);
        }
        if q > 0 {
            for _ in 0..q {
                result.push(quote);
            }
            continue;
        }

        // Handle escape sequences.
        if escaping && inp.peek() == Some('\\') {
            inp.skip();
            match inp.next() {
                Some('\\') => result.push('\\'),
                Some('/') => result.push('/'),
                Some('b') => result.push('\x08'),
                Some('f') => result.push('\x0C'),
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('u') if inp.peek() == Some('{') => {
                    inp.skip();
                    let mut hex = String::new();
                    while inp.peek().is_some_and(|c: char| c.is_ascii_hexdigit()) {
                        hex.push(inp.next().unwrap());
                    }
                    if inp.peek() == Some('}') {
                        inp.skip();
                    }
                    let c = u32::from_str_radix(&hex, 16)
                        .ok()
                        .and_then(char::from_u32)
                        .unwrap_or('\u{FFFD}');
                    result.push(c);
                }
                Some('x') => {
                    let mut hex = String::new();
                    for _ in 0..2 {
                        if inp.peek().is_some_and(|c: char| c.is_ascii_hexdigit()) {
                            hex.push(inp.next().unwrap());
                        }
                    }
                    let c = u32::from_str_radix(&hex, 16)
                        .ok()
                        .and_then(char::from_u32)
                        .unwrap_or('\u{FFFD}');
                    result.push(c);
                }
                Some(c) if c == quote => result.push(c),
                Some(c) => {
                    result.push('\\');
                    result.push(c);
                }
                None => {
                    return Err(Cheap::new(inp.span_since(&before_quotes)));
                }
            }
            continue;
        }

        // Regular character.
        match inp.next() {
            Some(c) => result.push(c),
            None => {
                return Err(Cheap::new(inp.span_since(&before_quotes)));
            }
        }
    }
}

fn non_ident<'src>() -> impl LtLexer<'src, ()> {
    any()
        .filter(|c: &char| c.is_alphanumeric() || *c == '_')
        .not()
        .ignored()
        .or(end())
}

// --- helpers ---

fn str_parse<T: FromStr>(x: String) -> T {
    let Ok(x) = x.parse::<T>() else {
        unreachable!()
    };
    x
}

fn str_parse_fraction<T: FromStr>(mut x: String, precision: usize) -> T {
    x.truncate(precision);
    while x.len() < precision {
        x.push('0');
    }
    let Ok(x) = x.parse::<T>() else {
        unreachable!()
    };
    x
}

fn is_semantic(t: &Token) -> bool {
    !matches!(t.kind, TokenKind::Comment(_) | TokenKind::NewLine)
}

fn span_to_span_in_source(span: SimpleSpan) -> SpanInSource {
    SpanInSource {
        start: span.start as u32,
        len: (span.end - span.start) as u16,
    }
}

fn convert_lexer_error(source: &str, e: LError, source_id: u16) -> Diagnostic {
    let start = e.span().start;
    let end = e.span().end;

    let found: &str = source.get(start..end).unwrap_or("");
    let span = Some(Span {
        start: start as u32,
        len: (end - start) as u16,
        source_id,
    });

    Diagnostic::new(format!("unexpected {found}"), DiagnosticCode::PARSER).with_span(span)
}
