mod expr;
mod interpolation;
pub(crate) mod lexer;
pub(crate) mod perror;
pub(crate) mod stmt;
mod test;
mod types;

use chumsky::{Stream, prelude::*};

use self::lexer::TokenKind;
use self::perror::PError;

use crate::diagnostic::Diagnostic;
use crate::pr;
use crate::span::Span;

pub fn parse_source(source: &str, source_id: u16) -> (Option<Vec<pr::Stmt>>, Vec<Diagnostic>) {
    let (tokens, mut errors) = lexer::lex_source_recovery(source, source_id);

    let ast = if let Some(tokens) = tokens {
        let stream = prepare_stream(tokens, source_id);
        let (ast, chum_errs) = stmt::source().parse_recovery(stream);

        errors.extend(chum_errs.into_iter().map(Diagnostic::from));
        ast
    } else {
        None
    };

    (ast, errors)
}

pub fn parse_expr(source: &str, source_id: u16) -> (Option<pr::Expr>, Vec<Diagnostic>) {
    let (tokens, mut errors) = lexer::lex_source_recovery(source, source_id);

    let ast = if let Some(tokens) = tokens {
        let stream = prepare_stream(tokens, source_id);
        let (ast, chum_errs) = expr::expr().parse_recovery(stream);

        errors.extend(chum_errs.into_iter().map(Diagnostic::from));
        ast
    } else {
        None
    };

    (ast, errors)
}

/// Convert the output of the lexer into the input of the parser. Requires
/// supplying the original source code.
pub(crate) fn prepare_stream<'a>(
    tokens: Vec<lexer::Token>,
    source_id: u16,
) -> Stream<'a, TokenKind, Span, impl Iterator<Item = (TokenKind, Span)> + Sized + 'a> {
    let final_span = tokens.last().map(|t| t.span.end).unwrap_or(0);

    let tokens = tokens
        .into_iter()
        .map(move |token| (token.kind, Span::new(source_id, token.span)));
    let eoi = Span {
        start: final_span,
        end: final_span,
        source_id,
    };
    Stream::from_iter(eoi, tokens)
}

fn ident_part() -> impl Parser<TokenKind, String, Error = PError> + Clone {
    select! {
        TokenKind::Ident(ident) => ident
    }
    .map_err(|e: PError| {
        PError::expected_input_found(
            e.span(),
            [Some(TokenKind::Ident("".to_string()))],
            e.found().cloned(),
        )
    })
}

fn keyword(kw: &'static str) -> impl Parser<TokenKind, (), Error = PError> + Clone {
    just(TokenKind::Keyword(kw.to_string())).ignored()
}

fn ctrl(char: char) -> impl Parser<TokenKind, (), Error = PError> + Clone {
    just(TokenKind::Control(char)).ignored()
}

/// Parse a sequence, allowing commas and new lines between items. Doesn't
/// include the surrounding delimiters.
fn sequence<'a, P, O>(parser: P) -> impl Parser<TokenKind, Vec<O>, Error = PError> + Clone
where
    P: Parser<TokenKind, O, Error = PError> + Clone,
    O: 'a,
{
    parser.separated_by(ctrl(',')).allow_trailing()
}

fn pipe() -> impl Parser<TokenKind, (), Error = PError> + Clone {
    ctrl('|').ignored()
}
