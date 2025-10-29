pub(crate) mod def;
mod expr;
mod interpolation;
pub(crate) mod lexer;
pub(crate) mod perror;
mod test;
mod types;

use chumsky::{Stream, prelude::*};

use self::lexer::{Token, TokenKind};
use self::perror::PError;

use crate::codespan::Span;
use crate::diagnostic::Diagnostic;
use crate::pr;

pub fn is_submodule(source: &str) -> Result<bool, Vec<Diagnostic>> {
    let mut tokens = lexer::lex_source(source)?;
    Ok(tokens
        .next()
        .is_some_and(|t| matches!(t.kind, TokenKind::Keyword("submodule"))))
}

pub fn parse_source(
    source: &str,
    source_id: u16,
) -> (Option<pr::Source>, Vec<Diagnostic>, Vec<Token>) {
    let (tokens, mut errors) = lexer::lex_source_recovery(source, source_id);

    let mut trivia = Vec::new();

    let ast = if let Some(tokens) = tokens {
        trivia = tokens.trivia;
        let stream = prepare_stream(tokens.semantic, source_id);
        let (mut parsed, chum_errs) = def::source().parse_recovery(stream);

        if let Some(parsed) = &mut parsed {
            parsed.span.start = 0;
            parsed.span.len = source.len() as u16;
        }
        errors.extend(chum_errs.into_iter().map(Diagnostic::from));

        parsed
    } else {
        None
    };

    (ast, errors, trivia)
}

pub fn parse_expr(source: &str, source_id: u16) -> (Option<pr::Expr>, Vec<Diagnostic>) {
    let (tokens, mut errors) = lexer::lex_source_recovery(source, source_id);

    let ast = if let Some(tokens) = tokens {
        let stream = prepare_stream(tokens.semantic, source_id);

        let ty = types::type_expr();
        let expr = expr::expr(ty);

        let (ast, chum_errs) = expr.parse_recovery(stream);

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
    let final_offset = tokens
        .last()
        .map(|t| t.span.start + t.span.len as u32)
        .unwrap_or(0);

    let tokens = tokens
        .into_iter()
        .map(move |token| (token.kind, token.span.with_source_id(source_id)));
    let eoi = Span {
        start: final_offset,
        len: 0,
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

fn ident_keyword(kw: &'static str) -> impl Parser<TokenKind, (), Error = PError> + Clone {
    select! {
        TokenKind::Ident(ident) if ident == kw => ()
    }
}

fn keyword(kw: &'static str) -> impl Parser<TokenKind, (), Error = PError> + Clone {
    just(TokenKind::Keyword(kw)).ignored()
}

fn ctrl(char: char) -> impl Parser<TokenKind, (), Error = PError> + Clone {
    just(TokenKind::Control(char)).ignored()
}

/// Parse a sequence, allowing commas between items.
/// Doesn't include the surrounding delimiters.
fn sequence<'a, P, O>(parser: P) -> impl Parser<TokenKind, Vec<O>, Error = PError> + Clone
where
    P: Parser<TokenKind, O, Error = PError> + Clone,
    O: 'a,
{
    parser.separated_by(ctrl(',')).allow_trailing()
}
