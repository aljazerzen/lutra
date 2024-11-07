mod grammar;
mod perror;
pub mod test;

pub use perror::{Error, Span};

use crate::ir;
use chumsky::{prelude::*, Stream};
use lutra_frontend::_lexer::{lex, Token, TokenKind};

pub fn parse_source(source: &str) -> (Option<ir::Program>, Vec<Error>) {
    let (tokens, errors) = lex(source, 0);

    let mut errors: Vec<_> = errors.into_iter().map(Error::from).collect();

    let ast = if let Some(tokens) = tokens {
        let stream = prepare_stream(tokens);
        let (ast, parse_errors) = grammar::program().parse_recovery(stream);

        errors.extend(
            parse_errors
                .into_iter()
                .map(|p| Error::from_parser(p, source)),
        );

        ast
    } else {
        None
    };

    (ast, errors)
}

/// Convert the output of the lexer into the input of the parser.
fn prepare_stream<'a>(
    tokens: Vec<Token>,
) -> Stream<'a, TokenKind, Span, impl Iterator<Item = (TokenKind, Span)> + Sized + 'a> {
    let final_span = tokens.last().map(|t| t.span.end).unwrap_or(0);

    // We don't want comments in the AST (but we do intend to use them as part of
    // formatting)
    let semantic_tokens = tokens.into_iter().filter(|token| {
        !matches!(
            token.kind,
            TokenKind::Comment(_) | TokenKind::LineWrap(_) | TokenKind::Start | TokenKind::NewLine
        )
    });

    let tokens = semantic_tokens
        .into_iter()
        .map(move |token| (token.kind, Span::from(token.span)));
    let eoi = Span {
        start: final_span,
        end: final_span,
    };
    Stream::from_iter(eoi, tokens)
}
