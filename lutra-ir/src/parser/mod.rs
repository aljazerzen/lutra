mod grammar;
mod perror;
mod test;

pub use perror::{Error, Span};

use chumsky::{prelude::*, Stream};
use lutra_bin::ir;
use lutra_compiler::_lexer::{lex, Token, TokenKind};

pub fn parse(source: &str) -> (Option<ir::Program>, Vec<Error>) {
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

    let tokens = tokens
        .into_iter()
        .map(move |token| (token.kind, Span::from(token.span)));
    let eoi = Span {
        start: final_span,
        end: final_span,
    };
    Stream::from_iter(eoi, tokens)
}

#[track_caller]
pub fn _test_parse(source: &str) -> ir::Program {
    let (program, errors) = super::parse(source);

    if !errors.is_empty() {
        let mut message = "[PARSER ERRORS]:\n".to_string();
        for error in &errors {
            message += &format!(
                "{}-{}: {}\n",
                error.span.start, error.span.end, error.message
            );
        }
        panic!("{message}");
    }
    program.unwrap()
}
