mod grammar;
mod perror;
mod test;

pub use perror::{Error, Span};

use chumsky::prelude::*;
use lutra_bin::ir;
use lutra_compiler::_lexer::{Token, TokenKind, lex};

// Possible improvment: two-pass parsing.
// First pass parses using fast EmptyErr, and on errors,
// parses using PError, to produce rich diagnostics.
// This was removed because it's too much moving parts for
// not enough benefit (it gave -0.4ms on successful parse of std.lt).

pub fn parse(source: &str) -> (Option<ir::Program>, Vec<Error>) {
    let (tokens, errors) = lex(source, 0);

    let mut errors: Vec<_> = errors.into_iter().map(Error::from).collect();

    let ast = if let Some(tokens) = tokens {
        let (pairs, eoi) = prepare_pairs(tokens.semantic);
        let input = pairs.as_slice().split_token_span(eoi);
        let (ast, parse_errors) = grammar::program().parse(input).into_output_errors();

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

/// Build the (token-pair vec, end-of-input span) needed for `split_token_span`.
fn prepare_pairs(tokens: Vec<Token>) -> (Vec<(TokenKind, Span)>, Span) {
    let final_end = tokens
        .last()
        .map(|t| t.span.start as usize + t.span.len as usize)
        .unwrap_or(0);

    let eoi = Span {
        start: final_end,
        end: final_end,
    };

    let pairs = tokens
        .into_iter()
        .map(|t| {
            (
                t.kind,
                Span {
                    start: t.span.start as usize,
                    end: t.span.start as usize + t.span.len as usize,
                },
            )
        })
        .collect();

    (pairs, eoi)
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
