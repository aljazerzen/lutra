mod def;
mod expr;
mod helpers;
mod interpolation;
mod lexer;
mod test;
mod types;

pub use self::lexer::{Token, TokenKind, lex_source_recovery};

use chumsky::input::Input as _;
use chumsky::prelude::*;

use crate::Span;
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
        let tokens = prepare_tokens(tokens.semantic, source_id);

        let (ast, errs) = def::source().parse(tokens.as_input()).into_output_errors();

        let mut parsed = ast;
        if let Some(parsed) = &mut parsed {
            parsed.span.start = 0;
            parsed.span.len = source.len() as u16;
        }

        errors.extend(errs.into_iter().map(Diagnostic::from));

        parsed
    } else {
        None
    };

    (ast, errors, trivia)
}

pub fn parse_expr(source: &str, source_id: u16) -> (Option<pr::Expr>, Vec<Diagnostic>) {
    let (tokens, mut errors) = lexer::lex_source_recovery(source, source_id);

    let ast = if let Some(tokens) = tokens {
        let tokens = prepare_tokens(tokens.semantic, source_id);

        let (ast, errs) = expr::expr(types::type_expr())
            .parse(tokens.as_input())
            .into_output_errors();
        errors.extend(errs.into_iter().map(Diagnostic::from));
        ast
    } else {
        None
    };

    (ast, errors)
}

pub fn parse_path(source: &str) -> Option<pr::Path> {
    let tokens = lexer::lex_source(source).ok()?;
    let tokens = prepare_tokens(tokens.collect(), 0);

    let (path, _) = expr::path().parse(tokens.as_input()).into_output_errors();
    path
}

/// Convert the output of the lexer into token pairs and end-of-input span.
fn prepare_tokens(tokens: Vec<lexer::Token>, source_id: u16) -> ParserTokens {
    let pairs: Vec<(TokenKind, Span)> = tokens
        .into_iter()
        .map(|t| (t.kind, t.span.with_source_id(source_id)))
        .collect();

    let eoi = Span {
        start: pairs.last().map(|(_, s)| s.end()).unwrap_or(0),
        len: 0,
        source_id,
    };

    ParserTokens { pairs, eoi }
}

struct ParserTokens {
    pairs: Vec<(TokenKind, Span)>,
    eoi: Span,
}

impl ParserTokens {
    fn as_input(&self) -> PInput<'_> {
        self.pairs.as_slice().split_token_span(self.eoi)
    }
}

/// Parser error type. `'src` is the lifetime of the input being parsed; it
/// bounds token references stored inside the Rich error.
pub(crate) type PError<'src> = Rich<'src, lexer::TokenKind, Span>;

/// Type of parser Extra we use (full `Rich` errors, for error reporting).
pub(crate) type PExtra<'src> = chumsky::extra::Err<PError<'src>>;

/// Type of parser input we use.
///
/// Uses a slice-based input so that backtracking is O(1) (cursor is just an
/// index) instead of cloning the entire remaining iterator on every checkpoint.
type PInput<'src> =
    chumsky::input::MappedInput<'src, lexer::TokenKind, Span, &'src [(lexer::TokenKind, Span)]>;
