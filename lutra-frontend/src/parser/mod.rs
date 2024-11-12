mod expr;
mod interpolation;
pub(crate) mod lexer;
pub(crate) mod perror;
pub(crate) mod stmt;
mod test;
mod types;

use chumsky::{prelude::*, Stream};

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

/// Convert the output of the lexer into the input of the parser. Requires
/// supplying the original source code.
pub(crate) fn prepare_stream<'a>(
    tokens: Vec<lexer::Token>,
    source_id: u16,
) -> Stream<'a, TokenKind, Span, impl Iterator<Item = (TokenKind, Span)> + Sized + 'a> {
    let final_span = tokens.last().map(|t| t.span.end).unwrap_or(0);

    // We don't want comments in the AST (but we do intend to use them as part of
    // formatting)
    let semantic_tokens = tokens
        .into_iter()
        .filter(|token| !matches!(token.kind, TokenKind::Comment(_) | TokenKind::LineWrap(_)));

    let tokens = semantic_tokens
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
        TokenKind::Ident(ident) => ident,
        TokenKind::Keyword(ident) if &ident == "module" => ident,
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

/// Our approach to new lines is each item consumes new lines _before_ itself,
/// but not newlines after itself. This allows us to enforce new lines between
/// some items. The only place we handle new lines after an item is in the root
/// parser.
pub(crate) fn new_line() -> impl Parser<TokenKind, (), Error = PError> + Clone {
    just(TokenKind::NewLine)
        // Start is considered a new line, so we can enforce things start on a new
        // line while allowing them to be at the beginning of a file
        .or(just(TokenKind::Start))
        .ignored()
        .labelled("new line")
}

fn ctrl(char: char) -> impl Parser<TokenKind, (), Error = PError> + Clone {
    just(TokenKind::Control(char)).ignored()
}

/// Parse a sequence, allowing commas and new lines between items. Doesn't
/// include the surrounding delimiters.
fn sequence<'a, P, O>(parser: P) -> impl Parser<TokenKind, Vec<O>, Error = PError> + Clone + 'a
where
    P: Parser<TokenKind, O, Error = PError> + Clone + 'a,
    O: 'a,
{
    parser
        .separated_by(ctrl(',').then_ignore(new_line().repeated()))
        .allow_trailing()
        // Note because we pad rather than only take the ending new line, we
        // can't put items that require a new line in a tuple, like:
        //
        // ```
        // {
        //   !# doc comment
        //   a,
        // }
        // ```
        // ...but I'm not sure there's a way around it, since we do need to
        // consume newlines in tuples...
        .padded_by(new_line().repeated())
}

fn pipe() -> impl Parser<TokenKind, (), Error = PError> + Clone {
    ctrl('|')
        .ignored()
        .or(new_line().repeated().at_least(1).ignored())
}
