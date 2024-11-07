#![cfg(test)]

use chumsky::Parser;
use insta::assert_debug_snapshot;
use std::fmt::Debug;

use crate::error::Diagnostic;
use crate::parser::lexer::TokenKind;
use crate::parser::perror::PError;
use crate::parser::prepare_stream;
use crate::parser::stmt;
use crate::pr::Stmt;

/// Parse source code based on the supplied parser.
///
/// Use this to test any parser!
pub(crate) fn parse_with_parser<O: Debug>(
    source: &str,
    parser: impl Parser<TokenKind, O, Error = PError>,
) -> Result<O, Vec<Diagnostic>> {
    let tokens = crate::parser::lexer::lex_source(source)?;
    let stream = prepare_stream(tokens.0, 0);

    // TODO: possibly should check we consume all the input? Either with an
    // end() parser or some other way (but if we add an end parser then this
    // func doesn't work with `source`, which has its own end parser...)
    let (ast, parse_errors) = parser.parse_recovery_verbose(stream);

    if !parse_errors.is_empty() {
        log::info!("ast: {ast:?}");
        return Err(parse_errors.into_iter().map(|e| e.into()).collect());
    }
    Ok(ast.unwrap())
}

/// Parse into statements
pub(crate) fn parse_source(source: &str) -> Result<Vec<Stmt>, Vec<Diagnostic>> {
    parse_with_parser(source, stmt::source())
}

#[test]
fn test_error_unicode_string() {
    // Test various unicode strings successfully parse errors. We were
    // getting loops in the lexer before.
    parse_source("s’ ").unwrap_err();
    parse_source("s’").unwrap_err();
    parse_source(" s’").unwrap_err();
    parse_source(" ’ s").unwrap_err();
    parse_source("’s").unwrap_err();
    parse_source("👍 s’").unwrap_err();

    let source = "Mississippi has four S’s and four I’s.";
    assert_debug_snapshot!(parse_source(source).unwrap_err(), @r#"
    [
        Diagnostic {
            code: DiagnosticCode(
                "E0003",
            ),
            message: "unexpected ’",
            span: Some(
                0:22-23,
            ),
            additional: [],
        },
        Diagnostic {
            code: DiagnosticCode(
                "E0003",
            ),
            message: "unexpected ’",
            span: Some(
                0:35-36,
            ),
            additional: [],
        },
    ]
    "#);
}

#[test]
fn test_error_unexpected() {
    assert_debug_snapshot!(parse_source("Answer: T-H-A-T!").unwrap_err(), @r#"
    [
        Diagnostic {
            code: DiagnosticCode(
                "E0003",
            ),
            message: "unexpected :",
            span: Some(
                0:6-7,
            ),
            additional: [],
        },
    ]
    "#);
}
