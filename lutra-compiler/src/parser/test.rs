#![cfg(test)]

use chumsky::Parser;
use insta::assert_debug_snapshot;
use itertools::Itertools;
use std::fmt::Debug;

use crate::diagnostic::Diagnostic;
use crate::parser::lexer::TokenKind;
use crate::parser::perror::PError;
use crate::parser::prepare_stream;
use crate::parser::stmt;
use crate::pr;
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
        tracing::info!("ast: {ast:?}");
        return Err(parse_errors.into_iter().map(|e| e.into()).collect());
    }
    Ok(ast.unwrap())
}

/// Parse into statements
#[track_caller]
fn parse_source(source: &str) -> Result<Vec<Stmt>, Vec<Diagnostic>> {
    parse_with_parser(source, stmt::source())
}

#[track_caller]
fn parse_expr(source: &str) -> pr::Expr {
    let stmts = parse_with_parser(source, stmt::source()).unwrap();
    let stmt = stmts
        .into_iter()
        .exactly_one()
        .unwrap_or_else(|e| panic!("{e:#?}"));
    let var = stmt.kind.into_var_def().unwrap();
    let expr = var.value.unwrap();
    *expr
}

#[test]
fn parse_01() {
    assert!(parse_expr("func () -> 4").kind.is_func());
    assert!(parse_expr("func() -> 4").kind.is_func());
}

#[test]
fn parse_02() {
    assert!(parse_expr("(4..5)").kind.is_range());
    assert!(parse_expr("func () -> 4..5").kind.is_func());
    assert_debug_snapshot!(parse_expr("4..5").kind, @r#"
    Range(
        Range {
            start: Some(
                Expr {
                    kind: Literal(
                        Integer(
                            4,
                        ),
                    ),
                    span: Some(
                        0:0-1,
                    ),
                    ty: None,
                    ty_args: [],
                },
            ),
            end: Some(
                Expr {
                    kind: Literal(
                        Integer(
                            5,
                        ),
                    ),
                    span: Some(
                        0:3-4,
                    ),
                    ty: None,
                    ty_args: [],
                },
            ),
        },
    )
    "#);
    assert_debug_snapshot!(parse_expr("func <A, B: int8 | int16> () -> 1"), @r#"
    Expr {
        kind: Func(
            Func {
                return_ty: None,
                body: Expr {
                    kind: Literal(
                        Integer(
                            1,
                        ),
                    ),
                    span: Some(
                        0:32-33,
                    ),
                    ty: None,
                    ty_args: [],
                },
                params: [],
                ty_params: [
                    TyParam {
                        name: "A",
                        domain: Open,
                        span: Some(
                            0:6-7,
                        ),
                    },
                    TyParam {
                        name: "B",
                        domain: OneOf(
                            [
                                int8,
                                int16,
                            ],
                        ),
                        span: Some(
                            0:9-24,
                        ),
                    },
                ],
            },
        ),
        span: Some(
            0:0-33,
        ),
        ty: None,
        ty_args: [],
    }
    "#);
    assert_debug_snapshot!(parse_expr("func <T: {b = int64, ..}> () -> 1"), @r#"
    Expr {
        kind: Func(
            Func {
                return_ty: None,
                body: Expr {
                    kind: Literal(
                        Integer(
                            1,
                        ),
                    ),
                    span: Some(
                        0:32-33,
                    ),
                    ty: None,
                    ty_args: [],
                },
                params: [],
                ty_params: [
                    TyParam {
                        name: "T",
                        domain: TupleFields(
                            [
                                TyDomainTupleField {
                                    name: Some(
                                        "b",
                                    ),
                                    ty: int64,
                                },
                            ],
                        ),
                        span: Some(
                            0:6-24,
                        ),
                    },
                ],
            },
        ),
        span: Some(
            0:0-33,
        ),
        ty: None,
        ty_args: [],
    }
    "#);
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
    assert_debug_snapshot!(parse_source("Answer? T-H-A-T!").unwrap_err(), @r#"
    [
        Diagnostic {
            code: DiagnosticCode(
                "E0003",
            ),
            message: "unexpected  ",
            span: Some(
                0:7-8,
            ),
            additional: [],
        },
    ]
    "#);
}
