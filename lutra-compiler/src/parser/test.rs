#![cfg(test)]

use chumsky::Parser;
use indexmap::IndexMap;
use insta::assert_debug_snapshot;
use std::fmt::Debug;

use crate::diagnostic::Diagnostic;
use crate::parser::def;
use crate::parser::expr;
use crate::parser::lexer::TokenKind;
use crate::parser::perror::PError;
use crate::parser::prepare_stream;
use crate::parser::types;
use crate::pr;
use crate::pr::Def;

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
fn parse_source(source: &str) -> Result<IndexMap<String, Def>, Vec<Diagnostic>> {
    parse_with_parser(source, def::source()).map(|s| s.defs)
}

#[track_caller]
fn parse_expr(source: &str) -> pr::Expr {
    let parser = expr::expr(types::type_expr());
    parse_with_parser(source, parser).unwrap()
}

#[track_caller]
fn parse_func(source: &str) -> pr::Expr {
    let module = parse_with_parser(source, def::source()).unwrap();
    let (_, def) = module.defs.into_iter().next().unwrap();
    *def.kind.into_expr().unwrap().value.unwrap()
}

#[test]
fn parse_01() {
    assert!(parse_expr("func () -> 4").kind.is_func());
    assert!(parse_expr("func() -> 4").kind.is_func());
    assert_debug_snapshot!(parse_expr("func() -> false").kind.as_func().unwrap().body, @r"
    Expr {
        kind: Literal(
            Boolean(
                false,
            ),
        ),
        span: Some(
            0:10-15,
        ),
        ty: None,
        ty_args: [],
        scope_id: None,
        target: None,
    }
    ");
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
                    scope_id: None,
                    target: None,
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
                    scope_id: None,
                    target: None,
                },
            ),
        },
    )
    "#);
}
#[test]
fn parse_03() {
    assert_debug_snapshot!(parse_func("func f() where A, B: int8 | int16 -> 1"), @r#"
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
                        0:37-38,
                    ),
                    ty: None,
                    ty_args: [],
                    scope_id: None,
                    target: None,
                },
                params: [],
                ty_params: [
                    TyParam {
                        name: "A",
                        domain: Open,
                        span: Some(
                            0:15-16,
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
                            0:18-33,
                        ),
                    },
                ],
            },
        ),
        span: Some(
            0:0-38,
        ),
        ty: None,
        ty_args: [],
        scope_id: None,
        target: None,
    }
    "#);
}
#[test]
fn parse_04() {
    assert_debug_snapshot!(parse_func("func f() where T: {b: int64, ..} -> 1"), @r#"
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
                        0:36-37,
                    ),
                    ty: None,
                    ty_args: [],
                    scope_id: None,
                    target: None,
                },
                params: [],
                ty_params: [
                    TyParam {
                        name: "T",
                        domain: TupleFields(
                            [
                                TyDomainTupleField {
                                    location: Name(
                                        "b",
                                    ),
                                    ty: Ty {
                                        kind: Primitive(
                                            int64,
                                        ),
                                        span: Some(
                                            0:22-27,
                                        ),
                                        name: None,
                                        scope_id: None,
                                        target: None,
                                    },
                                },
                            ],
                        ),
                        span: Some(
                            0:15-32,
                        ),
                    },
                ],
            },
        ),
        span: Some(
            0:0-37,
        ),
        ty: None,
        ty_args: [],
        scope_id: None,
        target: None,
    }
    "#);
}

#[test]
fn parse_05() {
    assert_debug_snapshot!(parse_expr(r#"
        match item.status {
            .Open => 5,
            .Closed(reason) => 6 + 7,
            anything_else => 0,
        }
    "#).kind, @r#"
    Match(
        Match {
            subject: Expr {
                kind: Indirection {
                    base: Expr {
                        kind: Ident(
                            [
                                "item",
                            ],
                        ),
                        span: Some(
                            0:15-19,
                        ),
                        ty: None,
                        ty_args: [],
                        scope_id: None,
                        target: None,
                    },
                    field: Name(
                        "status",
                    ),
                },
                span: Some(
                    0:19-26,
                ),
                ty: None,
                ty_args: [],
                scope_id: None,
                target: None,
            },
            branches: [
                MatchBranch {
                    pattern: Pattern {
                        kind: Enum(
                            "Open",
                            None,
                        ),
                        span: 0:41-46,
                        variant_tag: None,
                    },
                    value: Expr {
                        kind: Literal(
                            Integer(
                                5,
                            ),
                        ),
                        span: Some(
                            0:50-51,
                        ),
                        ty: None,
                        ty_args: [],
                        scope_id: None,
                        target: None,
                    },
                },
                MatchBranch {
                    pattern: Pattern {
                        kind: Enum(
                            "Closed",
                            Some(
                                Pattern {
                                    kind: Bind(
                                        "reason",
                                    ),
                                    span: 0:73-79,
                                    variant_tag: None,
                                },
                            ),
                        ),
                        span: 0:65-80,
                        variant_tag: None,
                    },
                    value: Expr {
                        kind: Binary(
                            BinaryExpr {
                                left: Expr {
                                    kind: Literal(
                                        Integer(
                                            6,
                                        ),
                                    ),
                                    span: Some(
                                        0:84-85,
                                    ),
                                    ty: None,
                                    ty_args: [],
                                    scope_id: None,
                                    target: None,
                                },
                                op: Add,
                                right: Expr {
                                    kind: Literal(
                                        Integer(
                                            7,
                                        ),
                                    ),
                                    span: Some(
                                        0:88-89,
                                    ),
                                    ty: None,
                                    ty_args: [],
                                    scope_id: None,
                                    target: None,
                                },
                            },
                        ),
                        span: Some(
                            0:84-89,
                        ),
                        ty: None,
                        ty_args: [],
                        scope_id: None,
                        target: None,
                    },
                },
                MatchBranch {
                    pattern: Pattern {
                        kind: Bind(
                            "anything_else",
                        ),
                        span: 0:103-116,
                        variant_tag: None,
                    },
                    value: Expr {
                        kind: Literal(
                            Integer(
                                0,
                            ),
                        ),
                        span: Some(
                            0:120-121,
                        ),
                        ty: None,
                        ty_args: [],
                        scope_id: None,
                        target: None,
                    },
                },
            ],
        },
    )
    "#);
}

#[test]
fn parse_06() {
    assert_debug_snapshot!(parse_expr(r#"
        match 1 {
            .Open(timestamp) => 1,
            .Closed(.Other(inner)) => 1,
        }
    "#).kind, @r#"
    Match(
        Match {
            subject: Expr {
                kind: Literal(
                    Integer(
                        1,
                    ),
                ),
                span: Some(
                    0:15-16,
                ),
                ty: None,
                ty_args: [],
                scope_id: None,
                target: None,
            },
            branches: [
                MatchBranch {
                    pattern: Pattern {
                        kind: Enum(
                            "Open",
                            Some(
                                Pattern {
                                    kind: Bind(
                                        "timestamp",
                                    ),
                                    span: 0:37-46,
                                    variant_tag: None,
                                },
                            ),
                        ),
                        span: 0:31-47,
                        variant_tag: None,
                    },
                    value: Expr {
                        kind: Literal(
                            Integer(
                                1,
                            ),
                        ),
                        span: Some(
                            0:51-52,
                        ),
                        ty: None,
                        ty_args: [],
                        scope_id: None,
                        target: None,
                    },
                },
                MatchBranch {
                    pattern: Pattern {
                        kind: Enum(
                            "Closed",
                            Some(
                                Pattern {
                                    kind: Enum(
                                        "Other",
                                        Some(
                                            Pattern {
                                                kind: Bind(
                                                    "inner",
                                                ),
                                                span: 0:81-86,
                                                variant_tag: None,
                                            },
                                        ),
                                    ),
                                    span: 0:74-87,
                                    variant_tag: None,
                                },
                            ),
                        ),
                        span: 0:66-88,
                        variant_tag: None,
                    },
                    value: Expr {
                        kind: Literal(
                            Integer(
                                1,
                            ),
                        ),
                        span: Some(
                            0:92-93,
                        ),
                        ty: None,
                        ty_args: [],
                        scope_id: None,
                        target: None,
                    },
                },
            ],
        },
    )
    "#);
}

#[test]
fn parse_07() {
    assert_debug_snapshot!(parse_expr(r"-1").kind, @r"
    Unary(
        UnaryExpr {
            op: Neg,
            expr: Expr {
                kind: Literal(
                    Integer(
                        1,
                    ),
                ),
                span: Some(
                    0:1-2,
                ),
                ty: None,
                ty_args: [],
                scope_id: None,
                target: None,
            },
        },
    )
    ");
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
