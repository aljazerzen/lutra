#![cfg(test)]

use chumsky::prelude::*;
use insta::assert_debug_snapshot;

use crate::diagnostic::Diagnostic;
use crate::parser::{PExtra, PInput, prepare_tokens};
use crate::parser::{def, expr, types};

use crate::pr;

/// Parse source code using a parser produced by the given factory.
///
/// The factory is called after the token slice is prepared, so the parser's
/// `'src` lifetime is correctly tied to the slice — avoiding the HRTB issue
/// that arises when a pre-built parser is passed as an argument.
#[track_caller]
pub(super) fn parse_with<O, F>(source: &str, make_parser: F) -> Result<O, Vec<Diagnostic>>
where
    O: std::fmt::Debug,
    F: for<'src> FnOnce(&'src ()) -> Box<dyn Parser<'src, PInput<'src>, O, PExtra<'src>> + 'src>,
{
    let tokens = crate::parser::lexer::lex_source(source)?.collect();
    let tokens = prepare_tokens(tokens, 0);

    let parser = make_parser(&());
    let (ast, parse_errors) = parser.parse(tokens.as_input()).into_output_errors();

    if !parse_errors.is_empty() {
        tracing::info!("ast: {ast:?}");
        return Err(parse_errors.into_iter().map(|e| e.into()).collect());
    }
    Ok(ast.unwrap())
}

#[track_caller]
fn parse_expr(source: &str) -> pr::Expr {
    parse_with(source, |_| Box::new(expr::expr(types::type_expr()))).unwrap()
}

#[track_caller]
fn parse_func(source: &str) -> pr::Expr {
    let source = parse_with(source, |_| Box::new(def::source())).unwrap();
    let (_, def) = source.root.defs.into_iter().next().unwrap();
    *def.kind.into_expr().unwrap().value
}

#[test]
fn parse_01() {
    assert!(parse_expr("func () -> 4").kind.is_func());
    assert!(parse_expr("func() -> 4").kind.is_func());
    assert_debug_snapshot!(parse_expr("func() -> false").kind.as_func().unwrap().body.as_ref().unwrap(), @r"
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
    assert!(
        parse_expr("(4..5)")
            .kind
            .as_nested()
            .unwrap()
            .kind
            .is_range()
    );
    assert!(parse_expr("func () -> 4..5").kind.is_func());
    assert_debug_snapshot!(parse_expr("4..5").kind, @r#"
    Range(
        Range {
            start: Some(
                Expr {
                    kind: Literal(
                        Number(
                            "4",
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
                        Number(
                            "5",
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
                params: [],
                return_ty: None,
                body: Some(
                    Expr {
                        kind: Literal(
                            Number(
                                "1",
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
                ),
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
                                Ty {
                                    kind: Primitive(
                                        int8,
                                    ),
                                    span: Some(
                                        0:21-25,
                                    ),
                                    name: None,
                                    scope_id: None,
                                    target: None,
                                    variants_force_ptr: [],
                                },
                                Ty {
                                    kind: Primitive(
                                        int16,
                                    ),
                                    span: Some(
                                        0:28-33,
                                    ),
                                    name: None,
                                    scope_id: None,
                                    target: None,
                                    variants_force_ptr: [],
                                },
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
                params: [],
                return_ty: None,
                body: Some(
                    Expr {
                        kind: Literal(
                            Number(
                                "1",
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
                ),
                ty_params: [
                    TyParam {
                        name: "T",
                        domain: TupleHasFields(
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
                                        variants_force_ptr: [],
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
            .open => 5,
            .closed(reason) => 6 + 7,
            anything_else => 0,
        }
    "#).kind, @r#"
    Match(
        Match {
            subject: Expr {
                kind: Lookup {
                    base: Expr {
                        kind: Ident(
                            item,
                        ),
                        span: Some(
                            0:15-19,
                        ),
                        ty: None,
                        ty_args: [],
                        scope_id: None,
                        target: None,
                    },
                    lookup: Name(
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
                            "open",
                            None,
                        ),
                        span: 0:41-46,
                        variant_tag: None,
                    },
                    value: Expr {
                        kind: Literal(
                            Number(
                                "5",
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
                            "closed",
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
                                        Number(
                                            "6",
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
                                        Number(
                                            "7",
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
                            Number(
                                "0",
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
            .open(timestamp) => 1,
            .closed(.Other(inner)) => 1,
        }
    "#).kind, @r#"
    Match(
        Match {
            subject: Expr {
                kind: Literal(
                    Number(
                        "1",
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
                            "open",
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
                            Number(
                                "1",
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
                            "closed",
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
                            Number(
                                "1",
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
    assert_debug_snapshot!(parse_expr(r"-1").kind, @r#"
    Literal(
        Number(
            "-1",
        ),
    )
    "#);
}

#[test]
fn parse_09() {
    assert_debug_snapshot!(parse_expr(r"{1, ..x, 2}").kind, @r#"
    Tuple(
        [
            TupleField {
                name: None,
                unpack: false,
                expr: Expr {
                    kind: Literal(
                        Number(
                            "1",
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
            TupleField {
                name: None,
                unpack: true,
                expr: Expr {
                    kind: Ident(
                        x,
                    ),
                    span: Some(
                        0:6-7,
                    ),
                    ty: None,
                    ty_args: [],
                    scope_id: None,
                    target: None,
                },
            },
            TupleField {
                name: None,
                unpack: false,
                expr: Expr {
                    kind: Literal(
                        Number(
                            "2",
                        ),
                    ),
                    span: Some(
                        0:9-10,
                    ),
                    ty: None,
                    ty_args: [],
                    scope_id: None,
                    target: None,
                },
            },
        ],
    )
    "#);
}

#[test]
fn parse_10() {
    assert_debug_snapshot!(parse_expr(r"{1 | to_int32}"), @r#"
    Expr {
        kind: Tuple(
            [
                TupleField {
                    name: None,
                    unpack: false,
                    expr: Expr {
                        kind: Binary(
                            BinaryExpr {
                                left: Expr {
                                    kind: Literal(
                                        Number(
                                            "1",
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
                                op: Pipe,
                                right: Expr {
                                    kind: Ident(
                                        to_int32,
                                    ),
                                    span: Some(
                                        0:5-13,
                                    ),
                                    ty: None,
                                    ty_args: [],
                                    scope_id: None,
                                    target: None,
                                },
                            },
                        ),
                        span: Some(
                            0:1-13,
                        ),
                        ty: None,
                        ty_args: [],
                        scope_id: None,
                        target: None,
                    },
                },
            ],
        ),
        span: Some(
            0:0-14,
        ),
        ty: None,
        ty_args: [],
        scope_id: None,
        target: None,
    }
    "#);
    assert_debug_snapshot!(parse_expr(r"1 + 2 | std::to_int32").kind, @r#"
    Binary(
        BinaryExpr {
            left: Expr {
                kind: Binary(
                    BinaryExpr {
                        left: Expr {
                            kind: Literal(
                                Number(
                                    "1",
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
                        op: Add,
                        right: Expr {
                            kind: Literal(
                                Number(
                                    "2",
                                ),
                            ),
                            span: Some(
                                0:4-5,
                            ),
                            ty: None,
                            ty_args: [],
                            scope_id: None,
                            target: None,
                        },
                    },
                ),
                span: Some(
                    0:0-5,
                ),
                ty: None,
                ty_args: [],
                scope_id: None,
                target: None,
            },
            op: Pipe,
            right: Expr {
                kind: Ident(
                    std::to_int32,
                ),
                span: Some(
                    0:8-21,
                ),
                ty: None,
                ty_args: [],
                scope_id: None,
                target: None,
            },
        },
    )
    "#);
    assert_debug_snapshot!(parse_expr(r"1 | to_int32 | to_int32").kind, @r#"
    Binary(
        BinaryExpr {
            left: Expr {
                kind: Binary(
                    BinaryExpr {
                        left: Expr {
                            kind: Literal(
                                Number(
                                    "1",
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
                        op: Pipe,
                        right: Expr {
                            kind: Ident(
                                to_int32,
                            ),
                            span: Some(
                                0:4-12,
                            ),
                            ty: None,
                            ty_args: [],
                            scope_id: None,
                            target: None,
                        },
                    },
                ),
                span: Some(
                    0:0-12,
                ),
                ty: None,
                ty_args: [],
                scope_id: None,
                target: None,
            },
            op: Pipe,
            right: Expr {
                kind: Ident(
                    to_int32,
                ),
                span: Some(
                    0:15-23,
                ),
                ty: None,
                ty_args: [],
                scope_id: None,
                target: None,
            },
        },
    )
    "#);
    assert_debug_snapshot!(parse_expr(r"1 | x -> x | to_int32").kind, @r#"
    Binary(
        BinaryExpr {
            left: Expr {
                kind: Literal(
                    Number(
                        "1",
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
            op: Pipe,
            right: Expr {
                kind: FuncShort(
                    FuncShort {
                        param: FuncParam {
                            constant: false,
                            label: None,
                            name: "x",
                            ty: None,
                            span: 0:4-5,
                        },
                        body: Expr {
                            kind: Binary(
                                BinaryExpr {
                                    left: Expr {
                                        kind: Ident(
                                            x,
                                        ),
                                        span: Some(
                                            0:9-10,
                                        ),
                                        ty: None,
                                        ty_args: [],
                                        scope_id: None,
                                        target: None,
                                    },
                                    op: Pipe,
                                    right: Expr {
                                        kind: Ident(
                                            to_int32,
                                        ),
                                        span: Some(
                                            0:13-21,
                                        ),
                                        ty: None,
                                        ty_args: [],
                                        scope_id: None,
                                        target: None,
                                    },
                                },
                            ),
                            span: Some(
                                0:9-21,
                            ),
                            ty: None,
                            ty_args: [],
                            scope_id: None,
                            target: None,
                        },
                    },
                ),
                span: Some(
                    0:4-21,
                ),
                ty: None,
                ty_args: [],
                scope_id: None,
                target: None,
            },
        },
    )
    "#);
}

#[test]
fn test_error_unicode_string() {
    // Test various unicode strings successfully parse errors. We were
    // getting loops in the lexer before.
    parse_expr("\"s’ \"");
    parse_expr("\"s’\"");
    parse_expr("\" s’\"");
    parse_expr("\" ’ s\"");
    parse_expr("\"’s\"");
    parse_expr("\"👍 s’\"");

    assert_debug_snapshot!(parse_expr(
        "\"Mississippi has four S’s and four I’s.\""
    ), @r#"
    Expr {
        kind: Literal(
            Text(
                "Mississippi has four S’s and four I’s.",
            ),
        ),
        span: Some(
            0:0-44,
        ),
        ty: None,
        ty_args: [],
        scope_id: None,
        target: None,
    }
    "#);
}
