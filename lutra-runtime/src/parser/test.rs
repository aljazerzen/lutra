#![cfg(test)]

use insta::assert_debug_snapshot;

#[track_caller]
pub fn parse(source: &str) -> crate::ir::Program {
    let (program, errors) = super::parse_source(source);
    for error in &errors {
        eprintln!("{}-{}: {}", error.span.start, error.span.end, error.message);
    }
    if !errors.is_empty() {
        panic!();
    }
    program.unwrap()
}

#[test]
fn parse_01() {
    assert_debug_snapshot!(parse(r#"
    let external = [std_int_add];

    let main =
        let 1 = func 2 -> [fn.2+0, fn.2+0, fn.2+0];
        let 2 = var.1;
        {
            (3.5) | var.2,
            (6, 7) | func 3 -> [ fn.3+0, fn.3+1 ],
            (6, 2) | external.0
        }.1[0]
    "#), @r#"
    Program {
        externals: [
            ExternalSymbol {
                id: "std_int_add",
            },
        ],
        main: Expr {
            kind: Binding(
                Binding {
                    symbol: Sid(
                        1073741825,
                    ),
                    expr: Expr {
                        kind: Function(
                            Function {
                                symbol_ns: Sid(
                                    2147484160,
                                ),
                                body: Expr {
                                    kind: Array(
                                        [
                                            Expr {
                                                kind: Pointer(
                                                    Sid(
                                                        2147484160,
                                                    ),
                                                ),
                                            },
                                            Expr {
                                                kind: Pointer(
                                                    Sid(
                                                        2147484160,
                                                    ),
                                                ),
                                            },
                                            Expr {
                                                kind: Pointer(
                                                    Sid(
                                                        2147484160,
                                                    ),
                                                ),
                                            },
                                        ],
                                    ),
                                },
                            },
                        ),
                    },
                    main: Expr {
                        kind: Binding(
                            Binding {
                                symbol: Sid(
                                    1073741826,
                                ),
                                expr: Expr {
                                    kind: Pointer(
                                        Sid(
                                            1073741825,
                                        ),
                                    ),
                                },
                                main: Expr {
                                    kind: ArrayLookup(
                                        ArrayLookup {
                                            base: Expr {
                                                kind: TupleLookup(
                                                    TupleLookup {
                                                        base: Expr {
                                                            kind: Tuple(
                                                                [
                                                                    Expr {
                                                                        kind: Call(
                                                                            Call {
                                                                                function: Expr {
                                                                                    kind: Pointer(
                                                                                        Sid(
                                                                                            1073741826,
                                                                                        ),
                                                                                    ),
                                                                                },
                                                                                args: [
                                                                                    Expr {
                                                                                        kind: Literal(
                                                                                            Float(
                                                                                                3.5,
                                                                                            ),
                                                                                        ),
                                                                                    },
                                                                                ],
                                                                            },
                                                                        ),
                                                                    },
                                                                    Expr {
                                                                        kind: Call(
                                                                            Call {
                                                                                function: Expr {
                                                                                    kind: Function(
                                                                                        Function {
                                                                                            symbol_ns: Sid(
                                                                                                2147484416,
                                                                                            ),
                                                                                            body: Expr {
                                                                                                kind: Array(
                                                                                                    [
                                                                                                        Expr {
                                                                                                            kind: Pointer(
                                                                                                                Sid(
                                                                                                                    2147484416,
                                                                                                                ),
                                                                                                            ),
                                                                                                        },
                                                                                                        Expr {
                                                                                                            kind: Pointer(
                                                                                                                Sid(
                                                                                                                    2147484417,
                                                                                                                ),
                                                                                                            ),
                                                                                                        },
                                                                                                    ],
                                                                                                ),
                                                                                            },
                                                                                        },
                                                                                    ),
                                                                                },
                                                                                args: [
                                                                                    Expr {
                                                                                        kind: Literal(
                                                                                            Int(
                                                                                                6,
                                                                                            ),
                                                                                        ),
                                                                                    },
                                                                                    Expr {
                                                                                        kind: Literal(
                                                                                            Int(
                                                                                                7,
                                                                                            ),
                                                                                        ),
                                                                                    },
                                                                                ],
                                                                            },
                                                                        ),
                                                                    },
                                                                    Expr {
                                                                        kind: Call(
                                                                            Call {
                                                                                function: Expr {
                                                                                    kind: Pointer(
                                                                                        Sid(
                                                                                            0,
                                                                                        ),
                                                                                    ),
                                                                                },
                                                                                args: [
                                                                                    Expr {
                                                                                        kind: Literal(
                                                                                            Int(
                                                                                                6,
                                                                                            ),
                                                                                        ),
                                                                                    },
                                                                                    Expr {
                                                                                        kind: Literal(
                                                                                            Int(
                                                                                                2,
                                                                                            ),
                                                                                        ),
                                                                                    },
                                                                                ],
                                                                            },
                                                                        ),
                                                                    },
                                                                ],
                                                            ),
                                                        },
                                                        offset: 1,
                                                    },
                                                ),
                                            },
                                            offset: 0,
                                        },
                                    ),
                                },
                            },
                        ),
                    },
                },
            ),
        },
    }
    "#);
}
