#![cfg(test)]
use insta::assert_debug_snapshot;

#[test]
fn parse_01() {
    assert_debug_snapshot!(super::_test_parse(r#"let externals = [
  std_int_add,
];

let main =
  let 1 = (
    func 2 -> [
        fn.2+0: float,
        fn.2+0: float,
        fn.2+0: float
    ]: [float]
  ): func (float) -> [float];

  let 2 = var.1: func (float) -> [float];
  {
    (
        call var.2: func (float) -> [float],
        3.5: float
    ): [float],
    (call
        (
            func 3 -> [
                fn.3+0: int,
                fn.3+1: int,
            ]: [int]
        ): func (int, int) -> [int],
        6: int,
        7: int,
    ): [int],
    (call
        external.0: func (int, int) -> int,
        6: int,
        2: int
    ): int
  }: {[float], [int], int}
  .1:[int]
  .[0]:int
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
                                                ty: Ty {
                                                    kind: Primitive(
                                                        float,
                                                    ),
                                                    layout: TyLayout {
                                                        head_size: 0,
                                                        variants_recursive: [],
                                                    },
                                                },
                                            },
                                            Expr {
                                                kind: Pointer(
                                                    Sid(
                                                        2147484160,
                                                    ),
                                                ),
                                                ty: Ty {
                                                    kind: Primitive(
                                                        float,
                                                    ),
                                                    layout: TyLayout {
                                                        head_size: 0,
                                                        variants_recursive: [],
                                                    },
                                                },
                                            },
                                            Expr {
                                                kind: Pointer(
                                                    Sid(
                                                        2147484160,
                                                    ),
                                                ),
                                                ty: Ty {
                                                    kind: Primitive(
                                                        float,
                                                    ),
                                                    layout: TyLayout {
                                                        head_size: 0,
                                                        variants_recursive: [],
                                                    },
                                                },
                                            },
                                        ],
                                    ),
                                    ty: Ty {
                                        kind: Array(
                                            Ty {
                                                kind: Primitive(
                                                    float,
                                                ),
                                                layout: TyLayout {
                                                    head_size: 0,
                                                    variants_recursive: [],
                                                },
                                            },
                                        ),
                                        layout: TyLayout {
                                            head_size: 0,
                                            variants_recursive: [],
                                        },
                                    },
                                },
                            },
                        ),
                        ty: Ty {
                            kind: Function(
                                TyFunction {
                                    params: [
                                        Ty {
                                            kind: Primitive(
                                                float,
                                            ),
                                            layout: TyLayout {
                                                head_size: 0,
                                                variants_recursive: [],
                                            },
                                        },
                                    ],
                                    body: Ty {
                                        kind: Array(
                                            Ty {
                                                kind: Primitive(
                                                    float,
                                                ),
                                                layout: TyLayout {
                                                    head_size: 0,
                                                    variants_recursive: [],
                                                },
                                            },
                                        ),
                                        layout: TyLayout {
                                            head_size: 0,
                                            variants_recursive: [],
                                        },
                                    },
                                },
                            ),
                            layout: TyLayout {
                                head_size: 0,
                                variants_recursive: [],
                            },
                        },
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
                                    ty: Ty {
                                        kind: Function(
                                            TyFunction {
                                                params: [
                                                    Ty {
                                                        kind: Primitive(
                                                            float,
                                                        ),
                                                        layout: TyLayout {
                                                            head_size: 0,
                                                            variants_recursive: [],
                                                        },
                                                    },
                                                ],
                                                body: Ty {
                                                    kind: Array(
                                                        Ty {
                                                            kind: Primitive(
                                                                float,
                                                            ),
                                                            layout: TyLayout {
                                                                head_size: 0,
                                                                variants_recursive: [],
                                                            },
                                                        },
                                                    ),
                                                    layout: TyLayout {
                                                        head_size: 0,
                                                        variants_recursive: [],
                                                    },
                                                },
                                            },
                                        ),
                                        layout: TyLayout {
                                            head_size: 0,
                                            variants_recursive: [],
                                        },
                                    },
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
                                                                                    ty: Ty {
                                                                                        kind: Function(
                                                                                            TyFunction {
                                                                                                params: [
                                                                                                    Ty {
                                                                                                        kind: Primitive(
                                                                                                            float,
                                                                                                        ),
                                                                                                        layout: TyLayout {
                                                                                                            head_size: 0,
                                                                                                            variants_recursive: [],
                                                                                                        },
                                                                                                    },
                                                                                                ],
                                                                                                body: Ty {
                                                                                                    kind: Array(
                                                                                                        Ty {
                                                                                                            kind: Primitive(
                                                                                                                float,
                                                                                                            ),
                                                                                                            layout: TyLayout {
                                                                                                                head_size: 0,
                                                                                                                variants_recursive: [],
                                                                                                            },
                                                                                                        },
                                                                                                    ),
                                                                                                    layout: TyLayout {
                                                                                                        head_size: 0,
                                                                                                        variants_recursive: [],
                                                                                                    },
                                                                                                },
                                                                                            },
                                                                                        ),
                                                                                        layout: TyLayout {
                                                                                            head_size: 0,
                                                                                            variants_recursive: [],
                                                                                        },
                                                                                    },
                                                                                },
                                                                                args: [
                                                                                    Expr {
                                                                                        kind: Literal(
                                                                                            Float(
                                                                                                3.5,
                                                                                            ),
                                                                                        ),
                                                                                        ty: Ty {
                                                                                            kind: Primitive(
                                                                                                float,
                                                                                            ),
                                                                                            layout: TyLayout {
                                                                                                head_size: 0,
                                                                                                variants_recursive: [],
                                                                                            },
                                                                                        },
                                                                                    },
                                                                                ],
                                                                            },
                                                                        ),
                                                                        ty: Ty {
                                                                            kind: Array(
                                                                                Ty {
                                                                                    kind: Primitive(
                                                                                        float,
                                                                                    ),
                                                                                    layout: TyLayout {
                                                                                        head_size: 0,
                                                                                        variants_recursive: [],
                                                                                    },
                                                                                },
                                                                            ),
                                                                            layout: TyLayout {
                                                                                head_size: 0,
                                                                                variants_recursive: [],
                                                                            },
                                                                        },
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
                                                                                                            ty: Ty {
                                                                                                                kind: Primitive(
                                                                                                                    int,
                                                                                                                ),
                                                                                                                layout: TyLayout {
                                                                                                                    head_size: 0,
                                                                                                                    variants_recursive: [],
                                                                                                                },
                                                                                                            },
                                                                                                        },
                                                                                                        Expr {
                                                                                                            kind: Pointer(
                                                                                                                Sid(
                                                                                                                    2147484417,
                                                                                                                ),
                                                                                                            ),
                                                                                                            ty: Ty {
                                                                                                                kind: Primitive(
                                                                                                                    int,
                                                                                                                ),
                                                                                                                layout: TyLayout {
                                                                                                                    head_size: 0,
                                                                                                                    variants_recursive: [],
                                                                                                                },
                                                                                                            },
                                                                                                        },
                                                                                                    ],
                                                                                                ),
                                                                                                ty: Ty {
                                                                                                    kind: Array(
                                                                                                        Ty {
                                                                                                            kind: Primitive(
                                                                                                                int,
                                                                                                            ),
                                                                                                            layout: TyLayout {
                                                                                                                head_size: 0,
                                                                                                                variants_recursive: [],
                                                                                                            },
                                                                                                        },
                                                                                                    ),
                                                                                                    layout: TyLayout {
                                                                                                        head_size: 0,
                                                                                                        variants_recursive: [],
                                                                                                    },
                                                                                                },
                                                                                            },
                                                                                        },
                                                                                    ),
                                                                                    ty: Ty {
                                                                                        kind: Function(
                                                                                            TyFunction {
                                                                                                params: [
                                                                                                    Ty {
                                                                                                        kind: Primitive(
                                                                                                            int,
                                                                                                        ),
                                                                                                        layout: TyLayout {
                                                                                                            head_size: 0,
                                                                                                            variants_recursive: [],
                                                                                                        },
                                                                                                    },
                                                                                                    Ty {
                                                                                                        kind: Primitive(
                                                                                                            int,
                                                                                                        ),
                                                                                                        layout: TyLayout {
                                                                                                            head_size: 0,
                                                                                                            variants_recursive: [],
                                                                                                        },
                                                                                                    },
                                                                                                ],
                                                                                                body: Ty {
                                                                                                    kind: Array(
                                                                                                        Ty {
                                                                                                            kind: Primitive(
                                                                                                                int,
                                                                                                            ),
                                                                                                            layout: TyLayout {
                                                                                                                head_size: 0,
                                                                                                                variants_recursive: [],
                                                                                                            },
                                                                                                        },
                                                                                                    ),
                                                                                                    layout: TyLayout {
                                                                                                        head_size: 0,
                                                                                                        variants_recursive: [],
                                                                                                    },
                                                                                                },
                                                                                            },
                                                                                        ),
                                                                                        layout: TyLayout {
                                                                                            head_size: 0,
                                                                                            variants_recursive: [],
                                                                                        },
                                                                                    },
                                                                                },
                                                                                args: [
                                                                                    Expr {
                                                                                        kind: Literal(
                                                                                            Int(
                                                                                                6,
                                                                                            ),
                                                                                        ),
                                                                                        ty: Ty {
                                                                                            kind: Primitive(
                                                                                                int,
                                                                                            ),
                                                                                            layout: TyLayout {
                                                                                                head_size: 0,
                                                                                                variants_recursive: [],
                                                                                            },
                                                                                        },
                                                                                    },
                                                                                    Expr {
                                                                                        kind: Literal(
                                                                                            Int(
                                                                                                7,
                                                                                            ),
                                                                                        ),
                                                                                        ty: Ty {
                                                                                            kind: Primitive(
                                                                                                int,
                                                                                            ),
                                                                                            layout: TyLayout {
                                                                                                head_size: 0,
                                                                                                variants_recursive: [],
                                                                                            },
                                                                                        },
                                                                                    },
                                                                                ],
                                                                            },
                                                                        ),
                                                                        ty: Ty {
                                                                            kind: Array(
                                                                                Ty {
                                                                                    kind: Primitive(
                                                                                        int,
                                                                                    ),
                                                                                    layout: TyLayout {
                                                                                        head_size: 0,
                                                                                        variants_recursive: [],
                                                                                    },
                                                                                },
                                                                            ),
                                                                            layout: TyLayout {
                                                                                head_size: 0,
                                                                                variants_recursive: [],
                                                                            },
                                                                        },
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
                                                                                    ty: Ty {
                                                                                        kind: Function(
                                                                                            TyFunction {
                                                                                                params: [
                                                                                                    Ty {
                                                                                                        kind: Primitive(
                                                                                                            int,
                                                                                                        ),
                                                                                                        layout: TyLayout {
                                                                                                            head_size: 0,
                                                                                                            variants_recursive: [],
                                                                                                        },
                                                                                                    },
                                                                                                    Ty {
                                                                                                        kind: Primitive(
                                                                                                            int,
                                                                                                        ),
                                                                                                        layout: TyLayout {
                                                                                                            head_size: 0,
                                                                                                            variants_recursive: [],
                                                                                                        },
                                                                                                    },
                                                                                                ],
                                                                                                body: Ty {
                                                                                                    kind: Primitive(
                                                                                                        int,
                                                                                                    ),
                                                                                                    layout: TyLayout {
                                                                                                        head_size: 0,
                                                                                                        variants_recursive: [],
                                                                                                    },
                                                                                                },
                                                                                            },
                                                                                        ),
                                                                                        layout: TyLayout {
                                                                                            head_size: 0,
                                                                                            variants_recursive: [],
                                                                                        },
                                                                                    },
                                                                                },
                                                                                args: [
                                                                                    Expr {
                                                                                        kind: Literal(
                                                                                            Int(
                                                                                                6,
                                                                                            ),
                                                                                        ),
                                                                                        ty: Ty {
                                                                                            kind: Primitive(
                                                                                                int,
                                                                                            ),
                                                                                            layout: TyLayout {
                                                                                                head_size: 0,
                                                                                                variants_recursive: [],
                                                                                            },
                                                                                        },
                                                                                    },
                                                                                    Expr {
                                                                                        kind: Literal(
                                                                                            Int(
                                                                                                2,
                                                                                            ),
                                                                                        ),
                                                                                        ty: Ty {
                                                                                            kind: Primitive(
                                                                                                int,
                                                                                            ),
                                                                                            layout: TyLayout {
                                                                                                head_size: 0,
                                                                                                variants_recursive: [],
                                                                                            },
                                                                                        },
                                                                                    },
                                                                                ],
                                                                            },
                                                                        ),
                                                                        ty: Ty {
                                                                            kind: Primitive(
                                                                                int,
                                                                            ),
                                                                            layout: TyLayout {
                                                                                head_size: 0,
                                                                                variants_recursive: [],
                                                                            },
                                                                        },
                                                                    },
                                                                ],
                                                            ),
                                                            ty: Ty {
                                                                kind: Tuple(
                                                                    [
                                                                        TyTupleField {
                                                                            name: None,
                                                                            ty: Ty {
                                                                                kind: Array(
                                                                                    Ty {
                                                                                        kind: Primitive(
                                                                                            float,
                                                                                        ),
                                                                                        layout: TyLayout {
                                                                                            head_size: 0,
                                                                                            variants_recursive: [],
                                                                                        },
                                                                                    },
                                                                                ),
                                                                                layout: TyLayout {
                                                                                    head_size: 0,
                                                                                    variants_recursive: [],
                                                                                },
                                                                            },
                                                                        },
                                                                        TyTupleField {
                                                                            name: None,
                                                                            ty: Ty {
                                                                                kind: Array(
                                                                                    Ty {
                                                                                        kind: Primitive(
                                                                                            int,
                                                                                        ),
                                                                                        layout: TyLayout {
                                                                                            head_size: 0,
                                                                                            variants_recursive: [],
                                                                                        },
                                                                                    },
                                                                                ),
                                                                                layout: TyLayout {
                                                                                    head_size: 0,
                                                                                    variants_recursive: [],
                                                                                },
                                                                            },
                                                                        },
                                                                        TyTupleField {
                                                                            name: None,
                                                                            ty: Ty {
                                                                                kind: Primitive(
                                                                                    int,
                                                                                ),
                                                                                layout: TyLayout {
                                                                                    head_size: 0,
                                                                                    variants_recursive: [],
                                                                                },
                                                                            },
                                                                        },
                                                                    ],
                                                                ),
                                                                layout: TyLayout {
                                                                    head_size: 0,
                                                                    variants_recursive: [],
                                                                },
                                                            },
                                                        },
                                                        offset: 1,
                                                    },
                                                ),
                                                ty: Ty {
                                                    kind: Array(
                                                        Ty {
                                                            kind: Primitive(
                                                                int,
                                                            ),
                                                            layout: TyLayout {
                                                                head_size: 0,
                                                                variants_recursive: [],
                                                            },
                                                        },
                                                    ),
                                                    layout: TyLayout {
                                                        head_size: 0,
                                                        variants_recursive: [],
                                                    },
                                                },
                                            },
                                            offset: 0,
                                        },
                                    ),
                                    ty: Ty {
                                        kind: Primitive(
                                            int,
                                        ),
                                        layout: TyLayout {
                                            head_size: 0,
                                            variants_recursive: [],
                                        },
                                    },
                                },
                            },
                        ),
                        ty: Ty {
                            kind: Primitive(
                                int,
                            ),
                            layout: TyLayout {
                                head_size: 0,
                                variants_recursive: [],
                            },
                        },
                    },
                },
            ),
            ty: Ty {
                kind: Primitive(
                    int,
                ),
                layout: TyLayout {
                    head_size: 0,
                    variants_recursive: [],
                },
            },
        },
    }
    "#);
}
