#![cfg(test)]
use insta::assert_debug_snapshot;

#[test]
fn parse_01() {
    assert_debug_snapshot!(super::_test_parse(r#"let externals = [
  std::int::add,
];

let main =
  let 1 = (
    func 2 -> [
        fn.2+0: float64,
        fn.2+0: float64,
        fn.2+0: float64
    ]: [float64]
  ): func (float64) -> [float64];

  let 2 = var.1: func (float64) -> [float64];
  {
    (
        call var.2: func (float64) -> [float64],
        3.5: float64
    ): [float64],
    (call
        (
            func 3 -> [
                fn.3+0: int64,
                fn.3+1: int64,
            ]: [int64]
        ): func (int64, int64) -> [int64],
        6: int64,
        7: int64,
    ): [int64],
    (call
        external.0: func (int64, int64) -> int64,
        6: int64,
        2: int64
    ): int64
  }: {[float64], [int64], int64}
  .1:[int64]
    "#), @r#"
    Program {
        externals: [
            ExternalSymbol {
                id: "std::int::add",
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
                                                        float64,
                                                    ),
                                                    layout: Some(
                                                        TyLayout {
                                                            head_size: 64,
                                                            body_ptrs: [],
                                                            variants_recursive: [],
                                                        },
                                                    ),
                                                    name: None,
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
                                                        float64,
                                                    ),
                                                    layout: Some(
                                                        TyLayout {
                                                            head_size: 64,
                                                            body_ptrs: [],
                                                            variants_recursive: [],
                                                        },
                                                    ),
                                                    name: None,
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
                                                        float64,
                                                    ),
                                                    layout: Some(
                                                        TyLayout {
                                                            head_size: 64,
                                                            body_ptrs: [],
                                                            variants_recursive: [],
                                                        },
                                                    ),
                                                    name: None,
                                                },
                                            },
                                        ],
                                    ),
                                    ty: Ty {
                                        kind: Array(
                                            Ty {
                                                kind: Primitive(
                                                    float64,
                                                ),
                                                layout: Some(
                                                    TyLayout {
                                                        head_size: 64,
                                                        body_ptrs: [],
                                                        variants_recursive: [],
                                                    },
                                                ),
                                                name: None,
                                            },
                                        ),
                                        layout: Some(
                                            TyLayout {
                                                head_size: 64,
                                                body_ptrs: [
                                                    0,
                                                ],
                                                variants_recursive: [],
                                            },
                                        ),
                                        name: None,
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
                                                float64,
                                            ),
                                            layout: Some(
                                                TyLayout {
                                                    head_size: 64,
                                                    body_ptrs: [],
                                                    variants_recursive: [],
                                                },
                                            ),
                                            name: None,
                                        },
                                    ],
                                    body: Ty {
                                        kind: Array(
                                            Ty {
                                                kind: Primitive(
                                                    float64,
                                                ),
                                                layout: Some(
                                                    TyLayout {
                                                        head_size: 64,
                                                        body_ptrs: [],
                                                        variants_recursive: [],
                                                    },
                                                ),
                                                name: None,
                                            },
                                        ),
                                        layout: Some(
                                            TyLayout {
                                                head_size: 64,
                                                body_ptrs: [
                                                    0,
                                                ],
                                                variants_recursive: [],
                                            },
                                        ),
                                        name: None,
                                    },
                                },
                            ),
                            layout: None,
                            name: None,
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
                                                            float64,
                                                        ),
                                                        layout: Some(
                                                            TyLayout {
                                                                head_size: 64,
                                                                body_ptrs: [],
                                                                variants_recursive: [],
                                                            },
                                                        ),
                                                        name: None,
                                                    },
                                                ],
                                                body: Ty {
                                                    kind: Array(
                                                        Ty {
                                                            kind: Primitive(
                                                                float64,
                                                            ),
                                                            layout: Some(
                                                                TyLayout {
                                                                    head_size: 64,
                                                                    body_ptrs: [],
                                                                    variants_recursive: [],
                                                                },
                                                            ),
                                                            name: None,
                                                        },
                                                    ),
                                                    layout: Some(
                                                        TyLayout {
                                                            head_size: 64,
                                                            body_ptrs: [
                                                                0,
                                                            ],
                                                            variants_recursive: [],
                                                        },
                                                    ),
                                                    name: None,
                                                },
                                            },
                                        ),
                                        layout: None,
                                        name: None,
                                    },
                                },
                                main: Expr {
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
                                                                                                float64,
                                                                                            ),
                                                                                            layout: Some(
                                                                                                TyLayout {
                                                                                                    head_size: 64,
                                                                                                    body_ptrs: [],
                                                                                                    variants_recursive: [],
                                                                                                },
                                                                                            ),
                                                                                            name: None,
                                                                                        },
                                                                                    ],
                                                                                    body: Ty {
                                                                                        kind: Array(
                                                                                            Ty {
                                                                                                kind: Primitive(
                                                                                                    float64,
                                                                                                ),
                                                                                                layout: Some(
                                                                                                    TyLayout {
                                                                                                        head_size: 64,
                                                                                                        body_ptrs: [],
                                                                                                        variants_recursive: [],
                                                                                                    },
                                                                                                ),
                                                                                                name: None,
                                                                                            },
                                                                                        ),
                                                                                        layout: Some(
                                                                                            TyLayout {
                                                                                                head_size: 64,
                                                                                                body_ptrs: [
                                                                                                    0,
                                                                                                ],
                                                                                                variants_recursive: [],
                                                                                            },
                                                                                        ),
                                                                                        name: None,
                                                                                    },
                                                                                },
                                                                            ),
                                                                            layout: None,
                                                                            name: None,
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
                                                                                    float64,
                                                                                ),
                                                                                layout: Some(
                                                                                    TyLayout {
                                                                                        head_size: 64,
                                                                                        body_ptrs: [],
                                                                                        variants_recursive: [],
                                                                                    },
                                                                                ),
                                                                                name: None,
                                                                            },
                                                                        },
                                                                    ],
                                                                },
                                                            ),
                                                            ty: Ty {
                                                                kind: Array(
                                                                    Ty {
                                                                        kind: Primitive(
                                                                            float64,
                                                                        ),
                                                                        layout: Some(
                                                                            TyLayout {
                                                                                head_size: 64,
                                                                                body_ptrs: [],
                                                                                variants_recursive: [],
                                                                            },
                                                                        ),
                                                                        name: None,
                                                                    },
                                                                ),
                                                                layout: Some(
                                                                    TyLayout {
                                                                        head_size: 64,
                                                                        body_ptrs: [
                                                                            0,
                                                                        ],
                                                                        variants_recursive: [],
                                                                    },
                                                                ),
                                                                name: None,
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
                                                                                                        int64,
                                                                                                    ),
                                                                                                    layout: Some(
                                                                                                        TyLayout {
                                                                                                            head_size: 64,
                                                                                                            body_ptrs: [],
                                                                                                            variants_recursive: [],
                                                                                                        },
                                                                                                    ),
                                                                                                    name: None,
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
                                                                                                        int64,
                                                                                                    ),
                                                                                                    layout: Some(
                                                                                                        TyLayout {
                                                                                                            head_size: 64,
                                                                                                            body_ptrs: [],
                                                                                                            variants_recursive: [],
                                                                                                        },
                                                                                                    ),
                                                                                                    name: None,
                                                                                                },
                                                                                            },
                                                                                        ],
                                                                                    ),
                                                                                    ty: Ty {
                                                                                        kind: Array(
                                                                                            Ty {
                                                                                                kind: Primitive(
                                                                                                    int64,
                                                                                                ),
                                                                                                layout: Some(
                                                                                                    TyLayout {
                                                                                                        head_size: 64,
                                                                                                        body_ptrs: [],
                                                                                                        variants_recursive: [],
                                                                                                    },
                                                                                                ),
                                                                                                name: None,
                                                                                            },
                                                                                        ),
                                                                                        layout: Some(
                                                                                            TyLayout {
                                                                                                head_size: 64,
                                                                                                body_ptrs: [
                                                                                                    0,
                                                                                                ],
                                                                                                variants_recursive: [],
                                                                                            },
                                                                                        ),
                                                                                        name: None,
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
                                                                                                int64,
                                                                                            ),
                                                                                            layout: Some(
                                                                                                TyLayout {
                                                                                                    head_size: 64,
                                                                                                    body_ptrs: [],
                                                                                                    variants_recursive: [],
                                                                                                },
                                                                                            ),
                                                                                            name: None,
                                                                                        },
                                                                                        Ty {
                                                                                            kind: Primitive(
                                                                                                int64,
                                                                                            ),
                                                                                            layout: Some(
                                                                                                TyLayout {
                                                                                                    head_size: 64,
                                                                                                    body_ptrs: [],
                                                                                                    variants_recursive: [],
                                                                                                },
                                                                                            ),
                                                                                            name: None,
                                                                                        },
                                                                                    ],
                                                                                    body: Ty {
                                                                                        kind: Array(
                                                                                            Ty {
                                                                                                kind: Primitive(
                                                                                                    int64,
                                                                                                ),
                                                                                                layout: Some(
                                                                                                    TyLayout {
                                                                                                        head_size: 64,
                                                                                                        body_ptrs: [],
                                                                                                        variants_recursive: [],
                                                                                                    },
                                                                                                ),
                                                                                                name: None,
                                                                                            },
                                                                                        ),
                                                                                        layout: Some(
                                                                                            TyLayout {
                                                                                                head_size: 64,
                                                                                                body_ptrs: [
                                                                                                    0,
                                                                                                ],
                                                                                                variants_recursive: [],
                                                                                            },
                                                                                        ),
                                                                                        name: None,
                                                                                    },
                                                                                },
                                                                            ),
                                                                            layout: None,
                                                                            name: None,
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
                                                                                    int64,
                                                                                ),
                                                                                layout: Some(
                                                                                    TyLayout {
                                                                                        head_size: 64,
                                                                                        body_ptrs: [],
                                                                                        variants_recursive: [],
                                                                                    },
                                                                                ),
                                                                                name: None,
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
                                                                                    int64,
                                                                                ),
                                                                                layout: Some(
                                                                                    TyLayout {
                                                                                        head_size: 64,
                                                                                        body_ptrs: [],
                                                                                        variants_recursive: [],
                                                                                    },
                                                                                ),
                                                                                name: None,
                                                                            },
                                                                        },
                                                                    ],
                                                                },
                                                            ),
                                                            ty: Ty {
                                                                kind: Array(
                                                                    Ty {
                                                                        kind: Primitive(
                                                                            int64,
                                                                        ),
                                                                        layout: Some(
                                                                            TyLayout {
                                                                                head_size: 64,
                                                                                body_ptrs: [],
                                                                                variants_recursive: [],
                                                                            },
                                                                        ),
                                                                        name: None,
                                                                    },
                                                                ),
                                                                layout: Some(
                                                                    TyLayout {
                                                                        head_size: 64,
                                                                        body_ptrs: [
                                                                            0,
                                                                        ],
                                                                        variants_recursive: [],
                                                                    },
                                                                ),
                                                                name: None,
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
                                                                                                int64,
                                                                                            ),
                                                                                            layout: Some(
                                                                                                TyLayout {
                                                                                                    head_size: 64,
                                                                                                    body_ptrs: [],
                                                                                                    variants_recursive: [],
                                                                                                },
                                                                                            ),
                                                                                            name: None,
                                                                                        },
                                                                                        Ty {
                                                                                            kind: Primitive(
                                                                                                int64,
                                                                                            ),
                                                                                            layout: Some(
                                                                                                TyLayout {
                                                                                                    head_size: 64,
                                                                                                    body_ptrs: [],
                                                                                                    variants_recursive: [],
                                                                                                },
                                                                                            ),
                                                                                            name: None,
                                                                                        },
                                                                                    ],
                                                                                    body: Ty {
                                                                                        kind: Primitive(
                                                                                            int64,
                                                                                        ),
                                                                                        layout: Some(
                                                                                            TyLayout {
                                                                                                head_size: 64,
                                                                                                body_ptrs: [],
                                                                                                variants_recursive: [],
                                                                                            },
                                                                                        ),
                                                                                        name: None,
                                                                                    },
                                                                                },
                                                                            ),
                                                                            layout: None,
                                                                            name: None,
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
                                                                                    int64,
                                                                                ),
                                                                                layout: Some(
                                                                                    TyLayout {
                                                                                        head_size: 64,
                                                                                        body_ptrs: [],
                                                                                        variants_recursive: [],
                                                                                    },
                                                                                ),
                                                                                name: None,
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
                                                                                    int64,
                                                                                ),
                                                                                layout: Some(
                                                                                    TyLayout {
                                                                                        head_size: 64,
                                                                                        body_ptrs: [],
                                                                                        variants_recursive: [],
                                                                                    },
                                                                                ),
                                                                                name: None,
                                                                            },
                                                                        },
                                                                    ],
                                                                },
                                                            ),
                                                            ty: Ty {
                                                                kind: Primitive(
                                                                    int64,
                                                                ),
                                                                layout: Some(
                                                                    TyLayout {
                                                                        head_size: 64,
                                                                        body_ptrs: [],
                                                                        variants_recursive: [],
                                                                    },
                                                                ),
                                                                name: None,
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
                                                                                float64,
                                                                            ),
                                                                            layout: Some(
                                                                                TyLayout {
                                                                                    head_size: 64,
                                                                                    body_ptrs: [],
                                                                                    variants_recursive: [],
                                                                                },
                                                                            ),
                                                                            name: None,
                                                                        },
                                                                    ),
                                                                    layout: Some(
                                                                        TyLayout {
                                                                            head_size: 64,
                                                                            body_ptrs: [
                                                                                0,
                                                                            ],
                                                                            variants_recursive: [],
                                                                        },
                                                                    ),
                                                                    name: None,
                                                                },
                                                            },
                                                            TyTupleField {
                                                                name: None,
                                                                ty: Ty {
                                                                    kind: Array(
                                                                        Ty {
                                                                            kind: Primitive(
                                                                                int64,
                                                                            ),
                                                                            layout: Some(
                                                                                TyLayout {
                                                                                    head_size: 64,
                                                                                    body_ptrs: [],
                                                                                    variants_recursive: [],
                                                                                },
                                                                            ),
                                                                            name: None,
                                                                        },
                                                                    ),
                                                                    layout: Some(
                                                                        TyLayout {
                                                                            head_size: 64,
                                                                            body_ptrs: [
                                                                                0,
                                                                            ],
                                                                            variants_recursive: [],
                                                                        },
                                                                    ),
                                                                    name: None,
                                                                },
                                                            },
                                                            TyTupleField {
                                                                name: None,
                                                                ty: Ty {
                                                                    kind: Primitive(
                                                                        int64,
                                                                    ),
                                                                    layout: Some(
                                                                        TyLayout {
                                                                            head_size: 64,
                                                                            body_ptrs: [],
                                                                            variants_recursive: [],
                                                                        },
                                                                    ),
                                                                    name: None,
                                                                },
                                                            },
                                                        ],
                                                    ),
                                                    layout: Some(
                                                        TyLayout {
                                                            head_size: 192,
                                                            body_ptrs: [
                                                                0,
                                                                0,
                                                            ],
                                                            variants_recursive: [],
                                                        },
                                                    ),
                                                    name: None,
                                                },
                                            },
                                            position: 1,
                                        },
                                    ),
                                    ty: Ty {
                                        kind: Array(
                                            Ty {
                                                kind: Primitive(
                                                    int64,
                                                ),
                                                layout: Some(
                                                    TyLayout {
                                                        head_size: 64,
                                                        body_ptrs: [],
                                                        variants_recursive: [],
                                                    },
                                                ),
                                                name: None,
                                            },
                                        ),
                                        layout: Some(
                                            TyLayout {
                                                head_size: 64,
                                                body_ptrs: [
                                                    0,
                                                ],
                                                variants_recursive: [],
                                            },
                                        ),
                                        name: None,
                                    },
                                },
                            },
                        ),
                        ty: Ty {
                            kind: Array(
                                Ty {
                                    kind: Primitive(
                                        int64,
                                    ),
                                    layout: Some(
                                        TyLayout {
                                            head_size: 64,
                                            body_ptrs: [],
                                            variants_recursive: [],
                                        },
                                    ),
                                    name: None,
                                },
                            ),
                            layout: Some(
                                TyLayout {
                                    head_size: 64,
                                    body_ptrs: [
                                        0,
                                    ],
                                    variants_recursive: [],
                                },
                            ),
                            name: None,
                        },
                    },
                },
            ),
            ty: Ty {
                kind: Array(
                    Ty {
                        kind: Primitive(
                            int64,
                        ),
                        layout: Some(
                            TyLayout {
                                head_size: 64,
                                body_ptrs: [],
                                variants_recursive: [],
                            },
                        ),
                        name: None,
                    },
                ),
                layout: Some(
                    TyLayout {
                        head_size: 64,
                        body_ptrs: [
                            0,
                        ],
                        variants_recursive: [],
                    },
                ),
                name: None,
            },
        },
    }
    "#);
}
