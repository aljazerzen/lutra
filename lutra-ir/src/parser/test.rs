#![cfg(test)]
use insta::assert_debug_snapshot;

#[test]
fn parse_01() {
    assert_debug_snapshot!(super::_test_parse(r#"type a = int64;
type b::c = text;
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
        external.std::int::add: func (int64, int64) -> int64,
        6: int64,
        2: int64
    ): int64
  }: {[float64], [int64], int64}
  .1:[int64]
    "#), @r#"
    Program {
        main: Expr {
            kind: Binding(
                Binding {
                    id: 1,
                    expr: Expr {
                        kind: Function(
                            Function {
                                id: 2,
                                body: Expr {
                                    kind: Array(
                                        [
                                            Expr {
                                                kind: Pointer(
                                                    Parameter(
                                                        ParameterPtr {
                                                            function_id: 2,
                                                            param_position: 0,
                                                        },
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
                                                        },
                                                    ),
                                                    name: None,
                                                    variants_recursive: [],
                                                },
                                            },
                                            Expr {
                                                kind: Pointer(
                                                    Parameter(
                                                        ParameterPtr {
                                                            function_id: 2,
                                                            param_position: 0,
                                                        },
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
                                                        },
                                                    ),
                                                    name: None,
                                                    variants_recursive: [],
                                                },
                                            },
                                            Expr {
                                                kind: Pointer(
                                                    Parameter(
                                                        ParameterPtr {
                                                            function_id: 2,
                                                            param_position: 0,
                                                        },
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
                                                        },
                                                    ),
                                                    name: None,
                                                    variants_recursive: [],
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
                                                    },
                                                ),
                                                name: None,
                                                variants_recursive: [],
                                            },
                                        ),
                                        layout: Some(
                                            TyLayout {
                                                head_size: 64,
                                                body_ptrs: [
                                                    0,
                                                ],
                                            },
                                        ),
                                        name: None,
                                        variants_recursive: [],
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
                                                },
                                            ),
                                            name: None,
                                            variants_recursive: [],
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
                                                    },
                                                ),
                                                name: None,
                                                variants_recursive: [],
                                            },
                                        ),
                                        layout: Some(
                                            TyLayout {
                                                head_size: 64,
                                                body_ptrs: [
                                                    0,
                                                ],
                                            },
                                        ),
                                        name: None,
                                        variants_recursive: [],
                                    },
                                },
                            ),
                            layout: None,
                            name: None,
                            variants_recursive: [],
                        },
                    },
                    main: Expr {
                        kind: Binding(
                            Binding {
                                id: 2,
                                expr: Expr {
                                    kind: Pointer(
                                        Binding(
                                            1,
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
                                                            },
                                                        ),
                                                        name: None,
                                                        variants_recursive: [],
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
                                                                },
                                                            ),
                                                            name: None,
                                                            variants_recursive: [],
                                                        },
                                                    ),
                                                    layout: Some(
                                                        TyLayout {
                                                            head_size: 64,
                                                            body_ptrs: [
                                                                0,
                                                            ],
                                                        },
                                                    ),
                                                    name: None,
                                                    variants_recursive: [],
                                                },
                                            },
                                        ),
                                        layout: None,
                                        name: None,
                                        variants_recursive: [],
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
                                                                            Binding(
                                                                                2,
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
                                                                                                },
                                                                                            ),
                                                                                            name: None,
                                                                                            variants_recursive: [],
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
                                                                                                    },
                                                                                                ),
                                                                                                name: None,
                                                                                                variants_recursive: [],
                                                                                            },
                                                                                        ),
                                                                                        layout: Some(
                                                                                            TyLayout {
                                                                                                head_size: 64,
                                                                                                body_ptrs: [
                                                                                                    0,
                                                                                                ],
                                                                                            },
                                                                                        ),
                                                                                        name: None,
                                                                                        variants_recursive: [],
                                                                                    },
                                                                                },
                                                                            ),
                                                                            layout: None,
                                                                            name: None,
                                                                            variants_recursive: [],
                                                                        },
                                                                    },
                                                                    args: [
                                                                        Expr {
                                                                            kind: Literal(
                                                                                float64(
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
                                                                                    },
                                                                                ),
                                                                                name: None,
                                                                                variants_recursive: [],
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
                                                                            },
                                                                        ),
                                                                        name: None,
                                                                        variants_recursive: [],
                                                                    },
                                                                ),
                                                                layout: Some(
                                                                    TyLayout {
                                                                        head_size: 64,
                                                                        body_ptrs: [
                                                                            0,
                                                                        ],
                                                                    },
                                                                ),
                                                                name: None,
                                                                variants_recursive: [],
                                                            },
                                                        },
                                                        Expr {
                                                            kind: Call(
                                                                Call {
                                                                    function: Expr {
                                                                        kind: Function(
                                                                            Function {
                                                                                id: 3,
                                                                                body: Expr {
                                                                                    kind: Array(
                                                                                        [
                                                                                            Expr {
                                                                                                kind: Pointer(
                                                                                                    Parameter(
                                                                                                        ParameterPtr {
                                                                                                            function_id: 3,
                                                                                                            param_position: 0,
                                                                                                        },
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
                                                                                                        },
                                                                                                    ),
                                                                                                    name: None,
                                                                                                    variants_recursive: [],
                                                                                                },
                                                                                            },
                                                                                            Expr {
                                                                                                kind: Pointer(
                                                                                                    Parameter(
                                                                                                        ParameterPtr {
                                                                                                            function_id: 3,
                                                                                                            param_position: 1,
                                                                                                        },
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
                                                                                                        },
                                                                                                    ),
                                                                                                    name: None,
                                                                                                    variants_recursive: [],
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
                                                                                                    },
                                                                                                ),
                                                                                                name: None,
                                                                                                variants_recursive: [],
                                                                                            },
                                                                                        ),
                                                                                        layout: Some(
                                                                                            TyLayout {
                                                                                                head_size: 64,
                                                                                                body_ptrs: [
                                                                                                    0,
                                                                                                ],
                                                                                            },
                                                                                        ),
                                                                                        name: None,
                                                                                        variants_recursive: [],
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
                                                                                                },
                                                                                            ),
                                                                                            name: None,
                                                                                            variants_recursive: [],
                                                                                        },
                                                                                        Ty {
                                                                                            kind: Primitive(
                                                                                                int64,
                                                                                            ),
                                                                                            layout: Some(
                                                                                                TyLayout {
                                                                                                    head_size: 64,
                                                                                                    body_ptrs: [],
                                                                                                },
                                                                                            ),
                                                                                            name: None,
                                                                                            variants_recursive: [],
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
                                                                                                    },
                                                                                                ),
                                                                                                name: None,
                                                                                                variants_recursive: [],
                                                                                            },
                                                                                        ),
                                                                                        layout: Some(
                                                                                            TyLayout {
                                                                                                head_size: 64,
                                                                                                body_ptrs: [
                                                                                                    0,
                                                                                                ],
                                                                                            },
                                                                                        ),
                                                                                        name: None,
                                                                                        variants_recursive: [],
                                                                                    },
                                                                                },
                                                                            ),
                                                                            layout: None,
                                                                            name: None,
                                                                            variants_recursive: [],
                                                                        },
                                                                    },
                                                                    args: [
                                                                        Expr {
                                                                            kind: Literal(
                                                                                int64(
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
                                                                                    },
                                                                                ),
                                                                                name: None,
                                                                                variants_recursive: [],
                                                                            },
                                                                        },
                                                                        Expr {
                                                                            kind: Literal(
                                                                                int64(
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
                                                                                    },
                                                                                ),
                                                                                name: None,
                                                                                variants_recursive: [],
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
                                                                            },
                                                                        ),
                                                                        name: None,
                                                                        variants_recursive: [],
                                                                    },
                                                                ),
                                                                layout: Some(
                                                                    TyLayout {
                                                                        head_size: 64,
                                                                        body_ptrs: [
                                                                            0,
                                                                        ],
                                                                    },
                                                                ),
                                                                name: None,
                                                                variants_recursive: [],
                                                            },
                                                        },
                                                        Expr {
                                                            kind: Call(
                                                                Call {
                                                                    function: Expr {
                                                                        kind: Pointer(
                                                                            External(
                                                                                ExternalPtr {
                                                                                    id: "std::int::add",
                                                                                },
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
                                                                                                },
                                                                                            ),
                                                                                            name: None,
                                                                                            variants_recursive: [],
                                                                                        },
                                                                                        Ty {
                                                                                            kind: Primitive(
                                                                                                int64,
                                                                                            ),
                                                                                            layout: Some(
                                                                                                TyLayout {
                                                                                                    head_size: 64,
                                                                                                    body_ptrs: [],
                                                                                                },
                                                                                            ),
                                                                                            name: None,
                                                                                            variants_recursive: [],
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
                                                                                            },
                                                                                        ),
                                                                                        name: None,
                                                                                        variants_recursive: [],
                                                                                    },
                                                                                },
                                                                            ),
                                                                            layout: None,
                                                                            name: None,
                                                                            variants_recursive: [],
                                                                        },
                                                                    },
                                                                    args: [
                                                                        Expr {
                                                                            kind: Literal(
                                                                                int64(
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
                                                                                    },
                                                                                ),
                                                                                name: None,
                                                                                variants_recursive: [],
                                                                            },
                                                                        },
                                                                        Expr {
                                                                            kind: Literal(
                                                                                int64(
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
                                                                                    },
                                                                                ),
                                                                                name: None,
                                                                                variants_recursive: [],
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
                                                                    },
                                                                ),
                                                                name: None,
                                                                variants_recursive: [],
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
                                                                                },
                                                                            ),
                                                                            name: None,
                                                                            variants_recursive: [],
                                                                        },
                                                                    ),
                                                                    layout: Some(
                                                                        TyLayout {
                                                                            head_size: 64,
                                                                            body_ptrs: [
                                                                                0,
                                                                            ],
                                                                        },
                                                                    ),
                                                                    name: None,
                                                                    variants_recursive: [],
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
                                                                                },
                                                                            ),
                                                                            name: None,
                                                                            variants_recursive: [],
                                                                        },
                                                                    ),
                                                                    layout: Some(
                                                                        TyLayout {
                                                                            head_size: 64,
                                                                            body_ptrs: [
                                                                                0,
                                                                            ],
                                                                        },
                                                                    ),
                                                                    name: None,
                                                                    variants_recursive: [],
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
                                                                        },
                                                                    ),
                                                                    name: None,
                                                                    variants_recursive: [],
                                                                },
                                                            },
                                                        ],
                                                    ),
                                                    layout: Some(
                                                        TyLayout {
                                                            head_size: 192,
                                                            body_ptrs: [
                                                                0,
                                                                8,
                                                            ],
                                                        },
                                                    ),
                                                    name: None,
                                                    variants_recursive: [],
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
                                                    },
                                                ),
                                                name: None,
                                                variants_recursive: [],
                                            },
                                        ),
                                        layout: Some(
                                            TyLayout {
                                                head_size: 64,
                                                body_ptrs: [
                                                    0,
                                                ],
                                            },
                                        ),
                                        name: None,
                                        variants_recursive: [],
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
                                        },
                                    ),
                                    name: None,
                                    variants_recursive: [],
                                },
                            ),
                            layout: Some(
                                TyLayout {
                                    head_size: 64,
                                    body_ptrs: [
                                        0,
                                    ],
                                },
                            ),
                            name: None,
                            variants_recursive: [],
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
                            },
                        ),
                        name: None,
                        variants_recursive: [],
                    },
                ),
                layout: Some(
                    TyLayout {
                        head_size: 64,
                        body_ptrs: [
                            0,
                        ],
                    },
                ),
                name: None,
                variants_recursive: [],
            },
        },
        types: [
            TyDef {
                name: Path(
                    [
                        "a",
                    ],
                ),
                ty: Ty {
                    kind: Primitive(
                        int64,
                    ),
                    layout: Some(
                        TyLayout {
                            head_size: 64,
                            body_ptrs: [],
                        },
                    ),
                    name: None,
                    variants_recursive: [],
                },
            },
            TyDef {
                name: Path(
                    [
                        "b",
                        "c",
                    ],
                ),
                ty: Ty {
                    kind: Primitive(
                        text,
                    ),
                    layout: Some(
                        TyLayout {
                            head_size: 64,
                            body_ptrs: [
                                0,
                            ],
                        },
                    ),
                    name: None,
                    variants_recursive: [],
                },
            },
        ],
    }
    "#);
}
