#![cfg(test)]
use insta::assert_debug_snapshot;

#[test]
fn parse_01() {
    assert_debug_snapshot!(super::_test_parse(r#"type a = int64;
type std::Float64 = Prim64;
type std::Int64 = Prim64;
type std::Text = [Prim8];
type b::c = std::Text;
let main =
  let 1 = (func 2 ->
    (array
      fn.2+0: std::Float64,
      fn.2+0: std::Float64,
      fn.2+0: std::Float64
    ): [std::Float64]
  ): func (std::Float64) -> [std::Float64];

  let 2 = var.1: func (std::Float64) -> [std::Float64];

  (tuple_lookup
    (tuple
      (call
        var.2: func (std::Float64) -> [std::Float64],
        3.5: std::Float64
      ): [std::Float64],
      (call
          (func 3 ->
            (array
              fn.3+0: std::Int64,
              fn.3+1: std::Int64,
            ): [std::Int64]
          ): func (std::Int64, std::Int64) -> [std::Int64],
          6: std::Int64,
          7: std::Int64,
        ): [std::Int64],
      (call
        external.std::int::add: func (std::Int64, std::Int64) -> std::Int64,
        6: std::Int64,
        2: std::Int64
      ): std::Int64
    ): {[std::Float64], [std::Int64], std::Int64}
    1
  ): [std::Int64]
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
                                                    kind: Ident(
                                                        Path(
                                                            [
                                                                "std",
                                                                "Float64",
                                                            ],
                                                        ),
                                                    ),
                                                    layout: None,
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
                                                    kind: Ident(
                                                        Path(
                                                            [
                                                                "std",
                                                                "Float64",
                                                            ],
                                                        ),
                                                    ),
                                                    layout: None,
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
                                                    kind: Ident(
                                                        Path(
                                                            [
                                                                "std",
                                                                "Float64",
                                                            ],
                                                        ),
                                                    ),
                                                    layout: None,
                                                    name: None,
                                                    variants_recursive: [],
                                                },
                                            },
                                        ],
                                    ),
                                    ty: Ty {
                                        kind: Array(
                                            Ty {
                                                kind: Ident(
                                                    Path(
                                                        [
                                                            "std",
                                                            "Float64",
                                                        ],
                                                    ),
                                                ),
                                                layout: None,
                                                name: None,
                                                variants_recursive: [],
                                            },
                                        ),
                                        layout: None,
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
                                            kind: Ident(
                                                Path(
                                                    [
                                                        "std",
                                                        "Float64",
                                                    ],
                                                ),
                                            ),
                                            layout: None,
                                            name: None,
                                            variants_recursive: [],
                                        },
                                    ],
                                    body: Ty {
                                        kind: Array(
                                            Ty {
                                                kind: Ident(
                                                    Path(
                                                        [
                                                            "std",
                                                            "Float64",
                                                        ],
                                                    ),
                                                ),
                                                layout: None,
                                                name: None,
                                                variants_recursive: [],
                                            },
                                        ),
                                        layout: None,
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
                                                        kind: Ident(
                                                            Path(
                                                                [
                                                                    "std",
                                                                    "Float64",
                                                                ],
                                                            ),
                                                        ),
                                                        layout: None,
                                                        name: None,
                                                        variants_recursive: [],
                                                    },
                                                ],
                                                body: Ty {
                                                    kind: Array(
                                                        Ty {
                                                            kind: Ident(
                                                                Path(
                                                                    [
                                                                        "std",
                                                                        "Float64",
                                                                    ],
                                                                ),
                                                            ),
                                                            layout: None,
                                                            name: None,
                                                            variants_recursive: [],
                                                        },
                                                    ),
                                                    layout: None,
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
                                                        TupleField {
                                                            expr: Expr {
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
                                                                                                kind: Ident(
                                                                                                    Path(
                                                                                                        [
                                                                                                            "std",
                                                                                                            "Float64",
                                                                                                        ],
                                                                                                    ),
                                                                                                ),
                                                                                                layout: None,
                                                                                                name: None,
                                                                                                variants_recursive: [],
                                                                                            },
                                                                                        ],
                                                                                        body: Ty {
                                                                                            kind: Array(
                                                                                                Ty {
                                                                                                    kind: Ident(
                                                                                                        Path(
                                                                                                            [
                                                                                                                "std",
                                                                                                                "Float64",
                                                                                                            ],
                                                                                                        ),
                                                                                                    ),
                                                                                                    layout: None,
                                                                                                    name: None,
                                                                                                    variants_recursive: [],
                                                                                                },
                                                                                            ),
                                                                                            layout: None,
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
                                                                                    Prim64(
                                                                                        4615063718147915776,
                                                                                    ),
                                                                                ),
                                                                                ty: Ty {
                                                                                    kind: Ident(
                                                                                        Path(
                                                                                            [
                                                                                                "std",
                                                                                                "Float64",
                                                                                            ],
                                                                                        ),
                                                                                    ),
                                                                                    layout: None,
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
                                                                            kind: Ident(
                                                                                Path(
                                                                                    [
                                                                                        "std",
                                                                                        "Float64",
                                                                                    ],
                                                                                ),
                                                                            ),
                                                                            layout: None,
                                                                            name: None,
                                                                            variants_recursive: [],
                                                                        },
                                                                    ),
                                                                    layout: None,
                                                                    name: None,
                                                                    variants_recursive: [],
                                                                },
                                                            },
                                                            unpack: false,
                                                        },
                                                        TupleField {
                                                            expr: Expr {
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
                                                                                                        kind: Ident(
                                                                                                            Path(
                                                                                                                [
                                                                                                                    "std",
                                                                                                                    "Int64",
                                                                                                                ],
                                                                                                            ),
                                                                                                        ),
                                                                                                        layout: None,
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
                                                                                                        kind: Ident(
                                                                                                            Path(
                                                                                                                [
                                                                                                                    "std",
                                                                                                                    "Int64",
                                                                                                                ],
                                                                                                            ),
                                                                                                        ),
                                                                                                        layout: None,
                                                                                                        name: None,
                                                                                                        variants_recursive: [],
                                                                                                    },
                                                                                                },
                                                                                            ],
                                                                                        ),
                                                                                        ty: Ty {
                                                                                            kind: Array(
                                                                                                Ty {
                                                                                                    kind: Ident(
                                                                                                        Path(
                                                                                                            [
                                                                                                                "std",
                                                                                                                "Int64",
                                                                                                            ],
                                                                                                        ),
                                                                                                    ),
                                                                                                    layout: None,
                                                                                                    name: None,
                                                                                                    variants_recursive: [],
                                                                                                },
                                                                                            ),
                                                                                            layout: None,
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
                                                                                                kind: Ident(
                                                                                                    Path(
                                                                                                        [
                                                                                                            "std",
                                                                                                            "Int64",
                                                                                                        ],
                                                                                                    ),
                                                                                                ),
                                                                                                layout: None,
                                                                                                name: None,
                                                                                                variants_recursive: [],
                                                                                            },
                                                                                            Ty {
                                                                                                kind: Ident(
                                                                                                    Path(
                                                                                                        [
                                                                                                            "std",
                                                                                                            "Int64",
                                                                                                        ],
                                                                                                    ),
                                                                                                ),
                                                                                                layout: None,
                                                                                                name: None,
                                                                                                variants_recursive: [],
                                                                                            },
                                                                                        ],
                                                                                        body: Ty {
                                                                                            kind: Array(
                                                                                                Ty {
                                                                                                    kind: Ident(
                                                                                                        Path(
                                                                                                            [
                                                                                                                "std",
                                                                                                                "Int64",
                                                                                                            ],
                                                                                                        ),
                                                                                                    ),
                                                                                                    layout: None,
                                                                                                    name: None,
                                                                                                    variants_recursive: [],
                                                                                                },
                                                                                            ),
                                                                                            layout: None,
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
                                                                                    Prim64(
                                                                                        6,
                                                                                    ),
                                                                                ),
                                                                                ty: Ty {
                                                                                    kind: Ident(
                                                                                        Path(
                                                                                            [
                                                                                                "std",
                                                                                                "Int64",
                                                                                            ],
                                                                                        ),
                                                                                    ),
                                                                                    layout: None,
                                                                                    name: None,
                                                                                    variants_recursive: [],
                                                                                },
                                                                            },
                                                                            Expr {
                                                                                kind: Literal(
                                                                                    Prim64(
                                                                                        7,
                                                                                    ),
                                                                                ),
                                                                                ty: Ty {
                                                                                    kind: Ident(
                                                                                        Path(
                                                                                            [
                                                                                                "std",
                                                                                                "Int64",
                                                                                            ],
                                                                                        ),
                                                                                    ),
                                                                                    layout: None,
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
                                                                            kind: Ident(
                                                                                Path(
                                                                                    [
                                                                                        "std",
                                                                                        "Int64",
                                                                                    ],
                                                                                ),
                                                                            ),
                                                                            layout: None,
                                                                            name: None,
                                                                            variants_recursive: [],
                                                                        },
                                                                    ),
                                                                    layout: None,
                                                                    name: None,
                                                                    variants_recursive: [],
                                                                },
                                                            },
                                                            unpack: false,
                                                        },
                                                        TupleField {
                                                            expr: Expr {
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
                                                                                                kind: Ident(
                                                                                                    Path(
                                                                                                        [
                                                                                                            "std",
                                                                                                            "Int64",
                                                                                                        ],
                                                                                                    ),
                                                                                                ),
                                                                                                layout: None,
                                                                                                name: None,
                                                                                                variants_recursive: [],
                                                                                            },
                                                                                            Ty {
                                                                                                kind: Ident(
                                                                                                    Path(
                                                                                                        [
                                                                                                            "std",
                                                                                                            "Int64",
                                                                                                        ],
                                                                                                    ),
                                                                                                ),
                                                                                                layout: None,
                                                                                                name: None,
                                                                                                variants_recursive: [],
                                                                                            },
                                                                                        ],
                                                                                        body: Ty {
                                                                                            kind: Ident(
                                                                                                Path(
                                                                                                    [
                                                                                                        "std",
                                                                                                        "Int64",
                                                                                                    ],
                                                                                                ),
                                                                                            ),
                                                                                            layout: None,
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
                                                                                    Prim64(
                                                                                        6,
                                                                                    ),
                                                                                ),
                                                                                ty: Ty {
                                                                                    kind: Ident(
                                                                                        Path(
                                                                                            [
                                                                                                "std",
                                                                                                "Int64",
                                                                                            ],
                                                                                        ),
                                                                                    ),
                                                                                    layout: None,
                                                                                    name: None,
                                                                                    variants_recursive: [],
                                                                                },
                                                                            },
                                                                            Expr {
                                                                                kind: Literal(
                                                                                    Prim64(
                                                                                        2,
                                                                                    ),
                                                                                ),
                                                                                ty: Ty {
                                                                                    kind: Ident(
                                                                                        Path(
                                                                                            [
                                                                                                "std",
                                                                                                "Int64",
                                                                                            ],
                                                                                        ),
                                                                                    ),
                                                                                    layout: None,
                                                                                    name: None,
                                                                                    variants_recursive: [],
                                                                                },
                                                                            },
                                                                        ],
                                                                    },
                                                                ),
                                                                ty: Ty {
                                                                    kind: Ident(
                                                                        Path(
                                                                            [
                                                                                "std",
                                                                                "Int64",
                                                                            ],
                                                                        ),
                                                                    ),
                                                                    layout: None,
                                                                    name: None,
                                                                    variants_recursive: [],
                                                                },
                                                            },
                                                            unpack: false,
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
                                                                            kind: Ident(
                                                                                Path(
                                                                                    [
                                                                                        "std",
                                                                                        "Float64",
                                                                                    ],
                                                                                ),
                                                                            ),
                                                                            layout: None,
                                                                            name: None,
                                                                            variants_recursive: [],
                                                                        },
                                                                    ),
                                                                    layout: None,
                                                                    name: None,
                                                                    variants_recursive: [],
                                                                },
                                                            },
                                                            TyTupleField {
                                                                name: None,
                                                                ty: Ty {
                                                                    kind: Array(
                                                                        Ty {
                                                                            kind: Ident(
                                                                                Path(
                                                                                    [
                                                                                        "std",
                                                                                        "Int64",
                                                                                    ],
                                                                                ),
                                                                            ),
                                                                            layout: None,
                                                                            name: None,
                                                                            variants_recursive: [],
                                                                        },
                                                                    ),
                                                                    layout: None,
                                                                    name: None,
                                                                    variants_recursive: [],
                                                                },
                                                            },
                                                            TyTupleField {
                                                                name: None,
                                                                ty: Ty {
                                                                    kind: Ident(
                                                                        Path(
                                                                            [
                                                                                "std",
                                                                                "Int64",
                                                                            ],
                                                                        ),
                                                                    ),
                                                                    layout: None,
                                                                    name: None,
                                                                    variants_recursive: [],
                                                                },
                                                            },
                                                        ],
                                                    ),
                                                    layout: None,
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
                                                kind: Ident(
                                                    Path(
                                                        [
                                                            "std",
                                                            "Int64",
                                                        ],
                                                    ),
                                                ),
                                                layout: None,
                                                name: None,
                                                variants_recursive: [],
                                            },
                                        ),
                                        layout: None,
                                        name: None,
                                        variants_recursive: [],
                                    },
                                },
                            },
                        ),
                        ty: Ty {
                            kind: Array(
                                Ty {
                                    kind: Ident(
                                        Path(
                                            [
                                                "std",
                                                "Int64",
                                            ],
                                        ),
                                    ),
                                    layout: None,
                                    name: None,
                                    variants_recursive: [],
                                },
                            ),
                            layout: None,
                            name: None,
                            variants_recursive: [],
                        },
                    },
                },
            ),
            ty: Ty {
                kind: Array(
                    Ty {
                        kind: Ident(
                            Path(
                                [
                                    "std",
                                    "Int64",
                                ],
                            ),
                        ),
                        layout: None,
                        name: None,
                        variants_recursive: [],
                    },
                ),
                layout: None,
                name: None,
                variants_recursive: [],
            },
        },
        defs: [
            TyDef {
                name: Path(
                    [
                        "a",
                    ],
                ),
                ty: Ty {
                    kind: Ident(
                        Path(
                            [
                                "int64",
                            ],
                        ),
                    ),
                    layout: None,
                    name: None,
                    variants_recursive: [],
                },
            },
            TyDef {
                name: Path(
                    [
                        "std",
                        "Float64",
                    ],
                ),
                ty: Ty {
                    kind: Primitive(
                        Prim64,
                    ),
                    layout: None,
                    name: None,
                    variants_recursive: [],
                },
            },
            TyDef {
                name: Path(
                    [
                        "std",
                        "Int64",
                    ],
                ),
                ty: Ty {
                    kind: Primitive(
                        Prim64,
                    ),
                    layout: None,
                    name: None,
                    variants_recursive: [],
                },
            },
            TyDef {
                name: Path(
                    [
                        "std",
                        "Text",
                    ],
                ),
                ty: Ty {
                    kind: Array(
                        Ty {
                            kind: Primitive(
                                Prim8,
                            ),
                            layout: None,
                            name: None,
                            variants_recursive: [],
                        },
                    ),
                    layout: None,
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
                    kind: Ident(
                        Path(
                            [
                                "std",
                                "Text",
                            ],
                        ),
                    ),
                    layout: None,
                    name: None,
                    variants_recursive: [],
                },
            },
        ],
    }
    "#);
}
