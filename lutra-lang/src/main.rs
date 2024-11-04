use lutra_parser::pr;

mod interpreter;
mod ir;
mod native;

macro_rules! println_size_of {
    ($type_name: ty) => {
        println!(
            concat!("size_of(", stringify!($type_name), ") = {:}B"),
            std::mem::size_of::<$type_name>()
        );
    };
}

fn main() {
    println_size_of!(interpreter::Symbol);
    println!("");

    println_size_of!(std::rc::Rc<lutra_bin::Value>);
    println_size_of!(Box<ir::Function>);
    println_size_of!(interpreter::NativeFunction);

    println!("");

    // std::mem::size_of

    let program = ir::Program {
        externals: vec![ir::ExternalSymbol {
            id: "std_int_add".into(),
        }],
        main: ir::Expr {
            kind: ir::ExprKind::Binding(ir::Binding {
                symbol: 0x40000001,
                expr: Box::new(ir::Expr {
                    kind: ir::ExprKind::Function(ir::Function {
                        symbol_ns: 0x80000200,
                        body: Box::new(ir::Expr {
                            kind: ir::ExprKind::Array(vec![
                                ir::Expr {
                                    kind: ir::ExprKind::Pointer(0x80000200),
                                },
                                ir::Expr {
                                    kind: ir::ExprKind::Pointer(0x80000200),
                                },
                                ir::Expr {
                                    kind: ir::ExprKind::Pointer(0x80000200),
                                },
                            ]),
                        }),
                    }),
                }),
                main: Box::new(ir::Expr {
                    kind: ir::ExprKind::Tuple(vec![
                        ir::Expr {
                            kind: ir::ExprKind::Call(ir::Call {
                                function: Box::new(ir::Expr {
                                    kind: ir::ExprKind::Pointer(0x40000001),
                                }),
                                args: vec![ir::Expr {
                                    kind: ir::ExprKind::Literal(ir::Literal::Float(3.5)),
                                }],
                            }),
                        },
                        ir::Expr {
                            kind: ir::ExprKind::Call(ir::Call {
                                function: Box::new(ir::Expr {
                                    kind: ir::ExprKind::Function(ir::Function {
                                        symbol_ns: 0x80000300,
                                        body: Box::new(ir::Expr {
                                            kind: ir::ExprKind::Array(vec![
                                                ir::Expr {
                                                    kind: ir::ExprKind::Pointer(0x80000300),
                                                },
                                                ir::Expr {
                                                    kind: ir::ExprKind::Pointer(0x80000301),
                                                },
                                            ]),
                                        }),
                                    }),
                                }),
                                args: vec![ir::Expr {
                                    kind: ir::ExprKind::Literal(ir::Literal::Int(6)),
                                }, ir::Expr {
                                    kind: ir::ExprKind::Literal(ir::Literal::Int(7)),
                                }],
                            }),
                        },
                        ir::Expr {
                            kind: ir::ExprKind::Call(ir::Call {
                                function: Box::new(ir::Expr {
                                    kind: ir::ExprKind::Pointer(0x00000000),
                                }),
                                args: vec![
                                    ir::Expr {
                                        kind: ir::ExprKind::Literal(ir::Literal::Int(6)),
                                    },
                                    ir::Expr {
                                        kind: ir::ExprKind::Literal(ir::Literal::Int(2)),
                                    },
                                ],
                            }),
                        },
                    ]),
                }),
            }),
        },
    };

    let res = interpreter::evaluate(&program, ());

    let ty = get_ty();
    println!("{}", res.print_source(&ty).unwrap());
}

fn get_ty() -> pr::Ty {
    let source = r#"
        type t = {[float], [int], int}
    "#;

    let (stmts, _errs) = lutra_parser::parse_source(source, 0);
    let stmt = stmts.unwrap().into_iter().next().unwrap();

    let type_def = stmt.kind.into_type_def().unwrap();

    let mut ty = type_def.value.unwrap();

    ty.name = Some(type_def.name);
    ty
}
