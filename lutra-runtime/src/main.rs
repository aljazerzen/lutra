use lutra_frontend::pr;

mod interpreter;
mod native;

mod ir {
    include!(concat!(env!("OUT_DIR"), "/ir.rs"));
}

macro_rules! println_size_of {
    ($type_name: ty) => {
        println!(
            concat!("size_of(", stringify!($type_name), ") = {:}B"),
            std::mem::size_of::<$type_name>()
        );
    };
}

fn main() {
    println_size_of!(interpreter::Cell);
    println!();

    println_size_of!(std::rc::Rc<lutra_bin::Value>);
    println_size_of!(Box<ir::Function>);
    println_size_of!(interpreter::NativeFunction);

    println!();

    // std::mem::size_of

    let program = ir::Program {
        externals: vec![ir::ExternalSymbol {
            id: "std_int_add".into(),
        }],
        main: ir::Expr {
            kind: ir::ExprKind::Binding(Box::new(ir::Binding {
                symbol: ir::Sid(0x40000001),
                expr: ir::Expr {
                    kind: ir::ExprKind::Function(Box::new(ir::Function {
                        symbol_ns: ir::Sid(0x80000200),
                        body: ir::Expr {
                            kind: ir::ExprKind::Array(vec![
                                ir::Expr {
                                    kind: ir::ExprKind::Pointer(ir::Sid(0x80000200)),
                                },
                                ir::Expr {
                                    kind: ir::ExprKind::Pointer(ir::Sid(0x80000200)),
                                },
                                ir::Expr {
                                    kind: ir::ExprKind::Pointer(ir::Sid(0x80000200)),
                                },
                            ]),
                        },
                    })),
                },
                main: ir::Expr {
                    kind: ir::ExprKind::Tuple(vec![
                        ir::Expr {
                            kind: ir::ExprKind::Call(Box::new(ir::Call {
                                function: ir::Expr {
                                    kind: ir::ExprKind::Pointer(ir::Sid(0x40000001)),
                                },
                                args: vec![ir::Expr {
                                    kind: ir::ExprKind::Literal(ir::Literal::Float(3.5)),
                                }],
                            })),
                        },
                        ir::Expr {
                            kind: ir::ExprKind::Call(Box::new(ir::Call {
                                function: ir::Expr {
                                    kind: ir::ExprKind::Function(Box::new(ir::Function {
                                        symbol_ns: ir::Sid(0x80000300),
                                        body: ir::Expr {
                                            kind: ir::ExprKind::Array(vec![
                                                ir::Expr {
                                                    kind: ir::ExprKind::Pointer(ir::Sid(
                                                        0x80000300,
                                                    )),
                                                },
                                                ir::Expr {
                                                    kind: ir::ExprKind::Pointer(ir::Sid(
                                                        0x80000301,
                                                    )),
                                                },
                                            ]),
                                        },
                                    })),
                                },
                                args: vec![
                                    ir::Expr {
                                        kind: ir::ExprKind::Literal(ir::Literal::Int(6)),
                                    },
                                    ir::Expr {
                                        kind: ir::ExprKind::Literal(ir::Literal::Int(7)),
                                    },
                                ],
                            })),
                        },
                        ir::Expr {
                            kind: ir::ExprKind::Call(Box::new(ir::Call {
                                function: ir::Expr {
                                    kind: ir::ExprKind::Pointer(ir::Sid(0x00000000)),
                                },
                                args: vec![
                                    ir::Expr {
                                        kind: ir::ExprKind::Literal(ir::Literal::Int(6)),
                                    },
                                    ir::Expr {
                                        kind: ir::ExprKind::Literal(ir::Literal::Int(2)),
                                    },
                                ],
                            })),
                        },
                    ]),
                },
            })),
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

    let source = lutra_frontend::SourceTree::single("".into(), source.into());
    let project = lutra_frontend::compile(source, lutra_frontend::CompileParams {}).unwrap();

    let name = pr::Path::from_name("t");
    let type_def = project.root_module.module.get(&name);

    type_def.unwrap().kind.as_ty().unwrap().clone()
}
