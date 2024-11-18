use lutra_bin::{Encode, Layout};
use lutra_ir::ir;

use crate::interpreter::{Cell, Interpreter};
use crate::NativeModule;

pub mod core {
    pub mod int {
        use crate::native::*;

        pub const MODULE: Module = Module;

        pub struct Module;

        impl NativeModule for Module {
            fn lookup_native_symbol(&self, id: &str) -> crate::interpreter::Cell {
                match id {
                    "add" => Cell::FunctionNative(&Self::add),
                    "sub" => Cell::FunctionNative(&Self::sub),
                    "mul" => Cell::FunctionNative(&Self::mul),
                    _ => panic!(),
                }
            }
        }

        impl Module {
            pub fn add(_: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
                let left = assume::int(&args[0].1);
                let right = assume::int(&args[1].1);

                Cell::Value(encode(&(left + right)))
            }

            pub fn sub(_: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
                let left = assume::int(&args[0].1);
                let right = assume::int(&args[1].1);

                Cell::Value(encode(&(left - right)))
            }

            pub fn mul(_: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
                let left = assume::int(&args[0].1);
                let right = assume::int(&args[1].1);

                Cell::Value(encode(&(left * right)))
            }
        }
    }

    pub mod array {
        use crate::native::*;

        pub const MODULE: Module = Module;
        pub struct Module;

        impl NativeModule for Module {
            fn lookup_native_symbol(&self, id: &str) -> crate::interpreter::Cell {
                match id {
                    "map" => Cell::FunctionNative(&Self::map),
                    _ => panic!(),
                }
            }
        }

        impl Module {
            pub fn map(it: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
                let func = &args[0];
                let array = assume::array(&args[1].0, &args[1].1);

                let ir::TyKind::Function(ty_func) = &func.0.kind else {
                    panic!()
                };
                let output_ty = ir::Ty {
                    kind: ir::TyKind::Array(Box::new(ty_func.body.clone())),
                    layout: ir::TyLayout {
                        head_size: 0,
                        variants_recursive: vec![],
                    },
                };
                let output_ty = lutra_ir::ty_into_pr(output_ty);
                let mut res = lutra_bin::ArrayWriter::new(&output_ty);

                for item in array {
                    let cell = Cell::Value(item);

                    let value = it.evaluate_func_call(&func.1, vec![(&args[1].0, cell)]);

                    res.write_item(assume::into_value(value));
                }

                Cell::Value(res.finish())
            }
        }
    }
}

pub mod interpreter {
    use crate::native::*;

    pub const MODULE: Module = Module;
    pub struct Module;

    impl NativeModule for Module {
        fn lookup_native_symbol(&self, id: &str) -> crate::interpreter::Cell {
            match id {
                "version" => Self::version(),
                _ => panic!(),
            }
        }
    }

    impl Module {
        fn version() -> Cell {
            Cell::Value(encode(&"lutra-runtime 0.0.1".to_string()))
        }
    }
}

mod assume {
    use crate::interpreter::Cell;
    use lutra_bin::{ArrayReader, Decode};
    use lutra_ir::ir;

    pub fn into_value(cell: Cell) -> lutra_bin::Data {
        match cell {
            Cell::Value(val) => val,
            Cell::Function(_) => panic!(),
            Cell::FunctionNative(_) => panic!(),
            Cell::Vacant => panic!(),
        }
    }

    pub fn as_value(cell: &Cell) -> &lutra_bin::Data {
        match cell {
            Cell::Value(val) => val,
            Cell::Function(_) => panic!(),
            Cell::FunctionNative(_) => panic!(),
            Cell::Vacant => panic!(),
        }
    }

    pub fn int(cell: &Cell) -> i64 {
        let bytes = as_value(cell);
        i64::decode_buffer(bytes.slice(8)).unwrap()
    }

    pub fn array<'d>(ty: &ir::Ty, cell: &'d Cell) -> ArrayReader {
        let ty = lutra_ir::ty_into_pr(ty.clone());

        let data = as_value(cell).clone();
        ArrayReader::new_for_ty(data, &ty)
    }
}

pub fn encode<T: Encode + Layout>(value: &T) -> lutra_bin::Data {
    let mut buf = Vec::with_capacity(T::head_size() / 8);
    value.encode(&mut buf).unwrap();
    lutra_bin::Data::new(buf)
}
