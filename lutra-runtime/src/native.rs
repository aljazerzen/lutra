use lutra_bin::Value;

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
            pub fn add(_: &mut Interpreter, args: Vec<Cell>) -> Value {
                let left = assume::int(&args[0]);
                let right = assume::int(&args[1]);

                Value::Int(left + right)
            }

            pub fn sub(_: &mut Interpreter, args: Vec<Cell>) -> Value {
                let left = assume::int(&args[0]);
                let right = assume::int(&args[1]);

                Value::Int(left - right)
            }

            pub fn mul(_: &mut Interpreter, args: Vec<Cell>) -> Value {
                let left = assume::int(&args[0]);
                let right = assume::int(&args[1]);

                Value::Int(left * right)
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
            pub fn map(it: &mut Interpreter, args: Vec<Cell>) -> Value {
                let func = &args[0];
                let array = assume::array(&args[1]);

                let mut res = if let Some(upper_limit) = array.size_hint().1 {
                    Vec::with_capacity(upper_limit)
                } else {
                    Vec::new()
                };
                for item in array {
                    let cell = Cell::Value(item.clone().into());
                    let cell = it.evaluate_func_call(func, vec![cell]);
                    res.push(assume::into_value(&cell));
                }

                Value::Array(res)
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
                "version" => Cell::Value(std::rc::Rc::new(Self::version())),
                _ => panic!(),
            }
        }
    }

    impl Module {
        fn version() -> Value {
            Value::Text("lutra-runtime 0.0.1".into())
        }
    }
}

mod assume {
    use crate::interpreter::Cell;
    use lutra_bin::Value;

    pub fn into_value(cell: &Cell) -> lutra_bin::Value {
        match cell {
            Cell::Value(val) => val.as_ref().clone(),
            Cell::Function(_) => panic!(),
            Cell::FunctionNative(_) => panic!(),
            Cell::Vacant => panic!(),
        }
    }

    pub fn as_value(cell: &Cell) -> &lutra_bin::Value {
        match cell {
            Cell::Value(val) => val.as_ref(),
            Cell::Function(_) => panic!(),
            Cell::FunctionNative(_) => panic!(),
            Cell::Vacant => panic!(),
        }
    }

    pub fn int(cell: &Cell) -> i64 {
        match into_value(cell) {
            Value::Int(v) => v,
            _ => panic!(),
        }
    }

    pub fn array(cell: &Cell) -> impl Iterator<Item = &Value> {
        match as_value(cell) {
            Value::Array(items) => items.iter(),
            _ => panic!(),
        }
    }
}
