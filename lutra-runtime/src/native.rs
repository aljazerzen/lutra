use lutra_bin::Value;

use crate::interpreter::{self, Cell, Interpreter};

pub fn lookup_native_symbol(id: &str) -> Cell {
    match id {
        "interpreter_version" => interpreter_version(),
        "core_int_add" => Cell::FunctionNative(&core::int::add),
        "core_int_sub" => Cell::FunctionNative(&core::int::sub),
        "core_int_mul" => Cell::FunctionNative(&core::int::mul),
        "core_array_map" => Cell::FunctionNative(&core::array::map),
        _ => panic!(),
    }
}

mod core {
    pub mod int {
        use crate::native::*;

        pub fn add(_: &mut Interpreter, args: Vec<Cell>) -> interpreter::Cell {
            let left = assume::int(&args[0]);
            let right = assume::int(&args[1]);

            interpreter::Cell::Value(Value::Int(left + right).into())
        }

        pub fn sub(_: &mut Interpreter, args: Vec<Cell>) -> interpreter::Cell {
            let left = assume::int(&args[0]);
            let right = assume::int(&args[1]);

            interpreter::Cell::Value(Value::Int(left - right).into())
        }

        pub fn mul(_: &mut Interpreter, args: Vec<Cell>) -> interpreter::Cell {
            let left = assume::int(&args[0]);
            let right = assume::int(&args[1]);

            interpreter::Cell::Value(Value::Int(left * right).into())
        }
    }

    pub mod array {
        use crate::native::*;

        pub fn map(it: &mut Interpreter, args: Vec<Cell>) -> interpreter::Cell {
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

            interpreter::Cell::Value(Value::Array(res).into())
        }
    }
}

fn interpreter_version() -> interpreter::Cell {
    interpreter::Cell::Value(Value::Text("lutra-runtime 0.0.1".into()).into())
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
