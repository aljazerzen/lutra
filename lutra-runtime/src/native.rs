use lutra_bin::Value;

use crate::interpreter::{self, Cell};

pub fn lookup_native_symbol(id: &str) -> Cell {
    match id {
        "interpreter_version" => interpreter_version(),
        "std_int_add" => Cell::FunctionNative(&std_int_add),
        _ => panic!(),
    }
}

fn interpreter_version() -> interpreter::Cell {
    interpreter::Cell::Value(Value::Text("lutra-runtime 0.0.1".into()).into())
}

fn std_int_add(args: Vec<interpreter::Cell>) -> interpreter::Cell {
    let left = assume_int(&args[0]);
    let right = assume_int(&args[1]);

    interpreter::Cell::Value(Value::Int(left + right).into())
}

fn assume_value(symbol: &Cell) -> lutra_bin::Value {
    match symbol {
        Cell::Value(val) => val.as_ref().clone(),
        Cell::Function(_) => panic!(),
        Cell::FunctionNative(_) => panic!(),
        Cell::Vacant => panic!(),
    }
}

fn assume_int(symbol: &Cell) -> i64 {
    match assume_value(symbol) {
        Value::Int(v) => v,
        _ => panic!(),
    }
}
