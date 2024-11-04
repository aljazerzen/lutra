use lutra_bin::Value;

use crate::interpreter::{self, Symbol};

pub fn lookup_native_symbol(id: &str) -> Symbol {
    match id {
        "interpreter_version" => interpreter_version(),
        "std_int_add" => Symbol::FunctionNative(&std_int_add),
        _ => panic!(),
    }
}

fn interpreter_version() -> interpreter::Symbol {
    interpreter::Symbol::Value(Value::Text("lutra-lang 0.0.1".into()).into())
}

fn std_int_add(args: Vec<interpreter::Symbol>) -> interpreter::Symbol {
    let left = assume_int(&args[0]);
    let right = assume_int(&args[1]);

    interpreter::Symbol::Value(Value::Int(left + right).into())
}

fn assume_value(symbol: &Symbol) -> lutra_bin::Value {
    match symbol {
        Symbol::Value(val) => val.as_ref().clone(),
        Symbol::Function(_) => panic!(),
        Symbol::FunctionNative(_) => panic!(),
        Symbol::Vacant => panic!(),
    }
}

fn assume_int(symbol: &Symbol) -> i64 {
    match assume_value(symbol) {
        Value::Int(v) => v,
        _ => panic!(),
    }
}
