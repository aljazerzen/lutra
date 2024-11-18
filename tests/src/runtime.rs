use lutra_bin::{Data, Encode, Value};
use lutra_ir::ir;
use lutra_runtime::{Cell, Interpreter};

use crate::lutra::runtime;

const MODULE: RuntimeModule = RuntimeModule;

struct RuntimeModule;

impl runtime::NativeFunctions for RuntimeModule {
    fn hello(_interpreter: &mut Interpreter, _args: Vec<(&ir::Ty, Cell)>) -> Cell {
        let mut buf = Vec::with_capacity(8);
        2_i64.encode(&mut buf).unwrap();
        Cell::Value(Data::new(buf))
    }
}

#[test]
fn test_01() {
    let mut modules = lutra_runtime::BUILTIN_MODULES.to_vec();
    modules.push(("world", &runtime::Wrapper(MODULE)));

    let program = ::lutra_ir::_test_parse(
        "
    let externals = [
        world_hello
    ];

    let main = (
        call external.0: func (int, int) -> int,
        1: int,
        2: int
    ): int
    ",
    );

    let value = lutra_runtime::evaluate(&program, (), &modules);

    let ty = lutra_ir::ty_into_pr(program.main.ty);
    let value = Value::decode(&value, &ty).unwrap();

    assert_eq!(value, Value::Int(2));
}
