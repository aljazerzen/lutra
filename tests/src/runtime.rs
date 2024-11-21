use lutra_bin::ir;
use lutra_bin::{Data, Encode, Value};
use lutra_runtime::{Cell, Interpreter};

use crate::lutra::runtime;

const MODULE: RuntimeModule = RuntimeModule;

struct RuntimeModule;

impl runtime::NativeFunctions for RuntimeModule {
    fn hello(_interpreter: &mut Interpreter, _args: Vec<(&ir::Ty, Cell)>) -> Cell {
        let mut buf = Vec::with_capacity(8);
        2_i64.encode(&mut buf).unwrap();
        Cell::Data(Data::new(buf))
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
        call external.0: func (int64, int64) -> int64,
        1: int64,
        2: int64,
    ): int64
    ",
    );

    let value = lutra_runtime::evaluate(&program, (), &modules);

    let value = Value::decode(&value, &program.main.ty).unwrap();

    assert_eq!(value, Value::Int64(2));
}
