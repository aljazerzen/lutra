use lutra_bin::Value;

use crate::lutra::runtime;

const MODULE: RuntimeModule = RuntimeModule;

struct RuntimeModule;

impl runtime::Functions for RuntimeModule {
    fn hello(a: f64, b: u64) -> i64 {
        (a * 4.0) as i64 + b as i64
    }
}

#[test]
fn test_01() {
    let mut modules = lutra_runtime::BUILTIN_MODULES.to_vec();
    modules.push(("world", &runtime::Wrapper(MODULE)));

    let program = ::lutra_ir::_test_parse(
        "
    let externals = [
        world::hello
    ];

    let main = (func 0 -> 
        (
            call external.0: func (float64, uint32) -> int64,
            1.0: float64,
            2: int64,
        ): int64
    ): func () -> int64
    ",
    );
    let program = lutra_frontend::bytecode_program(program);

    let value = lutra_runtime::evaluate(&program, vec![], &modules);

    let value = Value::decode(&value, &program.output_ty).unwrap();

    assert_eq!(value, Value::Int64(6));
}
