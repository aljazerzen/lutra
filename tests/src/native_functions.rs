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
#[ignore = "needs update for prim type collapse"]
fn test_01() {
    let mut modules = lutra_interpreter::BUILTIN_MODULES.to_vec();
    modules.push(("world", &runtime::Wrapper(MODULE)));

    let program = ::lutra_ir::_test_parse(
        "
    let main = (func 0 ->
        (call
            external.world::hello: func (prim64, prim64) -> prim64,
            4: prim64,
            2: prim64,
        ): prim64
    ): func () -> prim64
    ",
    );
    let output_ty = program.get_output_ty().clone();
    let bytecode = lutra_compiler::bytecode_program(program);

    let value = lutra_interpreter::evaluate(&bytecode, vec![], &modules, None).unwrap();

    let value = Value::decode(&value, &output_ty, &[]).unwrap();

    assert_eq!(value, Value::Prim64(6));
}
