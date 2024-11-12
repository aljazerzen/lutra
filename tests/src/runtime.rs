use lutra_bin::Value;

use crate::lutra::runtime;

const MODULE: RuntimeModule = RuntimeModule;

struct RuntimeModule;

impl runtime::NativeFunctions for RuntimeModule {
    fn hello(
        _interpreter: &mut ::lutra_runtime::Interpreter,
        _args: Vec<::lutra_runtime::Cell>,
    ) -> ::lutra_bin::Value {
        ::lutra_bin::Value::Int(2)
    }
}

#[test]
fn test_01() {
    let mut modules = ::lutra_runtime::BUILTIN_MODULES.to_vec();
    modules.push(("world", &runtime::Wrapper(MODULE)));

    let program = ::lutra_ir::_test_parse(
        "
    let externals = [
        world_hello
    ];

    let main = (1, 2) | external.0
    ",
    );

    let value = ::lutra_runtime::evaluate(&program, (), &modules);

    assert_eq!(value, Value::Int(2));
}
