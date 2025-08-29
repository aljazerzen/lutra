#![cfg(test)]

use insta::{assert_debug_snapshot, assert_snapshot};

use lutra_interpreter::EvalError;

#[track_caller]
fn _test_interpret(program: &str) -> String {
    let program = lutra_ir::_test_parse(program);
    let bytecode = lutra_compiler::bytecode_program(program.clone());

    let output =
        lutra_interpreter::evaluate(&bytecode, vec![], lutra_interpreter::BUILTIN_MODULES).unwrap();

    lutra_bin::print_source(&output, program.get_output_ty(), &[]).unwrap()
}

#[track_caller]
fn _test_err(program: &str) -> EvalError {
    let program = lutra_ir::_test_parse(program);
    let bytecode = lutra_compiler::bytecode_program(program.clone());

    lutra_interpreter::evaluate(&bytecode, vec![], lutra_interpreter::BUILTIN_MODULES).unwrap_err()
}

#[test]
fn interpreter_layout() {
    // TODO: when we have a bench, see if boxes would yield any speed up
    insta::assert_snapshot!(std::mem::size_of::<lutra_interpreter::Cell>(), @"32");
}

#[test]
fn interpret_01() {
    assert_snapshot!(_test_interpret(r#"
    let main = (func 3 ->
        let 1 = (
            func 2 -> [
                fn.2+0: float64,
                fn.2+0: float64,
                fn.2+0: float64
            ]: [float64]
        ): func (float64) -> [float64];
        let 2 = var.1: func (float64) -> [float64];
        {
            (call
                var.2: func (float64) -> [float64],
                3.5: float64
            ): [float64],
            (call
                (
                    func 3 -> [fn.3+0: int64, fn.3+1: int64]: [int64]
                ): func (int64) -> [int64],
                6: int64,
                7: int64,
            ): [int64],
            (call
                external.std::add: func (int64) -> int64,
                6: int64,
                2: int64,
            ): int64,
        }: {[float64], [int64], int64}
    ): func () -> {[float64], [int64], int64}
    "#,
    ), @r#"
    {
      [
        3.5,
        3.5,
        3.5,
      ],
      [
        6,
        7,
      ],
      8,
    }
    "#
    );
}

#[test]
fn interpret_02() {
    assert_snapshot!(_test_interpret(r#"
    let main = (func 0 ->
        let 1 = (
            func 1 -> {
                fn.1+0: int64,
                fn.1+0: int64,
            }: {int64, int64}
        ): func (int64) -> {int64, int64};
        (
            call external.std::map: func ([int64], func (int64) -> {int64, int64}) -> [{int64, int64}],
            [2: int64, 3: int64, 1: int64]: [int64],
            var.1: func (int64) -> {int64, int64},
        ): [{int64, int64}]
    ): func () -> [{int64, int64}]
    "#,
    ), @r#"
    [
      {
        2,
        2,
      },
      {
        3,
        3,
      },
      {
        1,
        1,
      },
    ]
    "#
    );
}

#[test]
fn interpret_03() {
    assert_snapshot!(_test_interpret(r#"
    let main = (func 0 ->
        let 1 = [
            {1:int64, 3:int64}: {int64, int64},
            {5:int64, 4:int64}: {int64, int64},
            {2:int64, 3:int64}: {int64, int64},
        ]: [{int64, int64}];
        let 2 = (func 1 ->
          (call external.std::mul: func (int64, int64) -> int64,
            (tuple_lookup
              fn.1+0: {int64, int64}
              0
            ): int64,
            (tuple_lookup
              fn.1+0: {int64, int64}
              1
            ): int64,
          ): int64
        ): func ({int64, int64}) -> int64;
        (call external.std::map: func ([{int64, int64}], func ({int64, int64}) -> int64) -> [int64],
            var.1: [{int64, int64}],
            var.2: func ({int64, int64}) -> int64,
        ): [int64]
    ): func () -> [int64]
    "#,
    ), @r#"
    [
      3,
      20,
      6,
    ]
    "#
    );
}

#[test]
fn eval_error_00() {
    assert_debug_snapshot!(_test_err(r#"
        let main = (func 0 ->
            var.1: int64
        ): func () -> int64
        "#), @"BadProgram",
    );
}

#[test]
#[ignore]
fn eval_error_01() {
    assert_debug_snapshot!(_test_err(r#"
        let main = (func 0 ->
          (tuple_lookup
            fn.0+0: int64,
            0
          )
        ): func ({int64}) -> int64
        "#), @"BadInputs",
    );
}
