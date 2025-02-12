#![cfg(test)]

use insta::assert_snapshot;

#[track_caller]
fn _test_interpret(program: &str) -> String {
    let program = lutra_ir::_test_parse(program);
    let output_ty = program.get_output_ty().clone();
    let program = lutra_frontend::bytecode_program(program);
    dbg!(&program);

    let output = crate::interpreter::evaluate(&program, vec![], crate::BUILTIN_MODULES);

    let output = lutra_bin::Value::decode(&output, &output_ty).unwrap();
    output.print_source(&output_ty).unwrap()
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
#[ignore]
fn interpret_02() {
    assert_snapshot!(_test_interpret(r#"
    let externals = [std::map];

    let main = (func 0 ->
        let 1 = (
            func 1 -> {
                fn.1+0: int64,
                fn.1+0: int64,
            }: {int64, int64}
        ): func (int64) -> {int64, int64};
        (
            call external.0: func (
                func (int64) -> {int64, int64}, [int64]
            ) -> [{int64, int64}],
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
#[ignore]
fn interpret_03() {
    assert_snapshot!(_test_interpret(r#"
    let externals = [std::map, std::mul];

    let main = (func 0 ->
        let 1 = [
            {1:int64, 3:int64}: {int64, int64}, 
            {5:int64, 4:int64}: {int64, int64}, 
            {2:int64, 3:int64}: {int64, int64},
        ]: [{int64, int64}];
        let 2 = (
            func 1 -> (
                call external.1: func (int64, int64) -> int64,
                fn.1+0: {int64, int64} .0: int64,
                fn.1+0: {int64, int64} .1: int64,
            ): int64
        ): func ({int64, int64}) -> int64;
        (
            call external.0: func (func ({int64, int64}) -> int64, [{int64, int64}]) -> [int64],
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
