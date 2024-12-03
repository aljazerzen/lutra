#![cfg(test)]

use insta::assert_snapshot;

#[track_caller]
fn _test_interpret(program: &str) -> String {
    let program = lutra_ir::_test_parse(program);

    let value = crate::interpreter::evaluate(&program, (), crate::BUILTIN_MODULES);

    let ty = &program.main.ty;
    let value = lutra_bin::Value::decode(&value, ty).unwrap();
    value.print_source(ty).unwrap()
}

#[test]
fn interpret_01() {
    assert_snapshot!(_test_interpret(r#"
    let externals = [std::int::add];

    let main =
        let 1 = (
            func 2 -> [
                fn.2+0: float64,
                fn.2+0: float64,
                fn.2+0: float64
            ]: [float64]
        ): func (float64) -> [float64];
        let 2 = var.1: func (float64) -> [float64];
        {
            (
                call var.2: func (float64) -> [float64],
                3.5: float64
            ): [float64],
            (
                call (
                    func 3 -> [fn.3+0: int64, fn.3+1: int64]: [int64]
                ): func (int64) -> [int64],
                6: int64,
                7: int64,
            ): [int64],
            (
                call external.0: func (int64) -> int64,
                6: int64,
                2: int64,
            ): int64,
        }: {[float64], [int64], int64}
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
    let externals = [std::array::map];

    let main =
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
            var.1: func (int64) -> {int64, int64},
            [2: int64, 3: int64, 1: int64]: [int64]
        ): [{int64, int64}]
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
    let externals = [std::array::map, std::int::mul];

    let main =
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
            var.2: func ({int64, int64}) -> int64,
            var.1: [{int64, int64}],
        ): [int64]
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
