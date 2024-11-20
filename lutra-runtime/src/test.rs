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
    let externals = [core_int_add];

    let main =
        let 1 = (func 2 -> [fn.2+0: float, fn.2+0: float, fn.2+0: float]: [float]): func (float) -> [float];
        let 2 = var.1: func (float) -> [float];
        {
            (
                call var.2: func (float) -> [float],
                3.5: float
            ): [float],
            (
                call (
                    func 3 -> [fn.3+0: int, fn.3+1: int]: [int]
                ): func (int) -> [int],
                6: int,
                7: int,
            ): [int],
            (
                call external.0: func (int) -> int,
                6: int,
                2: int,
            ): int,
        }: {[float], [int], int}
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
    let externals = [core_array_map];

    let main =
        let 1 = (func 1 -> {fn.1+0: int, fn.1+0: int}: {int, int}): func (int) -> {int, int};
        (
            call external.0: func (func (int) -> {int, int}, [int]) -> [{int, int}],
            var.1: func (int) -> {int, int},
            [2: int, 3: int, 1: int]: [int]
        ): [{int, int}]
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
    let externals = [core_array_map, core_int_mul];

    let main =
        let 1 = [
            {1:int, 3:int}: {int, int}, 
            {5:int, 4:int}: {int, int}, 
            {2:int, 3:int}: {int, int},
        ]: [{int, int}];
        let 2 = (
            func 1 -> (
                call external.1: func (int, int) -> int,
                fn.1+0: {int, int} .0: int,
                fn.1+0: {int, int} .1: int,
            ): int
        ): func ({int, int}) -> int;
        (
            call external.0: func (func ({int, int}) -> int, [{int, int}]) -> [int],
            var.2: func ({int, int}) -> int,
            var.1: [{int, int}],
        ): [int]
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
