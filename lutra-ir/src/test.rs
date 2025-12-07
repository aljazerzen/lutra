#![cfg(test)]

use insta::assert_snapshot;

#[track_caller]
pub fn _test_print(source: &str) -> String {
    let program = crate::parser::_test_parse(source);

    lutra_bin::ir::print(&program)
}

#[test]
fn print_01() {
    assert_snapshot!(_test_print(r#"
    let main = (func 0 ->
      let 1 =
        let 0 = (func 2 ->
          [
            fn.2+0: float64,
            fn.2+0: float64,
            fn.2+0: float64
          ]: [float64]
        ): func (float64) -> [float64];
        var.0: func (float64) -> [float64];

      let 2 = var.1: func (float64) -> [float64];
      (tuple_lookup
        {
          (call
            var.2: func (float64) -> [float64],
            3.5: float64
          ): [float64],
          (call
            (func 3 ->
              [
                fn.3+0: int64,
                fn.3+1: int64,
              ]: [int64]
            ): func (int64, int64) -> [int64],
            6: int64,
            7: int64,
          ): [int64],
          (call
            external.std::int::add: func (int64, int64) -> int64,
            6: int64,
            2: int64
          ): int64
        }: {[float64], [int64], int64}
        1
      ): [int64]
    ): func () -> [int64]
    "#), @r"
    let main = (func 0 ->
      let 1 =
        let 0 = (func 2 ->
          [
            fn.2+0[90m: float64[0m,
            fn.2+0[90m: float64[0m,
            fn.2+0[90m: float64[0m,
          ][90m: [float64][0m
        )[90m: func (float64) -> [float64][0m;
        var.0[90m: func (float64) -> [float64][0m;
      let 2 = var.1[90m: func (float64) -> [float64][0m;
      (tuple_lookup
        (tuple
          (call
            var.2[90m: func (float64) -> [float64][0m,
            3.5[90m: float64[0m,
          )[90m: [float64][0m,
          (call
            (func 3 ->
              [
                fn.3+0[90m: int64[0m,
                fn.3+1[90m: int64[0m,
              ][90m: [int64][0m
            )[90m: func (int64, int64) -> [int64][0m,
            6[90m: int64[0m,
            7[90m: int64[0m,
          )[90m: [int64][0m,
          (call
            external.std::int::add[90m: func (int64, int64) -> int64[0m,
            6[90m: int64[0m,
            2[90m: int64[0m,
          )[90m: int64[0m,
        )[90m: {[float64], [int64], int64}[0m
        1
      )[90m: [int64][0m
    )[90m: func () -> [int64][0m
    ");
}
