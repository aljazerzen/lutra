#![cfg(test)]

use insta::assert_snapshot;

#[test]
fn print_01() {
    assert_snapshot!(super::_test_print(r#"
    let externals = [
      std_int_add,
    ];
    
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
        (call
            (
                func 3 -> [
                    fn.3+0: int64,
                    fn.3+1: int64,
                ]: [int64]
            ): func (int64, int64) -> [int64],
            6: int64,
            7: int64,
        ): [int64],
        (call
            external.0: func (int64, int64) -> int64,
            6: int64,
            2: int64
        ): int64
      }: {[float64], [int64], int64}
      .1:[int64]
      .[0]:int64
    "#), @r#"
    let externals = [
      std_int_add,
    ];

    let main =
      let 1 = (
        func 2 -> [
          fn.2+0: float64,
          fn.2+0: float64,
          fn.2+0: float64,
        ]: [float64]
      ): func (float64) -> [float64];
      let 2 = var.1: func (float64) -> [float64];
      {
        (
          call var.2: func (float64) -> [float64], 
          3.5: float64, 
        ): [float64],
        (
          call (
            func 3 -> [
              fn.3+0: int64,
              fn.3+1: int64,
            ]: [int64]
          ): func (int64, int64) -> [int64], 
          6: int64, 
          7: int64, 
        ): [int64],
        (
          call external.0: func (int64, int64) -> int64, 
          6: int64, 
          2: int64, 
        ): int64,
      }: {[float64], [int64], int64}
      .1: [int64]
      .[0]: int64: int64
    "#);
}
