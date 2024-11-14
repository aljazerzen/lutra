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
            fn.2+0: float,
            fn.2+0: float,
            fn.2+0: float
        ]: [float]
      ): func (float) -> [float];
    
      let 2 = var.1: func (float) -> [float];
      {
        (
            call var.2: func (float) -> [float],
            3.5: float
        ): [float],
        (call
            (
                func 3 -> [
                    fn.3+0: int,
                    fn.3+1: int,
                ]: [int]
            ): func (int, int) -> [int],
            6: int,
            7: int,
        ): [int],
        (call
            external.0: func (int, int) -> int,
            6: int,
            2: int
        ): int
      }: {[float], [int], int}
      .1:[int]
      .[0]:int
    "#), @r#"
    let externals = [
      std_int_add,
    ];

    let main =
      let 1 = (
        func 2 -> [
          fn.2+0: float,
          fn.2+0: float,
          fn.2+0: float,
        ]: [float]
      ): func (float) -> [float];
      let 2 = var.1: func (float) -> [float];
      {
        (
          call var.2: func (float) -> [float], 
          3.5: float, 
        ): [float],
        (
          call (
            func 3 -> [
              fn.3+0: int,
              fn.3+1: int,
            ]: [int]
          ): func (int, int) -> [int], 
          6: int, 
          7: int, 
        ): [int],
        (
          call external.0: func (int, int) -> int, 
          6: int, 
          2: int, 
        ): int,
      }: {[float], [int], int}
      .1: [int]
      .[0]: int: int
    "#);
}
