#![cfg(test)]

use insta::assert_snapshot;

#[track_caller]
pub fn _test_print(source: &str) -> String {
    let program = crate::parser::_test_parse(source);

    lutra_bin::ir::print_no_color(&program)
}

#[test]
fn print_01() {
    assert_snapshot!(_test_print(r#"
    type std::Float64 = Prim64;
    type std::Int64 = Prim64;
    type std::Text = [Prim8];
    type b::c = Text;
    let main = let 1 = (func 2 ->
      (array
        fn.2+0: Float64,
        fn.2+0: Float64,
        fn.2+0: Float64,
      ): [Float64]
    ): func (Float64) -> [Float64];
    let 2 = var.1: func (Float64) -> [Float64];
    (tuple_lookup
      (tuple
        (call
          var.2: func (Float64) -> [Float64],
          4615063718147915776: Float64,
        ): [Float64],
        (call
          (func 3 ->
            (array
              fn.3+0: Int64,
              fn.3+1: Int64,
            ): [Int64]
          ): func (Int64, Int64) -> [Int64],
          6: Int64,
          7: Int64,
        ): [Int64],
        (call
          external.std::int::add: func (Int64, Int64) -> Int64,
          6: Int64,
          2: Int64,
        ): Int64,
      ): {[Float64], [Int64], Int64}
      1
    ): [Int64]
    "#), @"
    type std::Float64 = Prim64;
    type std::Int64 = Prim64;
    type std::Text = [Prim8];
    type b::c = Text;
    let main = let 1 = (func 2 ->
      (array
        fn.2+0: Float64,
        fn.2+0: Float64,
        fn.2+0: Float64,
      ): [Float64]
    ): func (Float64) -> [Float64];
    let 2 = var.1: func (Float64) -> [Float64];
    (tuple_lookup
      (tuple
        (call
          var.2: func (Float64) -> [Float64],
          4615063718147915776: Float64,
        ): [Float64],
        (call
          (func 3 ->
            (array
              fn.3+0: Int64,
              fn.3+1: Int64,
            ): [Int64]
          ): func (Int64, Int64) -> [Int64],
          6: Int64,
          7: Int64,
        ): [Int64],
        (call
          external.std::int::add: func (Int64, Int64) -> Int64,
          6: Int64,
          2: Int64,
        ): Int64,
      ): {[Float64], [Int64], Int64}
      1
    ): [Int64]
    ");
}
