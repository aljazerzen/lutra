use insta::assert_snapshot;

#[track_caller]
fn _test_compile_and_print(source: &str) -> String {
    let program = lutra_compiler::_test_compile_main(source).unwrap_or_else(|e| panic!("{e}"));
    let program = lutra_compiler::_test_inline(program);
    lutra_bin::ir::print_no_color(&program)
}

#[test]
fn inline_00() {
    assert_snapshot!(_test_compile_and_print(r#"
    func twice(x: T) where T -> {x, x}

    func main() -> twice([true, true, false])
    "#), @"
    type std::Bool = Prim8;
    let main = (func 1 ->
      let 2 = (array
        1: Bool,
        1: Bool,
        0: Bool,
      ): [Bool];
      (tuple
        var.2: [Bool],
        var.2: [Bool],
      ): {x: [Bool], x: [Bool]}
    ): func ({}) -> {x: [Bool], x: [Bool]}
    ")
}

#[test]
fn inline_01() {
    assert_snapshot!(_test_compile_and_print(r#"
    func once(x: T) where T -> {x}

    func main() -> once([true, true, false])
    "#), @"
    type std::Bool = Prim8;
    let main = (func 1 ->
      (tuple
        (array
          1: Bool,
          1: Bool,
          0: Bool,
        ): [Bool],
      ): {x: [Bool]}
    ): func ({}) -> {x: [Bool]}
    ")
}

#[test]
fn inline_02() {
    assert_snapshot!(_test_compile_and_print(r#"
    const my_rel: [{Int64, Int64}] = [{5,3},{65,1},{3, 2}]
    func main() -> (
      my_rel
      | std::aggregate(func (x: {[Int64], [Int64]}) -> {std::min(x.0), std::min(x.1)})
    )
    "#), @"
    type std::Int64 = Prim64;
    let main = (func 2 ->
      let 5 = (call
        external.std::array::to_columnar: func ([{Int64, Int64}]) -> {[Int64], [Int64]},
        (array
          (tuple
            5: Int64,
            3: Int64,
          ): {Int64, Int64},
          (tuple
            65: Int64,
            1: Int64,
          ): {Int64, Int64},
          (tuple
            3: Int64,
            2: Int64,
          ): {Int64, Int64},
        ): [{Int64, Int64}],
      ): {[Int64], [Int64]};
      (tuple
        (call
          external.std::array::min: func ([Int64]) -> enum {none, some: Int64},
          (tuple_lookup
            var.5: {[Int64], [Int64]}
            0
          ): [Int64],
        ): enum {none, some: Int64},
        (call
          external.std::array::min: func ([Int64]) -> enum {none, some: Int64},
          (tuple_lookup
            var.5: {[Int64], [Int64]}
            1
          ): [Int64],
        ): enum {none, some: Int64},
      ): {enum {none, some: Int64}, enum {none, some: Int64}}
    ): func ({}) -> {enum {none, some: Int64}, enum {none, some: Int64}}
    ")
}

#[test]
fn inline_03() {
    assert_snapshot!(_test_compile_and_print(r#"
    type OptText: enum {
      none,
      some: Text,
    }
    func main() -> {
      .some("hello"): OptText,
      .none: OptText,
      std::option::is_some(.some("hello")),
      std::option::is_none(.some("hello")),
    }
    "#), @r#"
    type std::Bool = Prim8;
    type std::Text = [Prim8];
    type OptText = enum {none, some: Text};
    let main = (func 1 ->
      (tuple
        (enum_variant 1
          "hello": Text
        ): OptText,
        (enum_variant 0): OptText,
        (call
          external.std::ops::eq: func (Prim8, Prim8) -> Bool,
          (enum_tag
            (enum_variant 1
              "hello": Text
            ): enum {none, some: Text}
          ): Prim8,
          1: Prim8,
        ): Bool,
        (call
          external.std::ops::not: func (Bool) -> Bool,
          (call
            external.std::ops::eq: func (Prim8, Prim8) -> Bool,
            (enum_tag
              (enum_variant 1
                "hello": Text
              ): enum {none, some: Text}
            ): Prim8,
            1: Prim8,
          ): Bool,
        ): Bool,
      ): {OptText, OptText, Bool, Bool}
    ): func ({}) -> {OptText, OptText, Bool, Bool}
    "#)
}
