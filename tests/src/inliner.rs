use insta::assert_snapshot;

#[track_caller]
fn _test_compile_and_print(source: &str) -> String {
    let program = lutra_compiler::_test_compile_main(source).unwrap_or_else(|e| panic!("{e}"));
    let program = lutra_compiler::inline(program);
    lutra_bin::ir::print_no_color(&program)
}

#[test]
fn inline_00() {
    assert_snapshot!(_test_compile_and_print(r#"
    func twice(x: T) where T -> {x, x}

    func main() -> twice([true, true, false])
    "#), @"
    let main = (func 1 ->
      let 2 = [
        true: bool,
        true: bool,
        false: bool,
      ]: [bool];
      (tuple
        var.2: [bool],
        var.2: [bool],
      ): {x: [bool], x: [bool]}
    ): func ({}) -> {x: [bool], x: [bool]}
    ")
}

#[test]
fn inline_01() {
    assert_snapshot!(_test_compile_and_print(r#"
    func once(x: T) where T -> {x}

    func main() -> once([true, true, false])
    "#), @"
    let main = (func 1 ->
      (tuple
        [
          true: bool,
          true: bool,
          false: bool,
        ]: [bool],
      ): {x: [bool]}
    ): func ({}) -> {x: [bool]}
    ")
}

#[test]
fn inline_02() {
    assert_snapshot!(_test_compile_and_print(r#"
    const my_rel: [{int64, int64}] = [{5,3},{65,1},{3, 2}]
    func main() -> (
      my_rel
      | std::aggregate(func (x: {[int64], [int64]}) -> {std::min(x.0), std::min(x.1)})
    )
    "#), @"
    let main = (func 2 ->
      let 5 = (call
        external.std::to_columnar: func ([{int64, int64}]) -> {[int64], [int64]},
        [
          (tuple
            5: int64,
            3: int64,
          ): {int64, int64},
          (tuple
            65: int64,
            1: int64,
          ): {int64, int64},
          (tuple
            3: int64,
            2: int64,
          ): {int64, int64},
        ]: [{int64, int64}],
      ): {[int64], [int64]};
      (tuple
        (call
          external.std::min: func ([int64]) -> enum {none, some: int64},
          (tuple_lookup
            var.5: {[int64], [int64]}
            0
          ): [int64],
        ): enum {none, some: int64},
        (call
          external.std::min: func ([int64]) -> enum {none, some: int64},
          (tuple_lookup
            var.5: {[int64], [int64]}
            1
          ): [int64],
        ): enum {none, some: int64},
      ): {enum {none, some: int64}, enum {none, some: int64}}
    ): func ({}) -> {enum {none, some: int64}, enum {none, some: int64}}
    ")
}

#[test]
fn inline_03() {
    assert_snapshot!(_test_compile_and_print(r#"
    type OptText: enum {
      none,
      some: text,
    }
    func main() -> {
      .some("hello"): OptText,
      .none: OptText,
      std::option::is_some(.some("hello")),
      std::option::is_none(.some("hello")),
    }
    "#), @r#"
    type OptText = enum {none, some: text};
    let main = (func 1 ->
      (tuple
        (enum_variant 1
          "hello": text
        ): OptText,
        (enum_variant 0): OptText,
        (enum_eq
          (enum_variant 1
            "hello": text
          ): enum {none, some: text}
          1
        ): bool,
        (call
          external.std::not: func (bool) -> bool,
          (enum_eq
            (enum_variant 1
              "hello": text
            ): enum {none, some: text}
            1
          ): bool,
        ): bool,
      ): {OptText, OptText, bool, bool}
    ): func ({}) -> {OptText, OptText, bool, bool}
    "#)
}
