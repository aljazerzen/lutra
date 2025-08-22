use insta::assert_snapshot;

#[track_caller]
fn _test_compile_and_print(source: &str) -> String {
    let program = lutra_compiler::_test_compile_main(source).unwrap_or_else(|e| panic!("{e}"));
    let program = lutra_compiler::inline(program);
    lutra_bin::ir::print(&program)
}

#[test]
fn inline_00() {
    assert_snapshot!(_test_compile_and_print(r#"
    func twice(x: T) where T -> {x, x}

    func main() -> twice([true, true, false])
    "#), @r"
    let main = (func 1 ->
      let 1 = [
        true: bool,
        true: bool,
        false: bool,
      ]: [bool];
      {
        var.1: [bool],
        var.1: [bool],
      }: {[bool], [bool]}
    ): func ({}) -> {[bool], [bool]}
    ")
}

#[test]
fn inline_01() {
    assert_snapshot!(_test_compile_and_print(r#"
    func once(x: T) where T -> {x}

    func main() -> once([true, true, false])
    "#), @r"
    let main = (func 1 ->
      {
        [
          true: bool,
          true: bool,
          false: bool,
        ]: [bool],
      }: {[bool]}
    ): func ({}) -> {[bool]}
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
    "#), @r"
    let main = (func 2 ->
      let 1 = [
        {
          5: int64,
          3: int64,
        }: {int64, int64},
        {
          65: int64,
          1: int64,
        }: {int64, int64},
        {
          3: int64,
          2: int64,
        }: {int64, int64},
      ]: [{int64, int64}];
      let 2 = var.1: [{int64, int64}];
      let 4 = (call
        external.std::to_columnar: func ([{int64, int64}]) -> {[int64], [int64]},
        var.2: [{int64, int64}],
      ): {[int64], [int64]};
      {
        (call
          external.std::min: func ([int64]) -> int64,
          (tuple_lookup
            var.4: {[int64], [int64]}
            0
          ): [int64],
        ): int64,
        (call
          external.std::min: func ([int64]) -> int64,
          (tuple_lookup
            var.4: {[int64], [int64]}
            1
          ): [int64],
        ): int64,
      }: {int64, int64}
    ): func ({}) -> {int64, int64}
    ")
}

#[test]
fn inline_03() {
    assert_snapshot!(_test_compile_and_print(r#"
    type OptText: enum {
      None,
      Some: text,
    }
    func main() -> {
      OptText::Some("hello"),
      OptText::None,
      std::is_some(OptText::Some("hello")),
      std::is_none(OptText::Some("hello")),
    }
    "#), @r#"
    type OptText = enum {None, Some: text};
    let main = (func 1 ->
      {
        (enum_variant 1
          "hello": text
        ): OptText,
        (enum_variant 0): OptText,
        let 2 = (enum_variant 1
          "hello": text
        ): OptText;
        (
          switch,
          (
            (enum_eq
              var.2: enum {None, Some: text}
              1
            ): bool,
            true: bool,
          ),
          (
            (enum_eq
              var.2: enum {None, Some: text}
              0
            ): bool,
            false: bool,
          ),
        ): bool,
        let 3 = (enum_variant 1
          "hello": text
        ): OptText;
        (
          switch,
          (
            (enum_eq
              var.3: enum {None, Some: text}
              1
            ): bool,
            false: bool,
          ),
          (
            (enum_eq
              var.3: enum {None, Some: text}
              0
            ): bool,
            true: bool,
          ),
        ): bool,
      }: {OptText, OptText, bool, bool}
    ): func ({}) -> {OptText, OptText, bool, bool}
    "#)
}
