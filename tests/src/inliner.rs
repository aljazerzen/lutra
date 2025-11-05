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
      let 2 = [
        true[90m: bool[0m,
        true[90m: bool[0m,
        false[90m: bool[0m,
      ][90m: [bool][0m;
      (tuple
        var.2[90m: [bool][0m,
        var.2[90m: [bool][0m,
      )[90m: {[bool], [bool]}[0m
    )[90m: func ({}) -> {[bool], [bool]}[0m
    ")
}

#[test]
fn inline_01() {
    assert_snapshot!(_test_compile_and_print(r#"
    func once(x: T) where T -> {x}

    func main() -> once([true, true, false])
    "#), @r"
    let main = (func 1 ->
      (tuple
        [
          true[90m: bool[0m,
          true[90m: bool[0m,
          false[90m: bool[0m,
        ][90m: [bool][0m,
      )[90m: {[bool]}[0m
    )[90m: func ({}) -> {[bool]}[0m
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
        (tuple
          5[90m: int64[0m,
          3[90m: int64[0m,
        )[90m: {int64, int64}[0m,
        (tuple
          65[90m: int64[0m,
          1[90m: int64[0m,
        )[90m: {int64, int64}[0m,
        (tuple
          3[90m: int64[0m,
          2[90m: int64[0m,
        )[90m: {int64, int64}[0m,
      ][90m: [{int64, int64}][0m;
      let 3 = var.1[90m: [{int64, int64}][0m;
      let 5 = (call
        external.std::to_columnar[90m: func ([{int64, int64}]) -> {[int64], [int64]}[0m,
        var.3[90m: [{int64, int64}][0m,
      )[90m: {[int64], [int64]}[0m;
      (tuple
        (call
          external.std::min[90m: func ([int64]) -> enum {None, Some: int64}[0m,
          (tuple_lookup
            var.5[90m: {[int64], [int64]}[0m
            0
          )[90m: [int64][0m,
        )[90m: enum {None, Some: int64}[0m,
        (call
          external.std::min[90m: func ([int64]) -> enum {None, Some: int64}[0m,
          (tuple_lookup
            var.5[90m: {[int64], [int64]}[0m
            1
          )[90m: [int64][0m,
        )[90m: enum {None, Some: int64}[0m,
      )[90m: {enum {None, Some: int64}, enum {None, Some: int64}}[0m
    )[90m: func ({}) -> {enum {None, Some: int64}, enum {None, Some: int64}}[0m
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
      (tuple
        (enum_variant 1
          "hello"[90m: text[0m
        )[90m: OptText[0m,
        (enum_variant 0)[90m: OptText[0m,
        let 2 = (enum_variant 1
          "hello"[90m: text[0m
        )[90m: OptText[0m;
        (
          switch,
          (
            (enum_eq
              var.2[90m: enum {None, Some: text}[0m
              1
            )[90m: bool[0m,
            true[90m: bool[0m,
          ),
          (
            (enum_eq
              var.2[90m: enum {None, Some: text}[0m
              0
            )[90m: bool[0m,
            false[90m: bool[0m,
          ),
        )[90m: bool[0m,
        let 3 = (enum_variant 1
          "hello"[90m: text[0m
        )[90m: OptText[0m;
        (
          switch,
          (
            (enum_eq
              var.3[90m: enum {None, Some: text}[0m
              1
            )[90m: bool[0m,
            false[90m: bool[0m,
          ),
          (
            (enum_eq
              var.3[90m: enum {None, Some: text}[0m
              0
            )[90m: bool[0m,
            true[90m: bool[0m,
          ),
        )[90m: bool[0m,
      )[90m: {OptText, OptText, bool, bool}[0m
    )[90m: func ({}) -> {OptText, OptText, bool, bool}[0m
    "#)
}
