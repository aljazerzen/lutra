use insta::assert_snapshot;

#[track_caller]
fn _test_compile_and_print(source: &str) -> String {
    let program = lutra_compiler::_test_compile(source).unwrap_or_else(|e| panic!("{e}"));
    let program = lutra_compiler::inline(program);
    lutra_bin::ir::print(&program)
}

#[test]
fn inline_00() {
    assert_snapshot!(_test_compile_and_print(r#"
    let twice = func <T> (x: T) -> {x, x}

    func () -> twice([true, true, false])
    "#), @r"
    let main = (
      func 0 -> 
        let 2 = [
          true: bool,
          true: bool,
          false: bool,
        ]: [bool];
        {
          var.2: [bool],
          var.2: [bool],
        }: {[bool], [bool]}
    ): func () -> {[bool], [bool]}
    ")
}

#[test]
fn inline_01() {
    assert_snapshot!(_test_compile_and_print(r#"
    let once = func <T> (x: T) -> {x}

    func () -> once([true, true, false])
    "#), @r"
    let main = (
      func 0 -> {
        [
          true: bool,
          true: bool,
          false: bool,
        ]: [bool],
      }: {[bool]}
    ): func () -> {[bool]}
    ")
}

#[test]
fn inline_02() {
    assert_snapshot!(_test_compile_and_print(r#"
    let my_rel: [{int64, int64}] = [{5,3},{65,1},{3, 2}]
    func () -> std::aggregate(my_rel, func (x: {[int64], [int64]}) -> {std::min(x.0), std::min(x.1)})
    "#), @r"
    let main = (
      func 0 -> 
        let 2 = [
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
        let 3 = var.2: [{int64, int64}];
        let 5 = (
          call external.std::to_columnar: func ([{int64, int64}]) -> {[int64], [int64]},
          var.3: [{int64, int64}],
        ): {[int64], [int64]};
        {
          (
            call external.std::min: func ([int64]) -> int64,
            var.5: {[int64], [int64]}
            .0: [int64],
          ): int64,
          (
            call external.std::min: func ([int64]) -> int64,
            var.5: {[int64], [int64]}
            .1: [int64],
          ): int64,
        }: {int64, int64}
    ): func () -> {int64, int64}
    ")
}
