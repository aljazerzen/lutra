#[track_caller]
fn _test_run(source: &str) -> String {
    env_logger::init();

    let program = lutra_frontend::_test_compile(source);
    let output_ty = program.get_output_ty();

    lutra_ir::print_ty(output_ty)
}

#[test]
fn types_01() {
    insta::assert_snapshot!(
        _test_run(
            "func () -> 4"
        ),
        @"int64"
    );
}
#[test]
fn types_02() {
    insta::assert_snapshot!(_test_run(r#"
        let identity = func <T> (x: T) -> x

        func () -> identity(4)
    "#), @"int64");
}
#[test]
fn types_02a() {
    insta::assert_snapshot!(_test_run(r#"
        let identity: func <T> (x: T): T

        func () -> identity(4)
    "#), @"int64");
}
#[test]
fn types_03() {
    insta::assert_snapshot!(_test_run(r#"
        let identity = func <T> (x: T) -> x
        let apply = func <I, O> (x: I, mapper: func (I): O): O -> mapper(x)

        func () -> apply(5, identity)
    "#), @"int64");
}
#[test]
fn types_04() {
    insta::assert_snapshot!(_test_run(r#"
        let twice = func <T> (x: T) -> {x, x}
        let apply = func <I, O> (x: I, mapper: func (I): O): O -> mapper(x)

        func () -> apply(5, twice)
    "#), @"{int64, int64}");
}
#[test]
fn types_06() {
    insta::assert_snapshot!(_test_run(r#"
        let identity = func <T> (x: T) -> x
        let map: func <I, O> (x: [I], mapper: func (I): O): [O]

        func () -> map([5, 4, 1], identity)
    "#), @"[int64]");
}
#[test]
fn types_07() {
    insta::assert_snapshot!(_test_run(r#"
        let twice = func <T> (x: T) -> {x, x}
        let map: func <I, O> (x: [I], mapper: func (I): O): [O]

        func () -> map([5, 4, 1], twice)
    "#), @"[{int64, int64}]");
}
#[test]
fn types_08() {
    insta::assert_snapshot!(_test_run(r#"
        let filter: func <T> (array: [T], condition: func (T): bool): [T]

        func () -> filter(
            [{5, false}, {4, true}, {1, true}],
            func (x: {int64, bool}) -> x.0 < 3 && x.1
        )
    "#), @"[{int64, bool}]");
}
