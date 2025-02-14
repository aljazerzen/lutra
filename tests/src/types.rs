#[track_caller]
fn _test_run(source: &str) -> String {
    env_logger::init();

    let program = match lutra_frontend::_test_compile(source) {
        Ok(program) => program,
        Err(e) => panic!("{e}"),
    };
    let output_ty = program.get_output_ty();

    lutra_ir::print_ty(output_ty)
}

#[track_caller]
fn _test_err(source: &str) -> String {
    use lutra_frontend::error::Error;

    env_logger::init();

    let Error::Compile { diagnostics } = lutra_frontend::_test_compile(source).unwrap_err() else {
        unreachable!()
    };
    let diagnostic = diagnostics.into_iter().next().unwrap();
    diagnostic.message().to_string()
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
        let map: func <I, O> (x: [I], mapper: func (I): O): [O]
        let slice: func <T> (x: [T], start: int64, end: int64): [T]

        func () -> (
            [{5, false}, {4, true}, {1, true}]
            | filter(func (x: {int64, bool}) -> x.1)
            | map(func (x: {int64, bool}) -> x.0)
            | slice(0, 1)
        )
    "#), @"[int64]");
}
#[test]
fn types_09() {
    insta::assert_snapshot!(_test_err(r#"
        let peek: func <T> (array: [T], condition: func <R> (T): R): [T]
    "#), @"generic type parameters are not allowed here");
}
#[test]
fn types_10() {
    insta::assert_snapshot!(_test_run(r#"
        func () -> 3..5
    "#), @"{start = int64, end = int64}");
}
