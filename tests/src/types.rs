#[track_caller]
fn _test_run(source: &str) -> String {
    env_logger::Builder::new()
        .filter_level(log::LevelFilter::Debug)
        .format_timestamp(None)
        .try_init()
        .ok();

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

    env_logger::Builder::new()
        .filter_level(log::LevelFilter::Debug)
        .format_timestamp(None)
        .try_init()
        .ok();

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
#[test]
fn types_11() {
    insta::assert_snapshot!(_test_run(r#"
        type album_sale = {id = int, total = float}
        let get_album_sales: func (): [album_sale]

        let filter: func <T> (array: [T], condition: func (T): bool): [T]

        func (): [album_sale] -> (
            get_album_sales()
            | std::filter(func (this: album_sale) -> this.id == 6)
        )
    "#), @"[album_sale]");
}
#[test]
fn types_12() {
    insta::assert_snapshot!(_test_run(r#"
        let a = {id = 4, total = 4.5}
        func () -> a.total
    "#), @"float64");
}
#[test]
fn types_13() {
    insta::assert_snapshot!(_test_run(r#"
        let floor: func <T: float32 | float64> (x: T): T
        func () -> floor(2.4)
    "#), @"float64");

    insta::assert_snapshot!(_test_err(r#"
        let floor: func <T: float32 | float64> (x: T): T
        func () -> floor(false)
    "#), @"T is restricted to one of float32, float64, found Ty { kind: Primitive(bool), span: Some(1:83-88), name: None, layout: Some(TyLayout { head_size: 8, body_ptrs: [], variants_recursive: [] }) }");
}
#[test]
fn types_14() {
    insta::assert_snapshot!(_test_run(r#"
        let identity: func (x: float64): float64
        let floor = func <T: float32 | float64> (x: T): T -> (
            identity(x)
        )
        func () -> floor(2.3)
    "#), @"float64");

    insta::assert_snapshot!(_test_err(r#"
        let identity: func (x: bool): bool
        let floor = func <T: float32 | float64> (x: T): T -> (
            identity(x)
        )
        func () -> floor(2.3)
    "#), @"T is restricted to one of float32, float64, found Ty { kind: Primitive(bool), span: Some(1:32-36), name: None, layout: Some(TyLayout { head_size: 8, body_ptrs: [], variants_recursive: [] }) }");
}
#[test]
fn types_15() {
    // tuple domain with named arg
    insta::assert_snapshot!(_test_run(r#"
        let get_b: func <T: {b = int64, ..}> (x: T): T
        func () -> get_b({a = false, b = 4})
    "#), @"{a = bool, b = int64}");

    insta::assert_snapshot!(_test_err(r#"
        let get_b: func <T: {b = int64, ..}> (x: T): T
        func () -> get_b({a = false, b = 4.6})
    "#), @"T.b is restricted to int64");
    insta::assert_snapshot!(_test_err(r#"
        let get_b: func <T: {b = int64, ..}> (x: T): T
        func () -> get_b({a = false, c = 4})
    "#), @"T is restricted to tuples with a field named `b`");
}
#[test]
fn types_16() {
    // tuple domain with positional arg
    insta::assert_snapshot!(_test_run(r#"
        let get_b: func <T: {bool, int64, ..}> (x: T): T
        func () -> get_b({a = false, 4, c = 5.7})
    "#), @"{a = bool, int64, c = float64}");

    insta::assert_snapshot!(_test_err(r#"
        let get_b: func <T: {bool, int64, ..}> (x: T): T
        func () -> get_b({a = 7, 4, c = 5.7})
    "#), @"T.0 is restricted to bool");

    insta::assert_snapshot!(_test_err(r#"
        let get_b: func <T: {bool, int64, a = bool, ..}> (x: T): T
        func () -> get_b({a = false})
    "#), @"T is restricted to tuples with at least 2 fields");
    insta::assert_snapshot!(_test_run(r#"
        let get_b: func <T: {bool, int64, a = bool, ..}> (x: T): T
        func () -> get_b({a = false, 4})
    "#), @"{a = bool, int64}");
}
