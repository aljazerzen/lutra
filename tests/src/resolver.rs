#[track_caller]
fn _test_run(source: &str) -> String {
    crate::init_logger();

    let program = match lutra_compiler::_test_compile(source) {
        Ok(program) => program,
        Err(e) => panic!("{e}"),
    };
    let output_ty = program.get_output_ty();

    lutra_bin::ir::print_ty(output_ty)
}

#[track_caller]
fn _test_err(source: &str) -> String {
    use lutra_compiler::error::Error;

    crate::init_logger();

    let res = lutra_compiler::_test_compile(source);

    let diagnostics = match res {
        Ok(program) => {
            tracing::error!("ir: {program:#?}");
            panic!("expected compilation to fail, but it succedded");
        }
        Err(Error::Compile { diagnostics }) => diagnostics,
        Err(_) => unreachable!(),
    };
    let diagnostic = diagnostics.into_iter().next().unwrap();
    diagnostic.display().to_string()
}

#[test]
fn types_01() {
    // type of a literal

    insta::assert_snapshot!(
        _test_run(
            "func () -> 4"
        ),
        @"int64"
    );
}
#[test]
fn types_02() {
    // inference of type arg

    insta::assert_snapshot!(_test_run(r#"
        let identity = func <T> (x: T) -> x

        func () -> identity(4)
    "#), @"int64");

    // same, but describe function as a type, not an expression
    insta::assert_snapshot!(_test_run(r#"
        let identity: func <T> (x: T): T

        func () -> identity(4)
    "#), @"int64");
}
#[test]
fn types_03() {
    // validation of type params

    insta::assert_snapshot!(_test_err(r#"
        let floor_64: func (x: float64): float64

        let floor = func <T> (x: T) -> floor_64(x)

        func () -> floor(4.4)
    "#), @r"
    [E0004] Error: 
       ╭─[:4:49]
       │
     4 │         let floor = func <T> (x: T) -> floor_64(x)
       │                                                 ┬  
       │                                                 ╰── function floor_64, one of the params expected type `float64`, but found type `T`
    ───╯
    ");
}
#[test]
fn types_05() {
    insta::assert_snapshot!(_test_run(r#"
        let identity = func <T> (x: T) -> x
        let apply = func <I, O> (x: I, mapper: func (I): O): O -> mapper(x)

        func () -> apply(5, identity)
    "#), @"int64");

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
    "#), @r"
    Error: 
       ╭─[:2:58]
       │
     2 │         let peek: func <T> (array: [T], condition: func <R> (T): R): [T]
       │                                                          ┬  
       │                                                          ╰── generic type parameters are not allowed here
    ───╯
    ");
}
#[test]
fn types_10() {
    // range

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
    // tuple indirection

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
    "#), @r"
    [E0005] Error: 
       ╭─[:3:20]
       │
     3 │         func () -> floor(false)
       │                    ──────┬─────  
       │                          ╰─────── T is restricted to one of float32, float64, found bool
    ───╯
    ");
}
#[test]
fn types_14() {
    // validate type params: one of

    insta::assert_snapshot!(_test_err(r#"
        let floor_64: func (x: float64): float64
        let floor = func <T: float32 | float64> (x: T): T -> (
            floor_64(x)
        )
        func () -> floor(2.3)
    "#), @r"
    [E0004] Error: 
       ╭─[:4:22]
       │
     4 │             floor_64(x)
       │                      ┬  
       │                      ╰── function floor_64, one of the params expected type `float64`, but found type `T`
    ───╯
    ");

    insta::assert_snapshot!(_test_run(r#"
        let floor: func <F: float32 | float64> (x: F): F
        let twice_floored = func <T: float32 | float64> (x: T) -> {floor(4.5), floor(x)}
        func (f: float32) -> twice_floored(f)
    "#), @"{float64, float32}");

    insta::assert_snapshot!(_test_run(r#"
        let floor: func <F: float32 | float64> (x: F): F
        let twice_floored = func <T: float64> (x: T) -> {floor(x), floor(x)}
        func () -> twice_floored(2.3)
    "#), @"{float64, float64}");

    insta::assert_snapshot!(_test_err(r#"
        let floor: func <F: float32 | float64> (x: F): F
        let twice_floored = func <T: float64 | bool> (x: T) -> {floor(x), floor(x)}
        func () -> twice_floored(2.3)
    "#), @r"
    [E0005] Error: 
       ╭─[:3:64]
       │
     3 │         let twice_floored = func <T: float64 | bool> (x: T) -> {floor(x), floor(x)}
       │                                                                ──────────┬─────────  
       │                                                                          ╰─────────── T is restricted to one of float32, float64, found bool
    ───╯
    ");
}
#[test]
fn types_15() {
    // type param: tuple domain with named arg
    insta::assert_snapshot!(_test_run(r#"
        let get_b: func <T: {b = int64, ..}> (x: T): T
        func () -> get_b({a = false, b = 4})
    "#), @"{a = bool, b = int64}");

    insta::assert_snapshot!(_test_err(r#"
        let get_b: func <T: {b = int64, ..}> (x: T): T
        func () -> get_b({a = false, b = 4.6})
    "#), @r"
    [E0004] Error: 
       ╭─[:3:42]
       │
     3 │         func () -> get_b({a = false, b = 4.6})
       │                                          ─┬─  
       │                                           ╰─── expected type `int64`, but found type `float64`
    ───╯
    ");
    insta::assert_snapshot!(_test_err(r#"
        let get_b: func <T: {b = int64, ..}> (x: T): T
        func () -> get_b({a = false, c = 4})
    "#), @r"
    [E0005] Error: 
       ╭─[:3:20]
       │
     3 │         func () -> get_b({a = false, c = 4})
       │                    ────────────┬────────────  
       │                                ╰────────────── T is restricted to tuples with a field named `b`
    ───╯
    ");
}
#[test]
fn types_16() {
    // type param: tuple domain with positional arg
    insta::assert_snapshot!(_test_run(r#"
        let get_b: func <T: {bool, int64, ..}> (x: T): T
        func () -> get_b({a = false, 4, c = 5.7})
    "#), @"{a = bool, int64, c = float64}");

    insta::assert_snapshot!(_test_err(r#"
        let get_b: func <T: {bool, int64, ..}> (x: T): T
        func () -> get_b({a = 7, 4, c = 5.7})
    "#), @r"
    [E0004] Error: 
       ╭─[:3:31]
       │
     3 │         func () -> get_b({a = 7, 4, c = 5.7})
       │                               ┬  
       │                               ╰── expected type `bool`, but found type `int64`
    ───╯
    ");

    insta::assert_snapshot!(_test_err(r#"
        let get_b: func <T: {bool, int64, a = bool, ..}> (x: T): T
        func () -> get_b({a = false})
    "#), @r"
    [E0005] Error: 
       ╭─[:3:20]
       │
     3 │         func () -> get_b({a = false})
       │                    ─────────┬────────  
       │                             ╰────────── T is restricted to tuples with at least 2 fields
    ───╯
    ");
    insta::assert_snapshot!(_test_run(r#"
        let get_b: func <T: {bool, int64, a = bool, ..}> (x: T): T
        func () -> get_b({a = false, 4})
    "#), @"{a = bool, int64}");
}

#[test]
fn types_17() {
    // validate type params: tuple domain

    insta::assert_snapshot!(_test_err(r#"
        let get_int: func (x: {int64}): int64
        let get = func <T: {int64, ..}> (x: T): T -> get_int(x)
        func () -> get({4})
    "#), @r"
    [E0004] Error: 
       ╭─[:3:62]
       │
     3 │         let get = func <T: {int64, ..}> (x: T): T -> get_int(x)
       │                                                              ┬  
       │                                                              ╰── function get_int, one of the params expected type `{int64}`, but found type `T`
    ───╯
    ");

    insta::assert_snapshot!(_test_err(r#"
        let get_int: func (x: {a = int64}): int64
        let get = func <T: {a = int64, ..}> (x: T): T -> get_int(x)
        func () -> get({4})
    "#), @r"
    [E0004] Error: 
       ╭─[:3:66]
       │
     3 │         let get = func <T: {a = int64, ..}> (x: T): T -> get_int(x)
       │                                                                  ┬  
       │                                                                  ╰── function get_int, one of the params expected type `{a = int64}`, but found type `T`
    ───╯
    ");

    insta::assert_snapshot!(_test_err(r#"
        let needs_two: func <I: {int64, bool, ..}> (x: I): int64
        let needs_one = func <T: {int64, ..}> (x: T): int64 -> needs_two(x)
        func () -> needs_one({4})
    "#), @r"
    [E0005] Error: 
       ╭─[:3:64]
       │
     3 │         let needs_one = func <T: {int64, ..}> (x: T): int64 -> needs_two(x)
       │                                                                ──────┬─────  
       │                                                                      ╰─────── I is restricted to tuples with at least 2 fields
    ───╯
    ");

    insta::assert_snapshot!(_test_run(r#"
        let needs_one: func <I: {int64, ..}> (x: I): I
        let needs_two = func <T: {int64, bool, ..}> (x: T): T -> needs_one(x)
        func () -> needs_two({4, false})
    "#), @"{int64, bool}");
}

#[test]
fn types_18() {
    // indirection into type params

    insta::assert_snapshot!(_test_run(r#"
        let f = func <T: {int64, ..}> (x: T) -> x.0
        func () -> f({4, false})
    "#), @"int64");

    insta::assert_snapshot!(_test_run(r#"
        let f = func <T: {a = int64, ..}> (x: T) -> x.a
        func () -> f({false, a = 4})
    "#), @"int64");
}

#[test]
fn empty_array_00() {
    insta::assert_snapshot!(
        _test_err(
            "func () -> []"
        ),
        @r"
    Error: 
       ╭─[:1:12]
       │
     1 │ func () -> []
       │            ─┬  
       │             ╰── cannot infer type
    ───╯
    "
    );
}

#[test]
fn empty_array_01() {
    insta::assert_snapshot!(
        _test_err(
            "func () -> std::lag([], 1)"
        ),
        @r"
    Error: 
       ╭─[:1:12]
       │
     1 │ func () -> std::lag([], 1)
       │            ────┬───  
       │                ╰───── cannot infer type of T
    ───╯
    "
    );
}

#[test]
fn empty_array_02() {
    insta::assert_snapshot!(
        _test_run(
            "func (): [int64] -> std::lag([], 1)"
        ),
        @"[int64]"
    );
}

#[test]
fn empty_array_03() {
    insta::assert_snapshot!(
        _test_err(
            "func () -> {false, [], true}"
        ),
        @r"
    Error: 
       ╭─[:1:20]
       │
     1 │ func () -> {false, [], true}
       │                    ─┬  
       │                     ╰── cannot infer type
    ───╯
    "
    );
}

#[test]
fn type_annotation_00() {
    insta::assert_snapshot!(
        _test_run(
            "func () -> 5: int64"
        ),
        @"int64"
    );
}

#[test]
fn type_annotation_01() {
    insta::assert_snapshot!(
        _test_err(
            "func () -> 5: text"
        ),
        @r"
    [E0004] Error: 
       ╭─[:1:12]
       │
     1 │ func () -> 5: text
       │            ┬  
       │            ╰── expected type `text`, but found type `int64`
    ───╯
    "
    );
}

#[test]
fn type_annotation_02() {
    insta::assert_snapshot!(
        _test_run(
            "func () -> []: [bool]"
        ),
        @"[bool]"
    );
}

#[test]
fn primitives() {
    insta::assert_snapshot!(
        _test_run(
            "
            let x: func (): {
                bool,
                int8,
                int16,
                int32,
                int64,
                uint8,
                uint16,
                uint32,
                uint64,
                float32,
                float64,
                text,
            }
            let main = func () -> x()
            "
        ),
        @"{bool, int8, int16, int32, int64, uint8, uint16, uint32, uint64, float32, float64, text}"
    );
}

#[test]
fn enums_00() {
    insta::assert_snapshot!(
        _test_run(
            "
            type Status = enum { Open, Done, Pending = text }

            let main = func () -> Status::Done
            "
        ),
        @"enum {Open, Done, Pending = text}"
    );
}

#[test]
fn enums_01() {
    insta::assert_snapshot!(
        _test_run(
            r#"
            type Status = enum { Open, Done, Pending = text }

            let main = func () -> Status::Pending("hello")
            "#
        ),
        @"enum {Open, Done, Pending = text}"
    );
}
#[test]
fn enums_02() {
    insta::assert_snapshot!(
        _test_run(
            r#"
            type X = { a = int64 }

            let main = func (): X::a -> 5
            "#
        ),
        @"int64"
    );
}
#[test]
fn enums_03() {
    insta::assert_snapshot!(
        _test_err(
            r#"
            type X = { a = int, b = X::a }
            "#
        ),
        @r"
    Error: 
       ╭─[:2:37]
       │
     2 │             type X = { a = int, b = X::a }
       │                                     ──┬─  
       │                                       ╰─── paths into self type are not allowed
    ───╯
    "
    );
}

#[test]
#[ignore] // TODO
fn recursive_00() {
    // This test is skipped, because it fails in layouter, where we cannot throw a proper error.
    // That's because we don't have span in IR and I don't want to add it.
    // IR is supposed to be a valid program, we should error out earlier.
    insta::assert_snapshot!(
        _test_err(
            r#"
            type Tree = {left = Tree, right = Tree}

            type OptionalTree = enum {
                None,
                Some = Tree,
            }
            let main = func (): OptionalTree -> OptionalTree::None
            "#
        ),
        @r#"
    Tree has infinite size.
    "#
    );
}

#[test]
fn match_00() {
    insta::assert_snapshot!(_test_run(r#"
        type Status = enum {Done, Pending = int16, Cancelled = text}

        let main = func () -> match Status::Done {
          Status::Done => "done",
          Status::Pending => "pending",
          Status::Cancelled => "cancelled",
        }
    "#), @"text");
}

#[test]
#[ignore] // TODO
fn match_01() {
    insta::assert_snapshot!(_test_err(r#"
        type Status = enum {Done, Pending = int16, Cancelled = text}

        let main = func () -> match Status::Done {
          Status::Done => "done",
          Status::Cancelled => "cancelled",
        }
    "#), @r#"
        Variant Status::Pending not covered.
    "#);
}

#[test]
fn match_02() {
    insta::assert_snapshot!(_test_err(r#"
        type Status = enum {Done, Pending = int16, Cancelled = text}
        type Color = enum {Red, Green, Blue}

        let main = func () -> match Status::Done {
          Color::Red => "red",
          Color::Green => "green",
          Color::Blue => "blue",
        }
    "#), @r#"
    [E0004] Error: 
       ╭─[:6:11]
       │
     6 │           Color::Red => "red",
       │           ─────┬────  
       │                ╰────── pattern expected type `Status`, but found type `Color`
       │ 
       │ Note: 
       │ type `Status` expands to `enum {Done, Pending = int16, Cancelled = text}`
       │ type `Color` expands to `enum {Red, Green, Blue}`
    ───╯
    "#);
}

#[test]
fn match_03() {
    insta::assert_snapshot!(_test_err(r#"
        type Color = enum {Red, Green, Blue}

        let main = func () -> match Color::Green {
          Color::Red => "red",
          Color::Green => 1,
          Color::Blue => false,
        }
    "#), @r"
    [E0004] Error: 
       ╭─[:6:27]
       │
     6 │           Color::Green => 1,
       │                           ┬  
       │                           ╰── match expected type `text`, but found type `int64`
    ───╯
    ");
}

#[test]
fn match_04() {
    insta::assert_snapshot!(_test_run(r#"
        type Status = enum {Pending = int64}

        let main = func () -> match Status::Pending(4) {
          Status::Pending(x) => x,
        }
    "#), @"int64");
}
