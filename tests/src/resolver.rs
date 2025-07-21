#[track_caller]
fn _test_ty(source: &str) -> String {
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
        _test_ty(
            "func () -> false"
        ),
        @"bool"
    );
}
#[test]
fn types_02() {
    // inference of type arg

    insta::assert_snapshot!(_test_ty(r#"
        func identity(x: T) where T -> x

        func () -> identity(false)
    "#), @"bool");

    // same, but describe function as a type, not an expression
    insta::assert_snapshot!(_test_ty(r#"
        func identity(x: T): T where T

        func () -> identity(false)
    "#), @"bool");
}
#[test]
fn types_03() {
    // validation of type params

    insta::assert_snapshot!(_test_err(r#"
        func floor_64(x: float64): float64

        func floor(x: T) where T -> floor_64(x)

        func () -> floor(4.4)
    "#), @r"
    [E0004] Error:
       ╭─[:4:46]
       │
     4 │         func floor(x: T) where T -> floor_64(x)
       │                                              ┬
       │                                              ╰── function floor_64, one of the params expected type `float64`, but found type `T`
    ───╯
    ");
}
#[test]
fn types_05() {
    insta::assert_snapshot!(_test_ty(r#"
        func identity(x: T) where T -> x

        func apply(x: I, mapper: func (I): O): O
        where I, O
        -> mapper(x)

        func () -> apply(false, identity)
    "#), @"bool");

    insta::assert_snapshot!(_test_ty(r#"
        func twice(x: T) where T -> {x, x}

        func apply(x: I, mapper: func (I): O): O
        where I, O
        -> mapper(x)

        func () -> apply(false, twice)
    "#), @"{bool, bool}");
}
#[test]
fn types_06() {
    insta::assert_snapshot!(_test_ty(r#"
        func identity(x: T) where T -> x

        func map(x: [I], mapper: func (I): O): [O]
        where I, O

        func () -> map([false, true, false], identity)
    "#), @"[bool]");
}
#[test]
fn types_07() {
    insta::assert_snapshot!(_test_ty(r#"
        func twice(x: T) where T -> {x, x}

        func map(x: [I], mapper: func (I): O): [O]
        where I, O

        func () -> map([false, true, true], twice)
    "#), @"[{bool, bool}]");
}
#[test]
fn types_08() {
    insta::assert_snapshot!(_test_ty(r#"
        func filter(array: [T], condition: func (T): bool): [T]
        where T

        func map(x: [I], mapper: func (I): O): [O]
        where I, O

        func slice(x: [T], start: int64, end: int64): [T]
        where T

        func () -> (
            [{"5", false}, {"4", true}, {"1", true}]
            | filter(func (x: {text, bool}) -> x.1)
            | map(func (x: {text, bool}) -> x.0)
            | slice(0, 1)
        )
    "#), @"[text]");
}
#[test]
fn types_09() {
    insta::assert_snapshot!(_test_err(r#"
        func peek(array: [T], condition: func <R> (T): R): [T]
        where T
    "#), @r"
    [E0003] Error:
       ╭─[:2:47]
       │
     2 │         func peek(array: [T], condition: func <R> (T): R): [T]
       │                                               ┬
       │                                               ╰── type expected (, but found <
    ───╯
    ");
}
#[test]
fn types_10() {
    // range

    insta::assert_snapshot!(_test_ty(r#"
        func () -> (3: int64)..(5: int64)
    "#), @"{start: int64, end: int64}");
}
#[test]
fn types_11() {
    insta::assert_snapshot!(_test_ty(r#"
        type album_sale: {id: int64, total: float64}
        func get_album_sales(): [album_sale]

        func filter(array: [T], condition: func (T): bool): [T]
        where T

        func (): [album_sale] -> (
            get_album_sales()
            | std::filter(func (this: album_sale) -> this.id == 6)
        )
    "#), @"[album_sale]");
}
#[test]
fn types_12() {
    // tuple indirection

    insta::assert_snapshot!(_test_ty(r#"
        let a = {id = 4: int64, total = 4.5: float64}
        func () -> a.total
    "#), @"float64");
}
#[test]
fn types_13() {
    insta::assert_snapshot!(_test_ty(r#"
        func floor(x: T): T
        where T: float32 | float64

        func () -> floor(2.4: float32)
    "#), @"float32");

    insta::assert_snapshot!(_test_err(r#"
        func floor(x: T): T
        where T: float32 | float64

        func () -> floor(false)
    "#), @r"
    [E0005] Error:
       ╭─[:5:20]
       │
     5 │         func () -> floor(false)
       │                    ──┬──
       │                      ╰──── T is restricted to one of float32, float64, found bool
    ───╯
    ");
}
#[test]
fn types_14() {
    // validate type params: one of

    insta::assert_snapshot!(_test_err(r#"
        func floor_64(x: float64): float64

        func floor(x: T): T
        where T: float32 | float64
        -> (
            floor_64(x)
        )
        func () -> floor(2.3)
    "#), @r"
    [E0004] Error:
       ╭─[:7:22]
       │
     7 │             floor_64(x)
       │                      ┬
       │                      ╰── function floor_64, one of the params expected type `float64`, but found type `T`
    ───╯
    ");

    insta::assert_snapshot!(_test_ty(r#"
        func floor(x: F): F
        where F: float32 | float64

        func twice_floored(x: T)
        where T: float32 | float64
        -> {floor(4.5: float64), floor(x)}

        func (f: float32) -> twice_floored(f)
    "#), @"{float64, float32}");

    insta::assert_snapshot!(_test_ty(r#"
        func floor(x: F): F
        where F: float32 | float64

        func twice_floored(x: T)
        where T: float64
        -> {floor(x), floor(x)}

        func () -> twice_floored(2.3: float64)
    "#), @"{float64, float64}");

    insta::assert_snapshot!(_test_err(r#"
        func floor(x: F): F
        where F: float32 | float64

        func twice_floored(x: T)
        where T: float64 | bool
        -> {floor(x), floor(x)}

        func () -> twice_floored(2.3)
    "#), @r"
    [E0005] Error:
       ╭─[:7:13]
       │
     7 │         -> {floor(x), floor(x)}
       │             ──┬──
       │               ╰──── T is restricted to one of float32, float64, found bool
    ───╯
    ");
}
#[test]
fn types_15() {
    // type param: tuple domain with named arg
    insta::assert_snapshot!(_test_ty(r#"
        func get_b(x: T): T
        where T: {b: int64, ..}

        func () -> get_b({a = false, b = 4: int64})
    "#), @"{a: bool, b: int64}");

    insta::assert_snapshot!(_test_err(r#"
        func get_b(x: T): T
        where T: {b: int64, ..}

        func () -> get_b({a = false, b = 4.6: float64})
    "#), @r"
    [E0004] Error:
       ╭─[:5:42]
       │
     5 │         func () -> get_b({a = false, b = 4.6: float64})
       │                                          ─┬─
       │                                           ╰─── expected type `int64`, but found type `float64`
    ───╯
    ");
    insta::assert_snapshot!(_test_err(r#"
        func get_b(x: T): T
        where T: {b: int64, ..}

        func () -> get_b({a = false, c = 4})
    "#), @r"
    Error:
       ╭─[:5:20]
       │
     5 │         func () -> get_b({a = false, c = 4})
       │                    ──┬──
       │                      ╰──── field .b does not exist in type {a: bool, c: _}
    ───╯
    ");
}
#[test]
fn types_16() {
    // type param: tuple domain with positional arg
    insta::assert_snapshot!(_test_ty(r#"
        func get_b(x: T): T
        where T: {bool, int64, ..}

        func () -> get_b({a = false, 4, c = 5.7: float32})
    "#), @"{a: bool, int64, c: float32}");

    insta::assert_snapshot!(_test_err(r#"
        func get_b(x: T): T
        where T: {bool, int64, ..}

        func () -> get_b({a = "7", 4: int64, c = 5.7})
    "#), @r#"
    [E0004] Error:
       ╭─[:5:31]
       │
     5 │         func () -> get_b({a = "7", 4: int64, c = 5.7})
       │                               ─┬─
       │                                ╰─── expected type `bool`, but found type `text`
    ───╯
    "#);

    insta::assert_snapshot!(_test_err(r#"
        func get_b(x: T): T
        where T: {bool, int64, a: bool, ..}

        func () -> get_b({a = false})
    "#), @r"
    Error:
       ╭─[:5:20]
       │
     5 │         func () -> get_b({a = false})
       │                    ──┬──
       │                      ╰──── field .1 does not exist in type {a: bool}
    ───╯
    ");
    insta::assert_snapshot!(_test_ty(r#"
        func get_b(x: T): T
        where T: {bool, int64, a: bool, ..}

        func () -> get_b({a = false, 4})
    "#), @"{a: bool, int64}");
}

#[test]
fn types_17() {
    // validate type params: tuple domain

    insta::assert_snapshot!(_test_err(r#"
        func get_int(x: {int64}): int64

        func get(x: T): T
        where T: {int64, ..}
        -> get_int(x)

        func () -> get({4})
    "#), @r"
    [E0004] Error:
       ╭─[:6:20]
       │
     6 │         -> get_int(x)
       │                    ┬
       │                    ╰── function get_int, one of the params expected type `{int64}`, but found type `T`
    ───╯
    ");

    insta::assert_snapshot!(_test_err(r#"
        func get_int(x: {a: int64}): int64

        func get(x: T): T
        where T: {a: int64, ..}
        -> get_int(x)

        func () -> get({4})
    "#), @r"
    [E0004] Error:
       ╭─[:6:20]
       │
     6 │         -> get_int(x)
       │                    ┬
       │                    ╰── function get_int, one of the params expected type `{a: int64}`, but found type `T`
    ───╯
    ");

    insta::assert_snapshot!(_test_err(r#"
        func needs_two(x: I): int64
        where I: {int64, bool, ..}

        func needs_one(x: T): int64
        where T: {int64, ..}
        -> needs_two(x)

        func () -> needs_one({4})
    "#), @r"
    Error:
       ╭─[:7:12]
       │
     7 │         -> needs_two(x)
       │            ────┬────
       │                ╰────── field .1 does not exist
    ───╯
    ");

    insta::assert_snapshot!(_test_ty(r#"
        func needs_one(x: I): I
        where I: {int64, ..}

        func needs_two(x: T): T
        where T: {int64, bool, ..}
        -> needs_one(x)

        func () -> needs_two({4: int64, false})
    "#), @"{int64, bool}");
}

#[test]
fn types_18() {
    // indirection into type params

    insta::assert_snapshot!(_test_ty(r#"
        func f(x: T) where T: {int64, ..} -> x.0

        func () -> f({4: int64, false})
    "#), @"int64");

    insta::assert_snapshot!(_test_ty(r#"
        func f(x: T) where T: {a: int64, ..} -> x.a
        func () -> f({false, a = 4: int64})
    "#), @"int64");
}

#[test]
fn types_19() {
    insta::assert_snapshot!(_test_err(r#"
        func f(x) where T -> x + 1
    "#), @r"
    Error:
       ╭─[:2:16]
       │
     2 │         func f(x) where T -> x + 1
       │                ┬
       │                ╰── missing type annotations
    ───╯
    ");
}

#[test]
fn types_20() {
    // matching patterns into type vars

    insta::assert_snapshot!(_test_ty(r#"
        type Status: enum {Done, Pending: int16, Cancelled: text}

        func () -> (
          Status::Done
          | func (x) -> match x {
            .Done => "done",
            .Cancelled(reason) => f"pending {reason}",
            _ => f"something else",
          }
        )
    "#), @"text");
}

#[test]
fn array_00() {
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
fn array_01() {
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
fn array_02() {
    insta::assert_snapshot!(
        _test_ty(
            "func (): [int64] -> std::lag([], 1)"
        ),
        @"[int64]"
    );
}

#[test]
fn array_03() {
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
fn array_04() {
    insta::assert_snapshot!(
        _test_ty(
            "func () -> [{5: int64, false}, {4, true}, {1, true}]"
        ),
        @"[{int64, bool}]"
    );
}

#[test]
fn array_05() {
    insta::assert_snapshot!(
        _test_ty(
            "func () -> [{5, false}, {4, true}, {1, true}]: [{int64, bool}]"
        ),
        @"[{int64, bool}]"
    );
}

#[test]
fn type_annotation_00() {
    insta::assert_snapshot!(
        _test_ty(
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
    [E0005] Error:
       ╭─[:1:12]
       │
     1 │ func () -> 5: text
       │            ┬
       │            ╰── restricted to one of int8, int16, int32, int64, uint8, uint16, uint32, uint64, found text
    ───╯
    "
    );
}

#[test]
fn type_annotation_02() {
    insta::assert_snapshot!(
        _test_ty(
            "func () -> []: [bool]"
        ),
        @"[bool]"
    );
}

#[test]
fn primitives() {
    insta::assert_snapshot!(
        _test_ty(
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
            func main() -> x()
            "
        ),
        @"{bool, int8, int16, int32, int64, uint8, uint16, uint32, uint64, float32, float64, text}"
    );
}

#[test]
fn enums_00() {
    insta::assert_snapshot!(
        _test_ty(
            "
            type Status: enum { Open, Done, Pending: text }

            func main() -> Status::Done
            "
        ),
        @"enum {Open, Done, Pending: text}"
    );
}

#[test]
fn enums_01() {
    insta::assert_snapshot!(
        _test_ty(
            r#"
            type Status: enum { Open, Done, Pending: text }

            func main() -> Status::Pending("hello")
            "#
        ),
        @"enum {Open, Done, Pending: text}"
    );
}
#[test]
fn enums_02() {
    insta::assert_snapshot!(
        _test_ty(
            r#"
            type X: { a: int64 }

            func main(): X::a -> 5
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
            type X: { a: int64, b: X::a }
            "#
        ),
        @r"
    Error:
       ╭─[:2:36]
       │
     2 │             type X: { a: int64, b: X::a }
       │                                    ──┬─
       │                                      ╰─── paths into self type are not allowed
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
            type Tree: {left: Tree, right: Tree}

            type OptionalTree: enum {
                None,
                Some = Tree,
            }
            func main(): OptionalTree -> OptionalTree::None
            "#
        ),
        @r#"
    Tree has infinite size.
    "#
    );
}

#[test]
fn match_00() {
    insta::assert_snapshot!(_test_ty(r#"
        type Status: enum {Done, Pending: int16, Cancelled: text}

        func main() -> match Status::Done {
          .Done => "done",
          .Pending => "pending",
          .Cancelled => "cancelled",
        }
    "#), @"text");
}

#[test]
#[ignore] // TODO
fn match_01() {
    insta::assert_snapshot!(_test_err(r#"
        type Status: enum {Done, Pending: int16, Cancelled: text}

        func main() -> match Status::Done {
          .Done => "done",
          .Cancelled => "cancelled",
        }
    "#), @r#"
        Variant Status::Pending not covered.
    "#);
}

#[test]
fn match_02() {
    insta::assert_snapshot!(_test_err(r#"
        type Status: enum {Done, Pending: int16, Cancelled: text}
        type Color: enum {Red, Green, Blue}

        func main() -> match Status::Done {
          .Red => "red",
          .Green => "green",
          .Blue => "blue",
        }
    "#), @r#"
    Error:
       ╭─[:6:11]
       │
     6 │           .Red => "red",
       │           ──┬─
       │             ╰─── variant does not exist
    ───╯
    "#);
}

#[test]
fn match_03() {
    insta::assert_snapshot!(_test_err(r#"
        type Color: enum {Red, Green, Blue}

        func main() -> match Color::Green {
          .Red => "red",
          .Green => 1,
          .Blue => false,
        }
    "#), @r"
    [E0004] Error:
       ╭─[:7:20]
       │
     7 │           .Blue => false,
       │                    ──┬──
       │                      ╰──── match expected type `text`, but found type `bool`
    ───╯
    ");
}

#[test]
fn match_04() {
    insta::assert_snapshot!(_test_ty(r#"
        type Status: enum {Pending: bool}

        func main() -> match Status::Pending(false) {
          .Pending(x) => x,
        }
    "#), @"bool");
}

#[test]
fn match_05() {
    insta::assert_snapshot!(_test_ty(r#"
        type Status: enum {Pending: int32}

        func main() -> match Status::Pending(4) {
          .Pending(x) => x,
        }
    "#), @"int32");
}

#[test]
fn func_param_00() {
    insta::assert_snapshot!(_test_ty(r#"
        func () -> (
            false | func (a) -> !a
        )
    "#), @"bool");
}

#[test]
fn func_param_01() {
    insta::assert_snapshot!(_test_ty(r#"
        func (): int16 -> (
            3 | func (x) -> x * x
        )
    "#), @"int16");
}

#[test]
fn func_param_02() {
    insta::assert_snapshot!(_test_ty(r#"
        func (): [int16] -> (
            [3, 2, 4, 1, 5, -3, 1]
            | std::map(func (x) -> -x)
        )
    "#), @"[int16]");
}

#[test]
fn func_param_03() {
    insta::assert_snapshot!(_test_ty(r#"
        func () -> (
            {a = 3, b = 5: int16} | func (x) -> x.a + x.b
        )
    "#), @"int16");
}
