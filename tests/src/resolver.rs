#[track_caller]
fn _test_ty(source: &str) -> String {
    crate::init_logger();

    let program = match lutra_compiler::_test_compile_main(source) {
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

    let res = lutra_compiler::_test_compile_main(source);

    let diagnostics = match res {
        Ok(program) => {
            tracing::error!("ir: {program:#?}");
            panic!("expected compilation to fail, but it succedded");
        }
        Err(Error::Compile { diagnostics }) => diagnostics,
        Err(_) => unreachable!(),
    };
    let displays: Vec<_> = diagnostics
        .into_iter()
        .map(|d| d.display().to_string())
        .collect();
    displays.join("\n")
}

#[test]
fn types_01() {
    // type of a literal

    insta::assert_snapshot!(
        _test_ty(
            "const main = false"
        ),
        @"bool"
    );
}
#[test]
fn types_02() {
    // inference of type arg

    insta::assert_snapshot!(_test_ty(r#"
        func identity(x: T) where T -> x

        func main() -> identity(false)
    "#), @"bool");

    // same, but describe function as a type, not an expression
    insta::assert_snapshot!(_test_ty(r#"
        func identity(x: T): T where T

        func main() -> identity(false)
    "#), @"bool");
}
#[test]
fn types_03() {
    // validation of type params

    insta::assert_snapshot!(_test_err(r#"
        func floor_64(x: float64): float64

        func floor(x: T) where T -> floor_64(x)

        func main() -> floor(4.4)
    "#), @r"
    [E0006] Error:
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

        func main() -> apply(false, identity)
    "#), @"bool");

    insta::assert_snapshot!(_test_ty(r#"
        func twice(x: T) where T -> {x, x}

        func apply(x: I, mapper: func (I): O): O
        where I, O
        -> mapper(x)

        func main() -> apply(false, twice)
    "#), @"{bool, bool}");
}
#[test]
fn types_06() {
    insta::assert_snapshot!(_test_ty(r#"
        func identity(x: T) where T -> x

        func map(x: [I], mapper: func (I): O): [O]
        where I, O

        func main() -> map([false, true, false], identity)
    "#), @"[bool]");
}
#[test]
fn types_07() {
    insta::assert_snapshot!(_test_ty(r#"
        func twice(x: T) where T -> {x, x}

        func map(x: [I], mapper: func (I): O): [O]
        where I, O

        func main() -> map([false, true, true], twice)
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

        func main() -> (
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
        func main() -> (3: int64)..(5: int64)
    "#), @"{start: int64, end: int64}");
}
#[test]
fn types_11() {
    insta::assert_snapshot!(_test_ty(r#"
        type album_sale: {id: int64, total: float64}
        func get_album_sales(): [album_sale]

        func filter(array: [T], condition: func (T): bool): [T]
        where T

        func main(): [album_sale] -> (
            get_album_sales()
            | std::filter(func (this: album_sale) -> this.id == 6)
        )
    "#), @"[album_sale]");
}
#[test]
fn types_12() {
    // tuple lookup

    insta::assert_snapshot!(_test_ty(r#"
        const a = {id = 4: int64, total = 4.5: float64}
        func main() -> a.total
    "#), @"float64");
}
#[test]
fn types_13() {
    insta::assert_snapshot!(_test_ty(r#"
        func floor(x: T): T
        where T: float32 | float64

        func main() -> floor(2.4: float32)
    "#), @"float32");

    insta::assert_snapshot!(_test_err(r#"
        func floor(x: T): T
        where T: float32 | float64

        func main() -> floor(false)
    "#), @r"
    [E0007] Error:
       ╭─[:5:24]
       │
     5 │         func main() -> floor(false)
       │                        ──┬──
       │                          ╰──── T is restricted to one of float32, float64, found bool
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
        func main() -> floor(2.3)
    "#), @r"
    [E0006] Error:
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

        func main(f: float32) -> twice_floored(f)
    "#), @"{float64, float32}");

    insta::assert_snapshot!(_test_ty(r#"
        func floor(x: F): F
        where F: float32 | float64

        func twice_floored(x: T)
        where T: float64
        -> {floor(x), floor(x)}

        func main() -> twice_floored(2.3: float64)
    "#), @"{float64, float64}");

    insta::assert_snapshot!(_test_err(r#"
        func floor(x: F): F
        where F: float32 | float64

        func twice_floored(x: T)
        where T: float64 | bool
        -> {floor(x), floor(x)}

        func main() -> twice_floored(2.3)
    "#), @r"
    [E0007] Error:
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

        func main() -> get_b({a = false, b = 4: int64})
    "#), @"{a: bool, b: int64}");

    insta::assert_snapshot!(_test_err(r#"
        func get_b(x: T): T
        where T: {b: int64, ..}

        func main() -> get_b({a = false, b = 4.6: float64})
    "#), @r"
    [E0006] Error:
       ╭─[:5:46]
       │
     5 │         func main() -> get_b({a = false, b = 4.6: float64})
       │                                              ──────┬─────
       │                                                    ╰─────── expected type `int64`, but found type `float64`
    ───╯
    ");
    insta::assert_snapshot!(_test_err(r#"
        func get_b(x: T): T
        where T: {b: int64, ..}

        func main() -> get_b({a = false, c = 4})
    "#), @r"
    [E0006] Error:
       ╭─[:5:24]
       │
     5 │         func main() -> get_b({a = false, c = 4})
       │                        ──┬──
       │                          ╰──── field .b does not exist in type {a: bool, c: _}
    ───╯
    ");
}
#[test]
fn types_16() {
    // type param: tuple domain with positional arg
    insta::assert_snapshot!(_test_ty(r#"
        func get_b(x: T): T
        where T: {bool, int64, ..}

        func main() -> get_b({a = false, 4, c = 5.7: float32})
    "#), @"{a: bool, int64, c: float32}");

    insta::assert_snapshot!(_test_err(r#"
        func get_b(x: T): T
        where T: {bool, int64, ..}

        func main() -> get_b({a = "7", 4: int64, c = 5.7})
    "#), @r#"
    [E0006] Error:
       ╭─[:5:35]
       │
     5 │         func main() -> get_b({a = "7", 4: int64, c = 5.7})
       │                                   ─┬─
       │                                    ╰─── expected type `bool`, but found type `text`
    ───╯
    "#);

    insta::assert_snapshot!(_test_err(r#"
        func get_b(x: T): T
        where T: {bool, int64, a: bool, ..}

        func main() -> get_b({a = false})
    "#), @r"
    [E0006] Error:
       ╭─[:5:24]
       │
     5 │         func main() -> get_b({a = false})
       │                        ──┬──
       │                          ╰──── field .1 does not exist in type {a: bool}
    ───╯
    ");
    insta::assert_snapshot!(_test_ty(r#"
        func get_b(x: T): T
        where T: {bool, int64, a: bool, ..}

        func main() -> get_b({a = false, 4})
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

        func main() -> get({4})
    "#), @r"
    [E0006] Error:
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

        func main() -> get({4})
    "#), @r"
    [E0006] Error:
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

        func main() -> needs_one({4})
    "#), @r"
    [E0006] Error:
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

        func main() -> needs_two({4: int64, false})
    "#), @"{int64, bool}");
}

#[test]
fn types_18() {
    // lookup into type params

    insta::assert_snapshot!(_test_ty(r#"
        func f(x: T) where T: {int64, ..} -> x.0

        func main() -> f({4: int64, false})
    "#), @"int64");

    insta::assert_snapshot!(_test_ty(r#"
        func f(x: T) where T: {a: int64, ..} -> x.a
        func main() -> f({false, a = 4: int64})
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

        func main() -> (
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
fn types_21() {
    // error messages on bad lookups into type vars

    insta::assert_snapshot!(_test_err(r#"
        func main() -> (
          {a = false, "hello", c = "world"}
          | func (x) -> x.b
        )
    "#), @r"
    [E0006] Error:
       ╭─[:4:19]
       │
     4 │           | func (x) -> x.b
       │                   ┬
       │                   ╰── field .b does not exist in type {a: bool, text, c: text}
    ───╯
    ");
    insta::assert_snapshot!(_test_err(r#"
        func main() -> (
          {a = false, "hello", c = "world"}
          | func (x) -> x.3
        )
    "#), @r"
    [E0006] Error:
       ╭─[:4:19]
       │
     4 │           | func (x) -> x.3
       │                   ┬
       │                   ╰── field .3 does not exist in type {a: bool, text, c: text}
    ───╯
    ");
}

#[test]
fn types_22() {
    // error messages on bad lookups into things that are not tuples

    insta::assert_snapshot!(_test_err(r#"
        func main() -> [true, false].a
    "#), @r"
    [E0006] Error:
       ╭─[:2:37]
       │
     2 │         func main() -> [true, false].a
       │                                     ─┬
       │                                      ╰── lookup expected a tuple, found [bool]
    ───╯
    ");
    insta::assert_snapshot!(_test_err(r#"
        func main() -> "hello".a
    "#), @r#"
    [E0006] Error:
       ╭─[:2:31]
       │
     2 │         func main() -> "hello".a
       │                               ─┬
       │                                ╰── lookup expected a tuple, found text
    ───╯
    "#);
    insta::assert_snapshot!(_test_err(r#"
        func main(x: T) where T: int32 | text -> x.a
    "#), @r"
    Error:
       ╭─[:2:51]
       │
     2 │         func main(x: T) where T: int32 | text -> x.a
       │                                                   ─┬
       │                                                    ╰── lookup expected a tuple, found type parameter T
       │
       │ Note:
       │ T is not constrained to tuples only
       │ add `T: {}` to constrain it to tuples
    ───╯
    ");
    insta::assert_snapshot!(_test_err(r#"
        func main(x: T) where T -> x.a
    "#), @r"
    Error:
       ╭─[:2:37]
       │
     2 │         func main(x: T) where T -> x.a
       │                                     ─┬
       │                                      ╰── lookup expected a tuple, found type parameter T
       │
       │ Note:
       │ T is not constrained to tuples only
       │ add `T: {}` to constrain it to tuples
    ───╯
    ");
    insta::assert_snapshot!(_test_err(r#"
        type t: text
        func main(x: t) -> x.a
    "#), @r"
    [E0006] Error:
       ╭─[:3:29]
       │
     3 │         func main(x: t) -> x.a
       │                             ─┬
       │                              ╰── lookup expected a tuple, found text
    ───╯
    ");
}

#[test]
fn types_23() {
    // error messages on missing types on functions

    insta::assert_snapshot!(_test_ty(r#"
        func main(a: int32, b) -> a + b
    "#), @"int32");

    insta::assert_snapshot!(_test_err(r#"
        func main(a: int32, b) -> a + 1
    "#), @r"
    Error:
       ╭─[:2:29]
       │
     2 │         func main(a: int32, b) -> a + 1
       │                             ┬
       │                             ╰── cannot infer type
    ───╯
    ");

    insta::assert_snapshot!(_test_err(r#"
        func main()
    "#), @r"
    Error:
       ╭─[:2:9]
       │
     2 │         func main()
       │         ─────┬─────
       │              ╰─────── cannot infer type
    ───╯
    ");

    insta::assert_snapshot!(_test_err(r#"
        func main(name): {}
    "#), @r"
    Error:
       ╭─[:2:9]
       │
     2 │         func main(name): {}
       │         ─────────┬─────────
       │                  ╰─────────── cannot infer type
    ───╯
    ");
}

#[test]
fn array_00() {
    insta::assert_snapshot!(
        _test_err(
            "func main() -> []"
        ),
        @r"
    Error:
       ╭─[:1:16]
       │
     1 │ func main() -> []
       │                ─┬
       │                 ╰── cannot infer type
    ───╯
    "
    );
}

#[test]
fn array_01() {
    insta::assert_snapshot!(
        _test_err(
            "func main() -> std::lag([], 1)"
        ),
        @r"
    Error:
       ╭─[:1:16]
       │
     1 │ func main() -> std::lag([], 1)
       │                ────┬───
       │                    ╰───── cannot infer type of T
    ───╯
    "
    );
}

#[test]
fn array_02() {
    insta::assert_snapshot!(
        _test_ty(
            "func main(): [int64] -> std::lag([], 1)"
        ),
        @"[int64]"
    );
}

#[test]
fn array_03() {
    insta::assert_snapshot!(
        _test_err(
            "const main = {false, [], true}"
        ),
        @r"
    Error:
       ╭─[:1:22]
       │
     1 │ const main = {false, [], true}
       │                      ─┬
       │                       ╰── cannot infer type
    ───╯
    "
    );
}

#[test]
fn array_04() {
    insta::assert_snapshot!(
        _test_ty(
            "const main = [{5: int64, false}, {4, true}, {1, true}]"
        ),
        @"[{int64, bool}]"
    );
}

#[test]
fn array_05() {
    insta::assert_snapshot!(
        _test_ty(
            "const main = [{5, false}, {4, true}, {1, true}]: [{int64, bool}]"
        ),
        @"[{int64, bool}]"
    );
}

#[test]
fn array_06() {
    insta::assert_snapshot!(_test_err(r#"
        const main = [false, "true", false]"#
    ), @r#"
    [E0006] Error:
       ╭─[:2:30]
       │
     2 │         const main = [false, "true", false]
       │                              ───┬──
       │                                 ╰──── expected type `bool`, but found type `text`
    ───╯
    "#);
}

#[test]
fn type_annotation_00() {
    insta::assert_snapshot!(
        _test_ty(
            "const main = 5: int64"
        ),
        @"int64"
    );
}

#[test]
fn type_annotation_01() {
    insta::assert_snapshot!(
        _test_err(
            "const main = 5: text"
        ),
        @r"
    [E0007] Error:
       ╭─[:1:14]
       │
     1 │ const main = 5: text
       │              ┬
       │              ╰── restricted to one of int8, int16, int32, int64, uint8, uint16, uint32, uint64, found text
    ───╯
    "
    );
}

#[test]
fn type_annotation_02() {
    insta::assert_snapshot!(
        _test_ty(
            "const main = []: [bool]"
        ),
        @"[bool]"
    );
}

#[test]
fn primitives() {
    insta::assert_snapshot!(
        _test_ty(
            "
            func x(): {
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
        @"Status"
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
        @"Status"
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
       │                                      ╰─── recursive paths into self are not allowed
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
    assert_eq!(
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
        r#"
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
    [E0006] Error:
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
fn match_06() {
    insta::assert_snapshot!(_test_err(r#"
        type Animal: enum {
          Cat: text,
          Dog: bool,
        }

        func main(): [text] -> match Animal::Cat {
          .Cat(name) | .Dog(is_vaccinated) => true,
        }
    "#), @r"
    Error:
       ╭─[:8:24]
       │
     8 │           .Cat(name) | .Dog(is_vaccinated) => true,
       │                        ─────────┬─────────
       │                                 ╰─────────── patterns introduce different variable names
    ───╯
    ");
}

#[test]
fn match_07() {
    insta::assert_snapshot!(_test_err(r#"
        type Animal: enum {
          Cat: text,
          Dog: bool,
        }

        func main(): text -> match Animal::Cat {
          .Cat(name) | .Dog(name) => name,
        }
    "#), @r"
    [E0006] Error:
       ╭─[:8:29]
       │
     8 │           .Cat(name) | .Dog(name) => name,
       │                             ──┬─
       │                               ╰─── expected type `text`, but found type `bool`
    ───╯
    ");
}

#[test]
fn func_param_00() {
    insta::assert_snapshot!(_test_ty(r#"
        func main() -> (
            false | func (a) -> !a
        )
    "#), @"bool");
}

#[test]
fn func_param_01() {
    insta::assert_snapshot!(_test_ty(r#"
        func main(): int16 -> (
            3 | func (x) -> x * x
        )
    "#), @"int16");
}

#[test]
fn func_param_02() {
    insta::assert_snapshot!(_test_ty(r#"
        func main(): [int16] -> (
            [3, 2, 4, 1, 5, -3, 1]
            | std::map(func (x) -> -x)
        )
    "#), @"[int16]");
}

#[test]
fn func_param_03() {
    insta::assert_snapshot!(_test_ty(r#"
        func main() -> (
            {a = 3, b = 5: int16} | func (x) -> x.a + x.b
        )
    "#), @"int16");
}

#[test]
fn defs_00() {
    insta::assert_snapshot!(_test_err(r#"
        const a = 3
        const a = 6
    "#), @r"
    [E0003] Error:
       ╭─[:3:9]
       │
     3 │         const a = 6
       │         ─────┬─────
       │              ╰─────── duplicate name
    ───╯
    [E0003] Error:
       ╭─[:2:9]
       │
     2 │         const a = 3
       │         ─────┬─────
       │              ╰─────── duplicate name
    ───╯
    ");
}
#[test]
fn defs_01() {
    insta::assert_snapshot!(_test_err(r#"
        func a() -> 3
        func a() -> 6
    "#), @r"
    [E0003] Error:
       ╭─[:3:9]
       │
     3 │         func a() -> 6
       │         ──────┬──────
       │               ╰──────── duplicate name
    ───╯
    [E0003] Error:
       ╭─[:2:9]
       │
     2 │         func a() -> 3
       │         ──────┬──────
       │               ╰──────── duplicate name
    ───╯
    ");
}
#[test]
fn constants_00() {
    insta::assert_snapshot!(_test_err(r#"
        const a = {false, true || false}
        const b = [6: int64, 2 + 6]
        const c = 5: int64
        const d = [1, c]
    "#), @r"
    Error:
       ╭─[:2:27]
       │
     2 │         const a = {false, true || false}
       │                           ──────┬──────
       │                                 ╰──────── non-constant expression
       │
       │ Note:
       │ use `func` instead of `const`
    ───╯
    Error:
       ╭─[:3:30]
       │
     3 │         const b = [6: int64, 2 + 6]
       │                              ──┬──
       │                                ╰──── non-constant expression
       │
       │ Note:
       │ use `func` instead of `const`
    ───╯
    ");
}
#[test]
fn constants_01() {
    insta::assert_snapshot!(_test_err(r#"
        const a = func () -> true
    "#), @r"
    Error:
       ╭─[:2:19]
       │
     2 │         const a = func () -> true
       │                   ───────┬───────
       │                          ╰───────── non-constant expression
       │
       │ Note:
       │ use `func` instead of `const`
    ───╯
    ");
}
#[test]
fn constants_02() {
    insta::assert_snapshot!(_test_err(r#"
        func main(table_name: text): [{bool}] -> std::sql::from(table_name)
    "#), @r"
    Error:
       ╭─[:2:65]
       │
     2 │         func main(table_name: text): [{bool}] -> std::sql::from(table_name)
       │                                                                 ─────┬────
       │                                                                      ╰────── non-constant expression
    ───╯
    ");
}
#[test]
fn unpack_00() {
    insta::assert_snapshot!(_test_ty(r#"
        const x = {false, "hello"}
        const main = {false, ..x, "hello"}
    "#), @"{bool, bool, text, text}");
}
#[test]
fn unpack_01() {
    insta::assert_snapshot!(_test_err(r#"
        const main = {false, ..true, "hello"}
    "#), @r#"
    [E0006] Error:
       ╭─[:2:32]
       │
     2 │         const main = {false, ..true, "hello"}
       │                                ──┬─
       │                                  ╰─── only tuples can be unpacked
       │
       │ Note:
       │ got type bool
    ───╯
    "#);
}
#[test]
fn unpack_02() {
    insta::assert_snapshot!(_test_ty(r#"
        type A: {int32, int32}
        const x: A = {45, 6}
        const main = {false, ..x, "hello"}
    "#), @"{bool, int32, int32, text}");
}
#[test]
fn unpack_03() {
    insta::assert_snapshot!(_test_ty(r#"
        func identity(x: T): T
        where T: {..}
        -> x

        func main() -> {a = 4: int32, ..identity({true, false}), b = false}
    "#), @"{a: int32, bool, bool, b: bool}");
}
#[test]
fn unpack_04() {
    insta::assert_snapshot!(_test_ty(r#"
        func false_x_false(x: T)
        where T: {..}
        -> {false, ..x, false}

        func main() -> false_x_false({"a", "b"})
    "#), @"{bool, text, text, bool}");
}

#[test]
fn unpack_05() {
    insta::assert_snapshot!(_test_ty(r#"
        const main = {
          a = 4: int32,
          ..{
            x1 = 3: int32,
            x2 = "hello",
          },
          ..{
            x3 = "world",
          },
          b = false
        }.x3
    "#), @"text");
}

#[test]
fn unpack_06() {
    // lookup into an unpack of a type var

    insta::assert_snapshot!(_test_err(r#"
      func main() -> (
        {x1 = true, x2 = false}
        | func (x) -> {4: int32, ..x, "hello"}.x2
      )
    "#), @r#"
    Error:
       ╭─[:4:47]
       │
     4 │         | func (x) -> {4: int32, ..x, "hello"}.x2
       │                                               ─┬─
       │                                                ╰─── ambiguous lookup into unpack of an unknown type
       │
       │ Note:
       │ consider annotating the unpacked expression
    ───╯
    "#);
}

#[test]
fn unpack_11() {
    insta::assert_snapshot!(_test_err(r#"
    func identity(x: T): T where T: {..} -> x
    func main() -> {4: int32, ..identity({true, false}), "hello"}.2
    "#), @r#"
    Error:
       ╭─[:3:66]
       │
     3 │     func main() -> {4: int32, ..identity({true, false}), "hello"}.2
       │                                                                  ─┬
       │                                                                   ╰── ambiguous lookup into unpack of an unknown type
       │
       │ Note:
       │ consider annotating the unpacked expression
    ───╯
    "#);
}

#[test]
fn unpack_12() {
    insta::assert_snapshot!(_test_ty(r#"
    func identity(x: T): T where T: {..} -> x
    func main() -> {4: int32, ..identity({true, false}): {bool, bool}, "hello"}.2
    "#), @"bool");
}

#[test]
fn unpack_13() {
    insta::assert_snapshot!(_test_err(r#"
    func false_x_false(x: T) where T: {x2: text, ..} -> {false, ..x, false}.2
    func main() -> false_x_false({true, x2 = "hello"})
    "#), @r"
    Error:
       ╭─[:2:76]
       │
     2 │     func false_x_false(x: T) where T: {x2: text, ..} -> {false, ..x, false}.2
       │                                                                            ─┬
       │                                                                             ╰── cannot do positional lookup into unpack of this type param
    ───╯
    ");
}

#[test]
fn unpack_14() {
    insta::assert_snapshot!(_test_ty(r#"
    func false_x_false(x: T) where T: {bool, text, ..} -> {false, ..x, false}.2
    func main() -> false_x_false({true, "hello"})
    "#), @"text");
}

#[test]
fn ty_tuple_comprehension_00() {
    // map to a simple type

    insta::assert_snapshot!(_test_ty(r#"
    type A: {id: int64, title: text}

    func main(): {for f: F in A do bool}
    -> std::default()
    "#), @"{bool, bool}");
}

#[test]
fn ty_tuple_comprehension_01() {
    // map to a simple type

    insta::assert_snapshot!(_test_ty(r#"
    type A: {id: int64, title: text}

    func main(): {for f: F in A do f: bool}
    -> std::default()
    "#), @"{id: bool, title: bool}");
}

#[test]
fn ty_tuple_comprehension_01a() {
    // bad body field name

    insta::assert_snapshot!(_test_err(r#"
    type A: {id: int64, title: text}

    func main(): {for f: F in A do g: bool}
    -> std::default()
    "#), @r"
    Error:
       ╭─[:4:5]
       │
     4 │ ╭─▶     func main(): {for f: F in A do g: bool}
     5 │ ├─▶     -> std::default()
       │ │
       │ ╰─────────────────────────── expected field to be named f
    ───╯
    ");
}

#[test]
fn ty_tuple_comprehension_02() {
    // map to a complex type, without refs to comprehension variable

    insta::assert_snapshot!(_test_ty(r#"
    type A: {id: int64, title: text}

    func main(): {for f: F in A do f: [bool]}
    -> std::default()
    "#), @"{id: [bool], title: [bool]}");
}

#[test]
fn ty_tuple_comprehension_03() {
    // lookup in tuple comprehension

    insta::assert_snapshot!(_test_ty(r#"
    type A: {id: int64, title: text}
    type B: {for f: F in A do f: bool}

    func f(flags: B): bool -> flags.id && flags.title

    func main() -> f({id = true, title = false})
    "#), @"bool");
}

#[test]
fn ty_tuple_comprehension_04() {
    // remove tuple names

    insta::assert_snapshot!(_test_ty(r#"
    type A: {id: int64, title: text}

    func main(): {for f: F in A do F}
    -> std::default()
    "#), @"{int64, text}");
}

#[test]
fn ty_tuple_comprehension_05() {
    // wrap into array

    insta::assert_snapshot!(_test_ty(r#"
    type A: {id: int64, title: text}

    func main(): {for f: F in A do f: [F]}
    -> std::default()
    "#), @"{id: [int64], title: [text]}");
}

#[test]
fn ty_tuple_comprehension_06() {
    // validate tuples against comprehension

    insta::assert_snapshot!(_test_ty(r#"
    func make_flags(x: T): {for f: F in T do f: bool}
    where T: {..}
    -> std::default()

    type A: {id: int64, title: text}

    func main(a: A): {id: bool, title: bool} -> make_flags(a)
    "#), @"{id: bool, title: bool}");

    insta::assert_snapshot!(_test_err(r#"
    func make_flags(x: T): {for f: F in T do f: bool}
    where T: {..}
    -> std::default()

    type A: {id: int64, title: text}

    func main(a: A): {id: bool, title: bool, release_year: bool} -> make_flags(a)
    "#), @r"
    [E0006] Error:
       ╭─[:8:69]
       │
     8 │     func main(a: A): {id: bool, title: bool, release_year: bool} -> make_flags(a)
       │                                                                     ─────┬────
       │                                                                          ╰────── field .2 does not exist in type {id: int64, title: text}
    ───╯
    ");

    insta::assert_snapshot!(_test_err(r#"
    func make_flags(x: T): {for f: F in T do f: bool}
    where T: {..}
    -> std::default()

    type A: {id: int64, title: text, release_year: int16}

    func main(a: A): {id: bool, title: bool} -> make_flags(a)
    "#), @r"
    [E0007] Error:
       ╭─[:8:49]
       │
     8 │     func main(a: A): {id: bool, title: bool} -> make_flags(a)
       │                                                 ─────┬────
       │                                                      ╰────── expected a tuple with 2 fields, found {id: int64, title: text, release_year: int16}
    ───╯
    ");
}

#[test]
fn ty_tuple_comprehension_07() {
    // validate tuples against comprehension

    insta::assert_snapshot!(_test_ty(r#"
    type A: {id: int64, title: text}
    func main(a: {id: bool, title: bool}): {for f: F in A do f: bool} -> a
    "#), @"{id: bool, title: bool}");
}

#[test]
fn ty_tuple_comprehension_08() {
    // lookup into comprehension

    insta::assert_snapshot!(_test_ty(r#"
    type A: {id: int64, title: text}
    func main(a: {for f: F in A do f: {F, F}}) -> a.title
    "#), @"{text, text}");
}

#[test]
fn ty_tuple_comprehension_09() {
    // validation of comprehension

    // I'm not really sure why does this test case require the intermediate
    // function. It was derived from a panic when dealing with to_columnar and
    // from_columnar. It was fixed by adding `LocalTyInliner`.

    insta::assert_snapshot!(_test_ty(r#"
    func from_columnar(columnar: {for f: F in T do f: [F]}): [T]
    where T: {..}

    func main() -> (
      {gift_id = []}: {gift_id: [int32]}
      | func (x) -> {x.gift_id, x.gift_id}
      | from_columnar
    )
    "#), @"[{int32, int32}]");

    // A few useful functions for writing tests:

    // func each_once(a: A): {for f: F in A do f: [F]}
    // where A: {..}

    // func first(t: [T]): T
    // where T
}

#[test]
fn tuple_00() {
    // tuples, named type validation

    insta::assert_snapshot!(_test_ty(r#"
    func main(): {title: text, is_released: bool} -> {title = "hello", is_released = true}
    "#), @"{title: text, is_released: bool}");

    insta::assert_snapshot!(_test_err(r#"
    func main(): {title: text, is_released: bool} -> {title = "hello"}
    "#), @r#"
    [E0006] Error:
       ╭─[:2:54]
       │
     2 │     func main(): {title: text, is_released: bool} -> {title = "hello"}
       │                                                      ────────┬────────
       │                                                              ╰────────── expected type `{title: text, is_released: bool}`, but found type `{title: text}`
    ───╯
    "#);

    insta::assert_snapshot!(_test_err(r#"
    func main(): {title: text} -> {title = "hello", is_released = true}
    "#), @r#"
    [E0006] Error:
       ╭─[:2:35]
       │
     2 │     func main(): {title: text} -> {title = "hello", is_released = true}
       │                                   ──────────────────┬──────────────────
       │                                                     ╰──────────────────── expected type `{title: text}`, but found type `{title: text, is_released: bool}`
    ───╯
    "#);
}

#[test]
fn tuple_01() {
    // tuples, positional type validation

    insta::assert_snapshot!(_test_ty(r#"
    func main(): {text, bool} -> {"hello", true}
    "#), @"{text, bool}");

    insta::assert_snapshot!(_test_err(r#"
    func main(): {text, bool} -> {"hello"}
    "#), @r#"
    [E0006] Error:
       ╭─[:2:34]
       │
     2 │     func main(): {text, bool} -> {"hello"}
       │                                  ────┬────
       │                                      ╰────── expected type `{text, bool}`, but found type `{text}`
    ───╯
    "#);

    insta::assert_snapshot!(_test_err(r#"
    func main(): {text} -> {"hello", true}
    "#), @r#"
    [E0006] Error:
       ╭─[:2:28]
       │
     2 │     func main(): {text} -> {"hello", true}
       │                            ───────┬───────
       │                                   ╰───────── expected type `{text}`, but found type `{text, bool}`
    ───╯
    "#);
}

#[test]
fn tuple_02() {
    // tuples, positional type variation of named tuples

    insta::assert_snapshot!(_test_ty(r#"
    func main(): {text, bool} -> {title = "hello", is_released = true}
    "#), @"{text, bool}");

    insta::assert_snapshot!(_test_err(r#"
    func main(): {text, bool} -> {title = "hello"}
    "#), @r#"
    [E0006] Error:
       ╭─[:2:34]
       │
     2 │     func main(): {text, bool} -> {title = "hello"}
       │                                  ────────┬────────
       │                                          ╰────────── expected type `{text, bool}`, but found type `{title: text}`
    ───╯
    "#);

    insta::assert_snapshot!(_test_err(r#"
    func main(): {text} -> {title = "hello", is_released = true}
    "#), @r#"
    [E0006] Error:
       ╭─[:2:28]
       │
     2 │     func main(): {text} -> {title = "hello", is_released = true}
       │                            ──────────────────┬──────────────────
       │                                              ╰──────────────────── expected type `{text}`, but found type `{title: text, is_released: bool}`
    ───╯
    "#);
}

#[test]
fn tuple_03() {
    // tuples, named type validation of unnamed tuples

    insta::assert_snapshot!(_test_ty(r#"
    func main(): {title: text, is_released: bool} -> {"hello", true}
    "#), @"{title: text, is_released: bool}");

    insta::assert_snapshot!(_test_err(r#"
    func main(): {title: text, is_released: bool} -> {"hello"}
    "#), @r#"
    [E0006] Error:
       ╭─[:2:54]
       │
     2 │     func main(): {title: text, is_released: bool} -> {"hello"}
       │                                                      ────┬────
       │                                                          ╰────── expected type `{title: text, is_released: bool}`, but found type `{text}`
    ───╯
    "#);

    insta::assert_snapshot!(_test_err(r#"
    func main(): {title: text} -> {"hello", true}
    "#), @r#"
    [E0006] Error:
       ╭─[:2:35]
       │
     2 │     func main(): {title: text} -> {"hello", true}
       │                                   ───────┬───────
       │                                          ╰───────── expected type `{title: text}`, but found type `{text, bool}`
    ───╯
    "#);
}

#[test]
fn tuple_04() {
    // tuples, multiple diagnostics

    insta::assert_snapshot!(_test_err(r#"
    func main(): {title: bool, is_released: bool} -> {"hello", "world"}
    "#), @r#"
    [E0006] Error:
       ╭─[:2:55]
       │
     2 │     func main(): {title: bool, is_released: bool} -> {"hello", "world"}
       │                                                       ───┬───
       │                                                          ╰───── expected type `bool`, but found type `text`
    ───╯
    [E0006] Error:
       ╭─[:2:64]
       │
     2 │     func main(): {title: bool, is_released: bool} -> {"hello", "world"}
       │                                                                ───┬───
       │                                                                   ╰───── expected type `bool`, but found type `text`
    ───╯
    "#);
}

#[test]
fn import_00() {
    insta::assert_snapshot!(_test_ty(r#"
    module a {
      func f() -> "f"

      const c = "c"

      type T: text
    }

    import a::f
    import a::c as d
    import a::T as U

    func main() -> {d, f()}: {a::T, U}
    "#), @"{a::T, a::T}");
}

#[test]
fn import_01() {
    insta::assert_snapshot!(_test_ty(r#"
    module a {
      module b {
        const c = "c"
        import project::a::b::d2 as d1
        import super::d3 as d2
      }
      import b::c as d3
    }

    func main() -> a::b::d1
    "#), @"text");
}

#[test]
fn import_02() {
    insta::assert_snapshot!(_test_err(r#"
    module a {
      import project::a::b as b
    }
    func main() -> {}
    "#), @r"
    Error:
       ╭─[:3:14]
       │
     3 │       import project::a::b as b
       │              ─────────┬────────
       │                       ╰────────── recursive path
    ───╯
    ");
}

#[test]
fn import_03() {
    insta::assert_snapshot!(_test_ty(r#"
    module a {
      module b {
        const c = "hello"
      }
      const d = false
    }
    import a as e
    func main() -> {e::d, e::b::c}
    "#), @"{bool, text}");
}

#[test]
fn import_04() {
    insta::assert_snapshot!(_test_err(r#"
    module a {}
    func main() -> a
    "#), @r"
    [E0005] Error:
       ╭─[:3:20]
       │
     3 │     func main() -> a
       │                    ┬
       │                    ╰── expected a value, found a module
    ───╯
    ");
    insta::assert_snapshot!(_test_err(r#"
    module a {}
    func main(): a -> 1
    "#), @r"
    [E0005] Error:
       ╭─[:3:18]
       │
     3 │     func main(): a -> 1
       │                  ┬
       │                  ╰── expected a type, found a module
    ───╯
    ");
}

#[test]
fn import_05() {
    insta::assert_snapshot!(_test_ty(r#"
    module a {
      const b = "b"
      const c = false
    }
    import a::(b, c)
    func main() -> {b, c}
    "#), @"{text, bool}");
}

#[test]
fn import_06() {
    insta::assert_snapshot!(_test_ty(r#"
    module a {
      const b = "b"
      module c {
        const d: int16 = 3
        const e = false
      }
    }
    import a::(b, c::(d, e))
    func main() -> {b, d, e}
    "#), @"{text, int16, bool}");
}

#[test]
fn nominal_00() {
    insta::assert_snapshot!(_test_err(r#"
    type MyInt(int32)
    const x: MyInt = 4: int32
    "#), @r"
    [E0006] Error:
       ╭─[:3:22]
       │
     3 │     const x: MyInt = 4: int32
       │                      ────┬───
       │                          ╰───── x expected type `MyInt`, but found type `int32`
    ───╯
    ");
}

#[test]
fn nominal_01() {
    insta::assert_snapshot!(_test_ty(r#"
    type MyInt(int32)
    const main = MyInt(4: int32)
    "#), @"MyInt");
}

#[test]
fn nominal_02() {
    insta::assert_snapshot!(_test_err(r#"
    type MyInt(int32)
    const main = MyInt(4: int32, false)
    "#), @r"
    Error:
       ╭─[:3:18]
       │
     3 │     const main = MyInt(4: int32, false)
       │                  ───────────┬──────────
       │                             ╰──────────── MyInt expected 1 arguments, but got 2
    ───╯
    ");
}

#[test]
fn nominal_03() {
    insta::assert_snapshot!(_test_err(r#"
    type MyInt(int32)
    const main = MyInt()
    "#), @r"
    Error:
       ╭─[:3:18]
       │
     3 │     const main = MyInt()
       │                  ───┬───
       │                     ╰───── MyInt expected 1 arguments, but got 0
    ───╯
    ");
}

#[test]
fn nominal_04() {
    insta::assert_snapshot!(_test_err(r#"
    type MyInt(int32)
    const main = MyInt(false)
    "#), @r"
    [E0006] Error:
       ╭─[:3:24]
       │
     3 │     const main = MyInt(false)
       │                        ──┬──
       │                          ╰──── function MyInt, one of the params expected type `MyInt`, but found type `bool`
       │
       │ Note:
       │ type `MyInt` expands to `int32`
    ───╯
    ");
}

#[test]
fn nominal_05() {
    insta::assert_snapshot!(_test_ty(r#"
    type MyInt(int32)
    const main = MyInt(12).0
    "#), @"int32");
}

#[test]
fn nominal_06() {
    insta::assert_snapshot!(_test_err(r#"
    type MyInt(int32)
    const main = MyInt(12).1
    "#), @r"
    [E0006] Error:
       ╭─[:3:27]
       │
     3 │     const main = MyInt(12).1
       │                           ─┬
       │                            ╰── field .1 does not exist in type MyInt
       │
       │ Note:
       │ MyInt is a framed type. Inner value can be accessed with `.0`
    ───╯
    ");
}

#[test]
fn nominal_07() {
    insta::assert_snapshot!(_test_err(r#"
    type MyInt(int32)
    const main = MyInt(12).inner
    "#), @r"
    [E0006] Error:
       ╭─[:3:27]
       │
     3 │     const main = MyInt(12).inner
       │                           ───┬──
       │                              ╰──── field .inner does not exist in type MyInt
       │
       │ Note:
       │ MyInt is a framed type. Inner value can be accessed with `.0`
    ───╯
    ");
}
