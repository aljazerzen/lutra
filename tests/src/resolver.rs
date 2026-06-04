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
        @"Bool"
    );
}
#[test]
fn types_02() {
    // inference of type arg

    insta::assert_snapshot!(_test_ty(r#"
        func identity(x: T) where T -> x

        func main() -> identity(false)
    "#), @"Bool");

    // same, but describe function as a type, not an expression
    insta::assert_snapshot!(_test_ty(r#"
        external func identity(x: T): T where T

        func main() -> identity(false)
    "#), @"Bool");
}
#[test]
fn types_03() {
    // validation of type params

    insta::assert_snapshot!(_test_err(r#"
        external func floor_64(x: Float64): Float64

        func floor(x: T) where T -> floor_64(x)

        func main() -> floor(4.4)
    "#), @"
    [E0006] Error:
       ╭─[ <unknown>:4:46 ]
       │
     4 │         func floor(x: T) where T -> floor_64(x)
       │                                              ┬
       │                                              ╰── func floor_64 expected type `Float64`, but found type `T`
    ───╯
    Error:
       ╭─[ <unknown>:6:24 ]
       │
     6 │         func main() -> floor(4.4)
       │                        ──┬──
       │                          ╰──── cannot infer type of T
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
    "#), @"Bool");

    insta::assert_snapshot!(_test_ty(r#"
        func twice(x: T) where T -> {x, x}

        func apply(x: I, mapper: func (I): O): O
        where I, O
        -> mapper(x)

        func main() -> apply(false, twice)
    "#), @"{x: Bool, x: Bool}");
}
#[test]
fn types_06() {
    insta::assert_snapshot!(_test_ty(r#"
        func identity(x: T) where T -> x

        external func map(x: [I], mapper: func (I): O): [O]
        where I, O

        func main() -> map([false, true, false], identity)
    "#), @"[Bool]");
}
#[test]
fn types_07() {
    insta::assert_snapshot!(_test_ty(r#"
        func twice(x: T) where T -> {x, x}

        external func map(x: [I], mapper: func (I): O): [O]
        where I, O

        func main() -> map([false, true, true], twice)
    "#), @"[{x: Bool, x: Bool}]");
}
#[test]
fn types_08() {
    insta::assert_snapshot!(_test_ty(r#"
        external func filter(array: [T], condition: func (T): Bool): [T]
        where T

        external func map(x: [I], mapper: func (I): O): [O]
        where I, O

        external func slice(x: [T], start: Int64, end: Int64): [T]
        where T

        func main() -> (
            [{"5", false}, {"4", true}, {"1", true}]
            | filter(func (x: {Text, Bool}) -> x.1)
            | map(func (x: {Text, Bool}) -> x.0)
            | slice(0, 1)
        )
    "#), @"[Text]");
}
#[test]
fn types_09() {
    insta::assert_snapshot!(_test_err(r#"
        external func peek(array: [T], condition: func <R> (T): R): [T]
        where T
    "#), @"
    [E0003] Error:
       ╭─[ <unknown>:2:56 ]
       │
     2 │         external func peek(array: [T], condition: func <R> (T): R): [T]
       │                                                        ┬
       │                                                        ╰── expected (, but found <
    ───╯
    ");
}
#[test]
fn types_10() {
    // range

    insta::assert_snapshot!(_test_ty(r#"
        func main() -> 3..5
    "#), @"{start: enum {none, some: Int64}, end: enum {none, some: Int64}}");
}
#[test]
fn types_11() {
    insta::assert_snapshot!(_test_ty(r#"
        type album_sale: {id: Int64, total: Float64}
        external func get_album_sales(): [album_sale]

        external func filter(array: [T], condition: func (T): Bool): [T]
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
        const a = {id = 4: Int64, total = 4.5: Float64}
        func main() -> a.total
    "#), @"Float64");
}

#[test]
fn types_13() {
    insta::assert_snapshot!(_test_ty(r#"
        external func floor(x: T): T
        where T: Float32 | Float64

        func main() -> floor(2.4: Float32)
    "#), @"Float32");

    insta::assert_snapshot!(_test_err(r#"
        external func floor(x: T): T
        where T: Float32 | Float64

        func main() -> floor(false)
    "#), @"
    [E0007] Error:
       ╭─[ <unknown>:5:24 ]
       │
     5 │         func main() -> floor(false)
       │                        ──┬──
       │                          ╰──── T is restricted to one of Float32, Float64, found Bool
    ───╯
    ");
}
#[test]
fn types_14() {
    // validate type params: one of

    insta::assert_snapshot!(_test_err(r#"
        external func floor_64(x: Float64): Float64

        func floor(x: T): T
        where T: Float32 | Float64
        -> floor_64(x)

        func main() -> floor(2.3)
    "#), @"
    [E0006] Error:
       ╭─[ <unknown>:6:21 ]
       │
     6 │         -> floor_64(x)
       │                     ┬
       │                     ╰── func floor_64 expected type `Float64`, but found type `T`
    ───╯
    [E0006] Error:
       ╭─[ <unknown>:6:12 ]
       │
     6 │         -> floor_64(x)
       │            ─────┬─────
       │                 ╰─────── expected type `T`, but found type `Float64`
    ───╯
    ");

    insta::assert_snapshot!(_test_ty(r#"
        external func floor(x: F): F
        where F: Float32 | Float64

        func twice_floored(x: T)
        where T: Float32 | Float64
        -> {floor(4.5: Float64), floor(x)}

        func main(f: Float32) -> twice_floored(f)
    "#), @"{Float64, Float32}");

    insta::assert_snapshot!(_test_ty(r#"
        external func floor(x: F): F
        where F: Float32 | Float64

        func twice_floored(x: T)
        where T: Float64
        -> {floor(x), floor(x)}

        func main() -> twice_floored(2.3: Float64)
    "#), @"{Float64, Float64}");

    insta::assert_snapshot!(_test_err(r#"
        external func floor(x: F): F
        where F: Float32 | Float64

        func twice_floored(x: T)
        where T: Float64 | Bool
        -> {floor(x), floor(x)}

        func main() -> twice_floored(2.3)
    "#), @"
    [E0007] Error:
       ╭─[ <unknown>:7:13 ]
       │
     7 │         -> {floor(x), floor(x)}
       │             ──┬──
       │               ╰──── T is restricted to one of Float32, Float64, found Bool
    ───╯
    ");
}
#[test]
fn types_15() {
    // type param: tuple domain with named arg
    insta::assert_snapshot!(_test_ty(r#"
        external func get_b(x: T): T
        where T: {b: Int64, ..}

        func main() -> get_b({a = false, b = 4: Int64})
    "#), @"{a: Bool, b: Int64}");

    insta::assert_snapshot!(_test_err(r#"
        external func get_b(x: T): T
        where T: {b: Int64, ..}

        func main() -> get_b({a = false, b = 4.6: Float64})
    "#), @"
    [E0006] Error:
       ╭─[ <unknown>:5:46 ]
       │
     5 │         func main() -> get_b({a = false, b = 4.6: Float64})
       │                                              ──────┬─────
       │                                                    ╰─────── expected type `Int64`, but found type `Float64`
    ───╯
    ");
    insta::assert_snapshot!(_test_err(r#"
        external func get_b(x: T): T
        where T: {b: Int64, ..}

        func main() -> get_b({a = false, c = 4})
    "#), @"
    [E0006] Error:
       ╭─[ <unknown>:5:24 ]
       │
     5 │         func main() -> get_b({a = false, c = 4})
       │                        ──┬──
       │                          ╰──── field .b does not exist in type {a: Bool, c: _}
    ───╯
    ");
}
#[test]
fn types_16() {
    // type param: tuple domain with positional arg
    insta::assert_snapshot!(_test_ty(r#"
        external func get_b(x: T): T
        where T: {Bool, Int64, ..}

        func main() -> get_b({a = false, 4, c = 5.7: Float32})
    "#), @"{a: Bool, Int64, c: Float32}");

    insta::assert_snapshot!(_test_err(r#"
        external func get_b(x: T): T
        where T: {Bool, Int64, ..}

        func main() -> get_b({a = "7", 4: Int64, c = 5.7})
    "#), @r#"
    [E0006] Error:
       ╭─[ <unknown>:5:35 ]
       │
     5 │         func main() -> get_b({a = "7", 4: Int64, c = 5.7})
       │                                   ─┬─
       │                                    ╰─── expected type `Bool`, but found type `Text`
    ───╯
    "#);

    insta::assert_snapshot!(_test_err(r#"
        external func get_b(x: T): T
        where T: {Bool, Int64, a: Bool, ..}

        func main() -> get_b({a = false})
    "#), @"
    [E0006] Error:
       ╭─[ <unknown>:5:24 ]
       │
     5 │         func main() -> get_b({a = false})
       │                        ──┬──
       │                          ╰──── field .1 does not exist in type {a: Bool}
    ───╯
    ");
    insta::assert_snapshot!(_test_ty(r#"
        external func get_b(x: T): T
        where T: {Bool, Int64, a: Bool, ..}

        func main() -> get_b({a = false, 4})
    "#), @"{a: Bool, Int64}");
}

#[test]
fn types_17() {
    // validate type params: tuple domain

    insta::assert_snapshot!(_test_err(r#"
        external func get_int(x: {Int64}): Int64

        func get(x: T): T
        where T: {Int64, ..}
        -> get_int(x)

        func main() -> get({4})
    "#), @"
    [E0006] Error:
       ╭─[ <unknown>:6:20 ]
       │
     6 │         -> get_int(x)
       │                    ┬
       │                    ╰── func get_int expected type `{Int64}`, but found type `T`
    ───╯
    [E0006] Error:
       ╭─[ <unknown>:6:12 ]
       │
     6 │         -> get_int(x)
       │            ─────┬────
       │                 ╰────── expected type `T`, but found type `Int64`
    ───╯
    ");

    insta::assert_snapshot!(_test_err(r#"
        external func get_int(x: {a: Int64}): Int64

        func get(x: T): T
        where T: {a: Int64, ..}
        -> get_int(x)

        func main() -> get({4})
    "#), @"
    [E0006] Error:
       ╭─[ <unknown>:6:20 ]
       │
     6 │         -> get_int(x)
       │                    ┬
       │                    ╰── func get_int expected type `{a: Int64}`, but found type `T`
    ───╯
    [E0006] Error:
       ╭─[ <unknown>:6:12 ]
       │
     6 │         -> get_int(x)
       │            ─────┬────
       │                 ╰────── expected type `T`, but found type `Int64`
    ───╯
    ");

    insta::assert_snapshot!(_test_err(r#"
        external func needs_two(x: I): Int64
        where I: {Int64, Bool, ..}

        func needs_one(x: T): Int64
        where T: {Int64, ..}
        -> needs_two(x)

        func main() -> needs_one({4})
    "#), @"
    [E0006] Error:
       ╭─[ <unknown>:7:12 ]
       │
     7 │         -> needs_two(x)
       │            ────┬────
       │                ╰────── field .1 does not exist
    ───╯
    ");

    insta::assert_snapshot!(_test_ty(r#"
        external func needs_one(x: I): I
        where I: {Int64, ..}

        func needs_two(x: T): T
        where T: {Int64, Bool, ..}
        -> needs_one(x)

        func main() -> needs_two({4: Int64, false})
    "#), @"{Int64, Bool}");
}

#[test]
fn types_18() {
    // lookup into type params

    insta::assert_snapshot!(_test_ty(r#"
        func f(x: T) where T: {Int64, ..} -> x.0

        func main() -> f({4: Int64, false})
    "#), @"Int64");

    insta::assert_snapshot!(_test_ty(r#"
        func f(x: T) where T: {a: Int64, ..} -> x.a
        func main() -> f({false, a = 4: Int64})
    "#), @"Int64");
}

#[test]
fn types_19() {
    insta::assert_snapshot!(_test_err(r#"
        func f(x) where T -> x + 1
    "#), @"
    Error:
       ╭─[ <unknown>:2:16 ]
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
        type Status: enum {done, pending: Int16, cancelled: Text}

        func main() -> (
          .done: Status
          | x -> match x {
            .done => "done",
            .cancelled(reason) => f"pending {reason}",
            _ => f"something else",
          }
        )
    "#), @"Text");
}

#[test]
fn types_21() {
    // error messages on bad lookups into type vars

    insta::assert_snapshot!(_test_err(r#"
    func main() -> (
      {a = false, "hello", c = "world"}
      | x -> x.b
    )
    "#), @"
    [E0006] Error:
       ╭─[ <unknown>:4:15 ]
       │
     4 │       | x -> x.b
       │               ─┬
       │                ╰── field .b does not exist in type {a: Bool, Text, c: Text}
    ───╯
    ");
    insta::assert_snapshot!(_test_err(r#"
    func main() -> (
      {a = false, "hello", c = "world"}
      | x -> x.3
    )
    "#), @"
    [E0006] Error:
       ╭─[ <unknown>:4:15 ]
       │
     4 │       | x -> x.3
       │               ─┬
       │                ╰── field .3 does not exist in type {a: Bool, Text, c: Text}
    ───╯
    ");
}

#[test]
fn types_22() {
    // error messages on bad lookups into things that are not tuples

    insta::assert_snapshot!(_test_err(r#"
        func main() -> [true, false].a
    "#), @"
    [E0006] Error:
       ╭─[ <unknown>:2:37 ]
       │
     2 │         func main() -> [true, false].a
       │                                     ─┬
       │                                      ╰── lookup expected a tuple, found [Bool]
    ───╯
    ");
    insta::assert_snapshot!(_test_err(r#"
        func main() -> "hello".a
    "#), @r#"
    [E0006] Error:
       ╭─[ <unknown>:2:31 ]
       │
     2 │         func main() -> "hello".a
       │                               ─┬
       │                                ╰── field .a does not exist in type Text
       │
       │ Note: Text is a framed type. Inner value can be accessed with `.0`
    ───╯
    "#);
    insta::assert_snapshot!(_test_err(r#"
        func main(x: T) where T: Int32 | Text -> x.a
    "#), @"
    Error:
       ╭─[ <unknown>:2:51 ]
       │
     2 │         func main(x: T) where T: Int32 | Text -> x.a
       │                                                   ─┬
       │                                                    ╰── lookup expected a tuple, found type parameter T
       │
       │ Note: T is not constrained to tuples only
       │       add `T: {}` to constrain it to tuples
    ───╯
    ");
    insta::assert_snapshot!(_test_err(r#"
        func main(x: T) where T -> x.a
    "#), @"
    Error:
       ╭─[ <unknown>:2:37 ]
       │
     2 │         func main(x: T) where T -> x.a
       │                                     ─┬
       │                                      ╰── lookup expected a tuple, found type parameter T
       │
       │ Note: T is not constrained to tuples only
       │       add `T: {}` to constrain it to tuples
    ───╯
    ");
    insta::assert_snapshot!(_test_err(r#"
        type t: Text
        func main(x: t) -> x.a
    "#), @"
    [E0006] Error:
       ╭─[ <unknown>:3:29 ]
       │
     3 │         func main(x: t) -> x.a
       │                             ─┬
       │                              ╰── field .a does not exist in type Text
       │
       │ Note: Text is a framed type. Inner value can be accessed with `.0`
    ───╯
    ");
}

#[test]
fn types_23() {
    // error messages on missing types on functions

    insta::assert_snapshot!(_test_ty(r#"
        func main(a: Int32, b) -> a + b
    "#), @"Int32");

    insta::assert_snapshot!(_test_err(r#"
        func main(a: Int32, b) -> a + 1
    "#), @"
    Error:
       ╭─[ <unknown>:2:29 ]
       │
     2 │         func main(a: Int32, b) -> a + 1
       │                             ┬
       │                             ╰── cannot infer type
    ───╯
    ");

    insta::assert_snapshot!(_test_err(r#"
        external func main()
    "#), @"
    [E0003] Error:
       ╭─[ <unknown>:2:29 ]
       │
     2 │         external func main()
       │                             │
       │                             ╰─ expected :, but encountered the end of the file.
    ───╯
    ");

    insta::assert_snapshot!(_test_err(r#"
        external func main(name): {}
    "#), @"
    Error:
       ╭─[ <unknown>:2:28 ]
       │
     2 │         external func main(name): {}
       │                            ──┬─
       │                              ╰─── name does not exist
    ───╯
    ");

    insta::assert_snapshot!(_test_err(r#"
        func main() -> "hello" | func (x: text): text
    "#), @r#"
    [E0003] Error:
       ╭─[ <unknown>:2:54 ]
       │
     2 │         func main() -> "hello" | func (x: text): text
       │                                                      │
       │                                                      ╰─ expected one of ->, :: or ?, but encountered the end of the file.
    ───╯
    "#);
}

#[test]
fn types_24() {
    // option type syntax desugars to enum { none, some: T }

    insta::assert_snapshot!(_test_ty(r#"
        func main(x: Int32?): Int32 -> match x {
          .none => 0,
          .some(v) => v + 1,
        }
    "#), @"Int32");

    insta::assert_snapshot!(_test_ty(r#"
        func main(x: [Int32]?): [Int32] -> match x {
          .none => [],
          .some(v) => v,
        }
    "#), @"[Int32]");
}

#[test]
fn types_25() {
    // types should only reference types
    insta::assert_snapshot!(_test_err(r#"
        module a {
        }
        type b: {a, Int64}
    "#), @"
    [E0005] Error:
       ╭─[ <unknown>:4:18 ]
       │
     4 │         type b: {a, Int64}
       │                  ┬
       │                  ╰── expected a type, found a module
    ───╯
    ");

    insta::assert_snapshot!(_test_err(r#"
        const a = false
        type b: {a, Int64}
    "#), @"
    [E0005] Error:
       ╭─[ <unknown>:3:18 ]
       │
     3 │         type b: {a, Int64}
       │                  ┬
       │                  ╰── expected a type, found a value
    ───╯
    ");

    insta::assert_snapshot!(_test_err(r#"
        func main(x: Int32) -> false | func (_: x) -> true
    "#), @"
    [E0005] Error:
       ╭─[ <unknown>:2:49 ]
       │
     2 │         func main(x: Int32) -> false | func (_: x) -> true
       │                                                 ┬
       │                                                 ╰── expected a type, found a value
    ───╯
    ");
}

#[test]
fn array_00() {
    insta::assert_snapshot!(
        _test_err(
            "func main() -> []"
        ),
        @"
    Error:
       ╭─[ <unknown>:1:16 ]
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
        @"
    Error:
       ╭─[ <unknown>:1:16 ]
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
            "func main(): [Int64] -> std::lag([], 1)"
        ),
        @"[Int64]"
    );
}

#[test]
fn array_03() {
    insta::assert_snapshot!(
        _test_err(
            "const main = {false, [], true}"
        ),
        @"
    Error:
       ╭─[ <unknown>:1:22 ]
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
            "const main = [{5: Int64, false}, {4, true}, {1, true}]"
        ),
        @"[{Int64, Bool}]"
    );
}

#[test]
fn array_05() {
    insta::assert_snapshot!(
        _test_ty(
            "const main = [{5, false}, {4, true}, {1, true}]: [{Int64, Bool}]"
        ),
        @"[{Int64, Bool}]"
    );
}

#[test]
fn array_06() {
    insta::assert_snapshot!(_test_err(r#"
        const main = [false, "true", false]"#
    ), @r#"
    [E0006] Error:
       ╭─[ <unknown>:2:30 ]
       │
     2 │         const main = [false, "true", false]
       │                              ───┬──
       │                                 ╰──── expected type `Bool`, but found type `Text`
    ───╯
    "#);
}

#[test]
fn type_annotation_00() {
    insta::assert_snapshot!(
        _test_ty(
            "const main = 5: Int64"
        ),
        @"Int64"
    );
}

#[test]
fn type_annotation_01() {
    insta::assert_snapshot!(
        _test_err(
            "const main = 5: Text"
        ),
        @"
    [E0007] Error:
       ╭─[ <unknown>:1:14 ]
       │
     1 │ const main = 5: Text
       │              ┬
       │              ╰── restricted to one of Int8, Int16, Int32, Int64, Uint8, Uint16, Uint32, Uint64, Float32, Float64, Decimal, found Text
    ───╯
    "
    );
}

#[test]
fn type_annotation_02() {
    insta::assert_snapshot!(
        _test_ty(
            "const main = []: [Bool]"
        ),
        @"[Bool]"
    );
}

#[test]
fn primitives() {
    insta::assert_snapshot!(
        _test_ty(
            "
            external func x(): {
                Bool,
                Int8,
                Int16,
                Int32,
                Int64,
                Uint8,
                Uint16,
                Uint32,
                Uint64,
                Float32,
                Float64,
                Text,
            }
            func main() -> x()
            "
        ),
        @"{Bool, Int8, Int16, Int32, Int64, Uint8, Uint16, Uint32, Uint64, Float32, Float64, Text}"
    );
}

#[test]
fn enums_00() {
    insta::assert_snapshot!(
        _test_ty(
            "
            type Status: enum { Open, done, pending: Text }

            func main() -> .done: Status
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
            type Status: enum { Open, done, pending: Text }

            func main() -> .pending("hello"): Status
            "#
        ),
        @"Status"
    );
}

#[test]
fn enums_02() {
    // TODO: this error message is confusing, because it is backwards
    // It does make *some* amount of sense: we constructed a value that has the
    // type `enum {.pending: {}, ..}`. What's failing is applying type
    // annotation to it. So, our value expected annotation to have type text.
    insta::assert_snapshot!(_test_err(
        r#"
        type Status: enum { Open, done, pending: Text }

        func main() -> .pending: Status
        "#
    ), @"
    [E0006] Error:
       ╭─[ <unknown>:4:24 ]
       │
     4 │         func main() -> .pending: Status
       │                        ────┬───
       │                            ╰───── expected type `{}`, but found type `Text`
    ───╯
    ");
}

#[test]
fn recursive_00() {
    insta::assert_snapshot!(_test_err(
        r#"
        type Tree1: {left: Tree1, right: Tree1}

        type Tree2: [Tree2]

        type Tree3: Tree3

        type Tree4: enum {none, some: Tree4}
        "#
    ), @"
    Error:
       ╭─[ <unknown>:2:9 ]
       │
     2 │         type Tree1: {left: Tree1, right: Tree1}
       │         ───────────────────┬───────────────────
       │                            ╰───────────────────── type has infinite size
       │
       │ Note: self references are allowed only from within arrays or enums
    ───╯
    Error:
       ╭─[ <unknown>:6:9 ]
       │
     6 │         type Tree3: Tree3
       │         ────────┬────────
       │                 ╰────────── type has infinite size
       │
       │ Note: self references are allowed only from within arrays or enums
    ───╯
    "
    );
}

#[test]
fn recursive_01() {
    insta::assert_snapshot!(_test_err(
        r#"
        type Tree1: enum {leaf: Int32, fork: Branches1}
        type Branches1: {left: Tree1, right: Tree1}

        type Tree2: {Branches2}
        type Branches2: {left: Tree2, right: Tree2}
        "#
    ), @"
    Error:
       ╭─[ <unknown>:5:9 ]
       │
     5 │         type Tree2: {Branches2}
       │         ───────────┬───────────
       │                    ╰───────────── type has infinite size
       │
       │ Note: recursive references are allowed only from within arrays or enums
    ───╯
    "
    );
}

#[test]
fn match_00() {
    insta::assert_snapshot!(_test_ty(r#"
        type Status: enum {done, pending: Int16, cancelled: Text}

        func main() -> match .done: Status {
          .done => "done",
          .pending => "pending",
          .cancelled => "cancelled",
        }
    "#), @"Text");
}

#[test]
#[ignore] // TODO
fn match_01() {
    insta::assert_snapshot!(_test_err(r#"
        type Status: enum {done, pending: Int16, cancelled: Text}

        func main() -> match .done: Status {
          .done => "done",
          .cancelled => "cancelled",
        }
    "#), @r#"
        Variant .pending not covered.
    "#);
}

#[test]
fn match_02() {
    insta::assert_snapshot!(_test_err(r#"
        type Status: enum {done, pending: Int16, cancelled: Text}
        type Color: enum {red, green, blue}

        func main() -> match .done: Status {
          .red => "red",
          .green => "green",
          .blue => "blue",
        }
    "#), @r#"
    Error:
       ╭─[ <unknown>:6:11 ]
       │
     6 │           .red => "red",
       │           ──┬─
       │             ╰─── variant does not exist
    ───╯
    "#);
}

#[test]
fn match_03() {
    insta::assert_snapshot!(_test_err(r#"
        type Color: enum {red, green, blue}

        func main() -> match .green: Color {
          .red => "red",
          .green => 1,
          .blue => false,
        }
    "#), @"
    [E0006] Error:
       ╭─[ <unknown>:7:20 ]
       │
     7 │           .blue => false,
       │                    ──┬──
       │                      ╰──── match expected type `Text`, but found type `Bool`
    ───╯
    ");
}

#[test]
fn match_04() {
    insta::assert_snapshot!(_test_ty(r#"
        type Status: enum {pending: Bool}

        func main() -> match .pending(false): Status {
          .pending(x) => x,
        }
    "#), @"Bool");
}

#[test]
fn match_05() {
    insta::assert_snapshot!(_test_ty(r#"
        type Status: enum {pending: Int32}

        func main() -> match .pending(4): Status {
          .pending(x) => x,
        }
    "#), @"Int32");
}

#[test]
fn match_06() {
    insta::assert_snapshot!(_test_err(r#"
        type Animal: enum {cat: Text, dog: Bool}

        func main(): [Text] -> match .cat: Animal {
          .cat(name) | .dog(is_vaccinated) => true,
        }
    "#), @"
    Error:
       ╭─[ <unknown>:5:24 ]
       │
     5 │           .cat(name) | .dog(is_vaccinated) => true,
       │                        ─────────┬─────────
       │                                 ╰─────────── patterns introduce different variable names
    ───╯
    ");
}

#[test]
fn match_07() {
    insta::assert_snapshot!(_test_err(r#"
        type Animal: enum {
          cat: Text,
          dog: Bool,
        }

        func main(): Text -> match .cat: Animal {
          .cat(name) | .dog(name) => name,
        }
    "#), @"
    [E0006] Error:
       ╭─[ <unknown>:8:29 ]
       │
     8 │           .cat(name) | .dog(name) => name,
       │                             ──┬─
       │                               ╰─── expected type `Text`, but found type `Bool`
    ───╯
    ");
}

#[test]
fn func_param_00() {
    insta::assert_snapshot!(_test_ty(r#"
        func main() -> (
            false | a -> !a
        )
    "#), @"Bool");
}

#[test]
fn func_param_01() {
    insta::assert_snapshot!(_test_ty(r#"
        func main(): Int16 -> (
            3 | x -> x * x
        )
    "#), @"Int16");
}

#[test]
fn func_param_02() {
    insta::assert_snapshot!(_test_ty(r#"
        func main(): [Int16] -> (
            [3, 2, 4, 1, 5, -3, 1]
            | std::map(x -> -x)
        )
    "#), @"[Int16]");
}

#[test]
fn func_param_03() {
    insta::assert_snapshot!(_test_ty(r#"
        func main() -> (
            {a = 3, b = 5: Int16} | x -> x.a + x.b
        )
    "#), @"Int16");
}

#[test]
fn defs_00() {
    insta::assert_snapshot!(_test_err(r#"
        const a = 3
        const a = 6
    "#), @"
    [E0003] Error:
       ╭─[ <unknown>:3:9 ]
       │
     3 │         const a = 6
       │         ─────┬─────
       │              ╰─────── duplicate name
    ───╯
    [E0003] Error:
       ╭─[ <unknown>:2:9 ]
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
    "#), @"
    [E0003] Error:
       ╭─[ <unknown>:3:9 ]
       │
     3 │         func a() -> 6
       │         ──────┬──────
       │               ╰──────── duplicate name
    ───╯
    [E0003] Error:
       ╭─[ <unknown>:2:9 ]
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
        const b = [6: Int64, 2 + 6]
        const c = 5: Int64
        const d = [1, c]
    "#), @"
    Error:
       ╭─[ <unknown>:2:27 ]
       │
     2 │         const a = {false, true || false}
       │                           ──────┬──────
       │                                 ╰──────── non-constant expression
       │
       │ Note: use `func` instead of `const`
    ───╯
    Error:
       ╭─[ <unknown>:3:30 ]
       │
     3 │         const b = [6: Int64, 2 + 6]
       │                              ──┬──
       │                                ╰──── non-constant expression
       │
       │ Note: use `func` instead of `const`
    ───╯
    ");
}
#[test]
fn constants_01() {
    insta::assert_snapshot!(_test_err(r#"
        const a = func () -> true
    "#), @"
    Error:
       ╭─[ <unknown>:2:19 ]
       │
     2 │         const a = func () -> true
       │                   ───────┬───────
       │                          ╰───────── non-constant expression
       │
       │ Note: use `func` instead of `const`
    ───╯
    ");
}
#[test]
fn constants_02() {
    insta::assert_snapshot!(_test_err(r#"
        func main(table_name: Text): [{Bool}] -> std::sql::from(table_name)
    "#), @"
    Error:
       ╭─[ <unknown>:2:65 ]
       │
     2 │         func main(table_name: Text): [{Bool}] -> std::sql::from(table_name)
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
    "#), @"{Bool, Bool, Text, Text}");
}
#[test]
fn unpack_01() {
    insta::assert_snapshot!(_test_err(r#"
        const main = {false, ..true, "hello"}
    "#), @r#"
    [E0006] Error:
       ╭─[ <unknown>:2:32 ]
       │
     2 │         const main = {false, ..true, "hello"}
       │                                ──┬─
       │                                  ╰─── only tuples can be unpacked
       │
       │ Note: got type Bool
    ───╯
    "#);
}
#[test]
fn unpack_02() {
    insta::assert_snapshot!(_test_ty(r#"
        type A: {Int32, Int32}
        const x: A = {45, 6}
        const main = {false, ..x, "hello"}
    "#), @"{Bool, Int32, Int32, Text}");
}
#[test]
fn unpack_03() {
    insta::assert_snapshot!(_test_ty(r#"
        func identity(x: T): T
        where T: {..}
        -> x

        func main() -> {a = 4: Int32, ..identity({true, false}), b = false}
    "#), @"{a: Int32, Bool, Bool, b: Bool}");
}
#[test]
fn unpack_04() {
    insta::assert_snapshot!(_test_ty(r#"
        func false_x_false(x: T)
        where T: {..}
        -> {false, ..x, false}

        func main() -> false_x_false({"a", "b"})
    "#), @"{Bool, Text, Text, Bool}");
}

#[test]
fn unpack_05() {
    insta::assert_snapshot!(_test_ty(r#"
        const main = {
          a = 4: Int32,
          ..{
            x1 = 3: Int32,
            x2 = "hello",
          },
          ..{
            x3 = "world",
          },
          b = false
        }.x3
    "#), @"Text");
}

#[test]
fn unpack_06() {
    // lookup into an unpack of a type var

    insta::assert_snapshot!(_test_err(r#"
      func main() -> (
        {x1 = true, x2 = false}
        | x -> {4: Int32, ..x, "hello"}.x2
      )
    "#), @r#"
    Error:
       ╭─[ <unknown>:4:40 ]
       │
     4 │         | x -> {4: Int32, ..x, "hello"}.x2
       │                                        ─┬─
       │                                         ╰─── ambiguous lookup into unpack of an unknown type
       │
       │ Note: consider annotating the unpacked expression
    ───╯
    "#);
}

#[test]
fn unpack_11() {
    insta::assert_snapshot!(_test_err(r#"
    func identity(x: T): T where T: {..} -> x
    func main() -> {4: Int32, ..identity({true, false}), "hello"}.2
    "#), @r#"
    Error:
       ╭─[ <unknown>:3:66 ]
       │
     3 │     func main() -> {4: Int32, ..identity({true, false}), "hello"}.2
       │                                                                  ─┬
       │                                                                   ╰── ambiguous lookup into unpack of an unknown type
       │
       │ Note: consider annotating the unpacked expression
    ───╯
    "#);
}

#[test]
fn unpack_12() {
    insta::assert_snapshot!(_test_ty(r#"
    func identity(x: T): T where T: {..} -> x
    func main() -> {4: Int32, ..identity({true, false}): {Bool, Bool}, "hello"}.2
    "#), @"Bool");
}

#[test]
fn unpack_13() {
    insta::assert_snapshot!(_test_err(r#"
    func false_x_false(x: T) where T: {x2: Text, ..} -> {false, ..x, false}.2
    func main() -> false_x_false({true, x2 = "hello"})
    "#), @"
    Error:
       ╭─[ <unknown>:2:76 ]
       │
     2 │     func false_x_false(x: T) where T: {x2: Text, ..} -> {false, ..x, false}.2
       │                                                                            ─┬
       │                                                                             ╰── cannot do positional lookup into unpack of this type param
    ───╯
    ");
}

#[test]
fn unpack_14() {
    insta::assert_snapshot!(_test_ty(r#"
    func false_x_false(x: T) where T: {Bool, Text, ..} -> {false, ..x, false}.2
    func main() -> false_x_false({true, "hello"})
    "#), @"Text");
}

#[test]
fn ty_tuple_comprehension_00() {
    // map to a simple type

    insta::assert_snapshot!(_test_ty(r#"
    type A: {id: Int64, title: Text}

    func main(): {for f: F in A do Bool}
    -> std::default()
    "#), @"{Bool, Bool}");
}

#[test]
fn ty_tuple_comprehension_01() {
    // map to a simple type

    insta::assert_snapshot!(_test_ty(r#"
    type A: {id: Int64, title: Text}

    func main(): {for f: F in A do f: Bool}
    -> std::default()
    "#), @"{id: Bool, title: Bool}");
}

#[test]
fn ty_tuple_comprehension_01a() {
    // bad body field name

    insta::assert_snapshot!(_test_err(r#"
    type A: {id: Int64, title: Text}

    func main(): {for f: F in A do g: Bool}
    -> std::default()
    "#), @"
    Error:
       ╭─[ <unknown>:4:5 ]
       │
     4 │ ╭─▶     func main(): {for f: F in A do g: Bool}
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
    type A: {id: Int64, title: Text}

    func main(): {for f: F in A do f: [Bool]}
    -> std::default()
    "#), @"{id: [Bool], title: [Bool]}");
}

#[test]
fn ty_tuple_comprehension_03() {
    // lookup in tuple comprehension

    insta::assert_snapshot!(_test_ty(r#"
    type A: {id: Int64, title: Text}
    type B: {for f: F in A do f: Bool}

    func f(flags: B): Bool -> flags.id && flags.title

    func main() -> f({id = true, title = false})
    "#), @"Bool");
}

#[test]
fn ty_tuple_comprehension_04() {
    // remove tuple names

    insta::assert_snapshot!(_test_ty(r#"
    type A: {id: Int64, title: Text}

    func main(): {for f: F in A do F}
    -> std::default()
    "#), @"{Int64, Text}");
}

#[test]
fn ty_tuple_comprehension_05() {
    // wrap into array

    insta::assert_snapshot!(_test_ty(r#"
    type A: {id: Int64, title: Text}

    func main(): {for f: F in A do f: [F]}
    -> std::default()
    "#), @"{id: [Int64], title: [Text]}");
}

#[test]
fn ty_tuple_comprehension_06() {
    // validate tuples against comprehension

    insta::assert_snapshot!(_test_ty(r#"
    func make_flags(x: T): {for f: F in T do f: Bool}
    where T: {..}
    -> std::default()

    type A: {id: Int64, title: Text}

    func main(a: A): {id: Bool, title: Bool} -> make_flags(a)
    "#), @"{id: Bool, title: Bool}");

    insta::assert_snapshot!(_test_err(r#"
    func make_flags(x: T): {for f: F in T do f: Bool}
    where T: {..}
    -> std::default()

    type A: {id: Int64, title: Text}

    func main(a: A): {id: Bool, title: Bool, release_year: Bool} -> make_flags(a)
    "#), @"
    [E0006] Error:
       ╭─[ <unknown>:8:60 ]
       │
     8 │     func main(a: A): {id: Bool, title: Bool, release_year: Bool} -> make_flags(a)
       │                                                            ──┬─
       │                                                              ╰─── field .release_year does not exist in type {id: Int64, title: Text}
    ───╯
    ");

    insta::assert_snapshot!(_test_err(r#"
    func make_flags(x: T): {for f: F in T do f: Bool}
    where T: {..}
    -> std::default()

    type A: {id: Int64, title: Text, release_year: Int16}

    func main(a: A): {id: Bool, title: Bool} -> make_flags(a)
    "#), @"
    [E0007] Error:
       ╭─[ <unknown>:8:49 ]
       │
     8 │     func main(a: A): {id: Bool, title: Bool} -> make_flags(a)
       │                                                 ─────┬────
       │                                                      ╰────── expected a tuple with 2 fields, found {id: Int64, title: Text, release_year: Int16}
    ───╯
    ");
}

#[test]
fn ty_tuple_comprehension_07() {
    // validate tuples against comprehension

    insta::assert_snapshot!(_test_ty(r#"
    type A: {id: Int64, title: Text}
    func main(a: {id: Bool, title: Bool}): {for f: F in A do f: Bool} -> a
    "#), @"{id: Bool, title: Bool}");
}

#[test]
fn ty_tuple_comprehension_08() {
    // lookup into comprehension

    insta::assert_snapshot!(_test_ty(r#"
    type A: {id: Int64, title: Text}
    func main(a: {for f: F in A do f: {F, F}}) -> a.title
    "#), @"{Text, Text}");
}

#[test]
fn ty_tuple_comprehension_09() {
    // validation of comprehension

    // I'm not really sure why does this test case require the intermediate
    // function. It was derived from a panic when dealing with to_columnar and
    // from_columnar. It was fixed by adding `LocalTyInliner`.

    insta::assert_snapshot!(_test_ty(r#"
    external func from_columnar(columnar: {for f: F in T do f: [F]}): [T]
    where T: {..}

    func main() -> (
      {gift_id = []}: {gift_id: [Int32]}
      | x -> {x.gift_id, x.gift_id}
      | from_columnar
    )
    "#), @"[{gift_id: Int32, gift_id: Int32}]");

    // A few useful functions for writing tests:

    // func each_once(a: A): {for f: F in A do f: [F]}
    // where A: {..}

    // func first(t: [T]): T
    // where T
}

#[test]
fn ty_tuple_comprehension_10() {
    // comprehension yields named fields

    insta::assert_snapshot!(_test_ty(r#"
    external func from_columnar(columnar: {for f: F in T do f: [F]}): T
    where T: {..}

    func main() -> (
      ({gift_id = []: [Int32]} | from_columnar)
    )
    "#), @"{gift_id: Int32}");
}

#[test]
fn tuple_00() {
    // tuples, named type validation

    insta::assert_snapshot!(_test_ty(r#"
    func main(): {title: Text, is_released: Bool} -> {title = "hello", is_released = true}
    "#), @"{title: Text, is_released: Bool}");

    insta::assert_snapshot!(_test_err(r#"
    func main(): {title: Text, is_released: Bool} -> {title = "hello"}
    "#), @r#"
    [E0006] Error:
       ╭─[ <unknown>:2:54 ]
       │
     2 │     func main(): {title: Text, is_released: Bool} -> {title = "hello"}
       │                                                      ────────┬────────
       │                                                              ╰────────── expected type `{title: Text, is_released: Bool}`, but found type `{title: Text}`
    ───╯
    "#);

    insta::assert_snapshot!(_test_err(r#"
    func main(): {title: Text} -> {title = "hello", is_released = true}
    "#), @r#"
    [E0006] Error:
       ╭─[ <unknown>:2:35 ]
       │
     2 │     func main(): {title: Text} -> {title = "hello", is_released = true}
       │                                   ──────────────────┬──────────────────
       │                                                     ╰──────────────────── expected type `{title: Text}`, but found type `{title: Text, is_released: Bool}`
    ───╯
    "#);
}

#[test]
fn tuple_01() {
    // tuples, positional type validation

    insta::assert_snapshot!(_test_ty(r#"
    func main(): {Text, Bool} -> {"hello", true}
    "#), @"{Text, Bool}");

    insta::assert_snapshot!(_test_err(r#"
    func main(): {Text, Bool} -> {"hello"}
    "#), @r#"
    [E0006] Error:
       ╭─[ <unknown>:2:34 ]
       │
     2 │     func main(): {Text, Bool} -> {"hello"}
       │                                  ────┬────
       │                                      ╰────── expected type `{Text, Bool}`, but found type `{Text}`
    ───╯
    "#);

    insta::assert_snapshot!(_test_err(r#"
    func main(): {Text} -> {"hello", true}
    "#), @r#"
    [E0006] Error:
       ╭─[ <unknown>:2:28 ]
       │
     2 │     func main(): {Text} -> {"hello", true}
       │                            ───────┬───────
       │                                   ╰───────── expected type `{Text}`, but found type `{Text, Bool}`
    ───╯
    "#);
}

#[test]
fn tuple_02() {
    // tuples, positional type variation of named tuples

    insta::assert_snapshot!(_test_ty(r#"
    func main(): {Text, Bool} -> {title = "hello", is_released = true}
    "#), @"{Text, Bool}");

    insta::assert_snapshot!(_test_err(r#"
    func main(): {Text, Bool} -> {title = "hello"}
    "#), @r#"
    [E0006] Error:
       ╭─[ <unknown>:2:34 ]
       │
     2 │     func main(): {Text, Bool} -> {title = "hello"}
       │                                  ────────┬────────
       │                                          ╰────────── expected type `{Text, Bool}`, but found type `{title: Text}`
    ───╯
    "#);

    insta::assert_snapshot!(_test_err(r#"
    func main(): {Text} -> {title = "hello", is_released = true}
    "#), @r#"
    [E0006] Error:
       ╭─[ <unknown>:2:28 ]
       │
     2 │     func main(): {Text} -> {title = "hello", is_released = true}
       │                            ──────────────────┬──────────────────
       │                                              ╰──────────────────── expected type `{Text}`, but found type `{title: Text, is_released: Bool}`
    ───╯
    "#);
}

#[test]
fn tuple_03() {
    // tuples, named type validation of unnamed tuples

    insta::assert_snapshot!(_test_ty(r#"
    func main(): {title: Text, is_released: Bool} -> {"hello", true}
    "#), @"{title: Text, is_released: Bool}");

    insta::assert_snapshot!(_test_err(r#"
    func main(): {title: Text, is_released: Bool} -> {"hello"}
    "#), @r#"
    [E0006] Error:
       ╭─[ <unknown>:2:54 ]
       │
     2 │     func main(): {title: Text, is_released: Bool} -> {"hello"}
       │                                                      ────┬────
       │                                                          ╰────── expected type `{title: Text, is_released: Bool}`, but found type `{Text}`
    ───╯
    "#);

    insta::assert_snapshot!(_test_err(r#"
    func main(): {title: Text} -> {"hello", true}
    "#), @r#"
    [E0006] Error:
       ╭─[ <unknown>:2:35 ]
       │
     2 │     func main(): {title: Text} -> {"hello", true}
       │                                   ───────┬───────
       │                                          ╰───────── expected type `{title: Text}`, but found type `{Text, Bool}`
    ───╯
    "#);
}

#[test]
fn tuple_04() {
    // tuples, multiple diagnostics

    insta::assert_snapshot!(_test_err(r#"
    func main(): {title: Bool, is_released: Bool} -> {"hello", "world"}
    "#), @r#"
    [E0006] Error:
       ╭─[ <unknown>:2:55 ]
       │
     2 │     func main(): {title: Bool, is_released: Bool} -> {"hello", "world"}
       │                                                       ───┬───
       │                                                          ╰───── expected type `Bool`, but found type `Text`
    ───╯
    [E0006] Error:
       ╭─[ <unknown>:2:64 ]
       │
     2 │     func main(): {title: Bool, is_released: Bool} -> {"hello", "world"}
       │                                                                ───┬───
       │                                                                   ╰───── expected type `Bool`, but found type `Text`
    ───╯
    "#);
}

#[test]
fn import_simple() {
    // Basic imports: single item, aliased item, and aliased type
    insta::assert_snapshot!(_test_ty(r#"
    module a {
      func f() -> "f"

      const c = "c"

      type T: Text
    }

    import a::f
    import a::c as d
    import a::T as U

    func main() -> {d, f()}: {a::T, U}
    "#), @"{a::T, a::T}");
}

#[test]
fn import_cross_module_chain() {
    // Imports that form a chain across module boundaries
    // d1 <- d2 <- d3 <- c, all via imports across submodule and parent
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
    "#), @"Text");
}

#[test]
fn import_recursive_error() {
    // Importing a module that references itself should be an error
    insta::assert_snapshot!(_test_err(r#"
    module a {
      import project::a::b as b
    }
    func main() -> {}
    "#), @"
    Error:
       ╭─[ <unknown>:3:14 ]
       │
     3 │       import project::a::b as b
       │              ─────────┬────────
       │                       ╰────────── recursive reference
    ───╯
    ");
}

#[test]
fn import_module_alias() {
    // A whole module can be imported under an alias and its members accessed via alias
    insta::assert_snapshot!(_test_ty(r#"
    module a {
      module b {
        const c = "hello"
      }
      const d = false
    }
    import a as e
    func main() -> {e::d, e::b::c}
    "#), @"{d: Bool, c: Text}");
}

#[test]
fn import_module_as_value_error() {
    // A module cannot be used as an expression or type directly
    insta::assert_snapshot!(_test_err(r#"
    module a {}
    func main() -> a
    "#), @"
    [E0005] Error:
       ╭─[ <unknown>:3:20 ]
       │
     3 │     func main() -> a
       │                    ┬
       │                    ╰── expected a value, found a module
    ───╯
    ");
    insta::assert_snapshot!(_test_err(r#"
    module a {}
    func main(): a -> 1
    "#), @"
    [E0005] Error:
       ╭─[ <unknown>:3:18 ]
       │
     3 │     func main(): a -> 1
       │                  ┬
       │                  ╰── expected a type, found a module
    ───╯
    ");
}

#[test]
fn import_multiple_grouped() {
    // Multiple items from the same module imported with grouping syntax
    insta::assert_snapshot!(_test_ty(r#"
    module a {
      const b = "b"
      const c = false
    }
    import a::(b, c)
    func main() -> {b, c}
    "#), @"{b: Text, c: Bool}");
}

#[test]
fn import_nested_grouped() {
    // Nested grouping imports items from a submodule inline
    insta::assert_snapshot!(_test_ty(r#"
    module a {
      const b = "b"
      module c {
        const d: Int16 = 3
        const e = false
      }
    }
    import a::(b, c::(d, e))
    func main() -> {b, d, e}
    "#), @"{b: Text, d: Int16, e: Bool}");
}

#[test]
fn import_star_basic() {
    // Star import brings all defs from a module
    insta::assert_snapshot!(_test_ty(r#"
    module a {
      func f() -> "f"
      const c = "c"
      type T: Text
    }
    import a::*
    func main() -> {c, f()}: {T, T}
    "#), @"{a::T, a::T}");
}

#[test]
fn import_star_nested_module() {
    // Star import from a nested module
    insta::assert_snapshot!(_test_ty(r#"
    module a {
      module b {
        const x = "hello"
        const y = "world"
      }
    }
    import a::b::*
    func main() -> {x, y}
    "#), @"{x: Text, y: Text}");
}

#[test]
fn import_star_includes_submodules() {
    // Star import includes submodules
    insta::assert_snapshot!(_test_ty(r#"
    module a {
      module inner {
        const val = "hello"
      }
    }
    import a::*
    func main() -> inner::val
    "#), @"Text");
}

#[test]
fn import_star_shadowed_by_explicit() {
    // Explicit definitions shadow star imports
    insta::assert_snapshot!(_test_ty(r#"
    module a {
      const x = "original"
    }
    import a::*
    const x = true
    func main() -> x
    "#), @"Bool");
}

#[test]
fn import_star_no_reexports() {
    // Star imports include re-exports from the target module
    insta::assert_snapshot!(_test_ty(r#"
    module inner {
      const x = true
    }
    module a {
      import project::inner::x
    }
    import a::*
    func main() -> x
    "#), @"Bool");
}

#[test]
fn import_star_missing_module() {
    // Error when star importing from non-existent module
    insta::assert_snapshot!(_test_err(r#"
    import nonexistent::*
    func main() -> 1
    "#), @"
    Error:
       ╭─[ <unknown>:2:12 ]
       │
     2 │     import nonexistent::*
       │            ───────┬──────
       │                   ╰──────── name does not exist
    ───╯
    ");
}

#[test]
fn framed_00() {
    insta::assert_snapshot!(_test_err(r#"
    type MyInt(Int32)
    const x: MyInt = 4: Int32
    "#), @"
    [E0006] Error:
       ╭─[ <unknown>:3:22 ]
       │
     3 │     const x: MyInt = 4: Int32
       │                      ────┬───
       │                          ╰───── x expected type `MyInt`, but found type `Int32`
    ───╯
    ");
}

#[test]
fn framed_01() {
    insta::assert_snapshot!(_test_ty(r#"
    type MyInt(Int32)
    const main = MyInt(4: Int32)
    "#), @"MyInt");
}

#[test]
fn framed_02() {
    insta::assert_snapshot!(_test_err(r#"
    type MyInt(Int32)
    const main = MyInt(4: Int32, false)
    "#), @"
    Error:
       ╭─[ <unknown>:3:18 ]
       │
     3 │     const main = MyInt(4: Int32, false)
       │                  ───────────┬──────────
       │                             ╰──────────── func MyInt expected 1 arguments, but got 2
    ───╯
    ");
}

#[test]
fn framed_03() {
    insta::assert_snapshot!(_test_err(r#"
    type MyInt(Int32)
    const main = MyInt()
    "#), @"
    Error:
       ╭─[ <unknown>:3:18 ]
       │
     3 │     const main = MyInt()
       │                  ───┬───
       │                     ╰───── func MyInt expected 1 arguments, but got 0
    ───╯
    ");
}

#[test]
fn framed_04() {
    insta::assert_snapshot!(_test_err(r#"
    type MyInt(Int32)
    const main = MyInt(false)
    "#), @"
    [E0006] Error:
       ╭─[ <unknown>:3:24 ]
       │
     3 │     const main = MyInt(false)
       │                        ──┬──
       │                          ╰──── func MyInt expected type `MyInt`, but found type `Bool`
       │
       │ Note: type `MyInt` expands to `Int32`
    ───╯
    ");
}

#[test]
fn framed_05() {
    insta::assert_snapshot!(_test_ty(r#"
    type MyInt(Int32)
    const main = MyInt(12).0
    "#), @"Int32");
}

#[test]
fn framed_06() {
    insta::assert_snapshot!(_test_err(r#"
    type MyInt(Int32)
    const main = MyInt(12).1
    "#), @"
    [E0006] Error:
       ╭─[ <unknown>:3:27 ]
       │
     3 │     const main = MyInt(12).1
       │                           ─┬
       │                            ╰── field .1 does not exist in type MyInt
       │
       │ Note: MyInt is a framed type. Inner value can be accessed with `.0`
    ───╯
    ");
}

#[test]
fn framed_07() {
    insta::assert_snapshot!(_test_err(r#"
    type MyInt(Int32)
    const main = MyInt(12).inner
    "#), @"
    [E0006] Error:
       ╭─[ <unknown>:3:27 ]
       │
     3 │     const main = MyInt(12).inner
       │                           ───┬──
       │                              ╰──── field .inner does not exist in type MyInt
       │
       │ Note: MyInt is a framed type. Inner value can be accessed with `.0`
    ───╯
    ");
}

#[test]
#[ignore] // Issue #88
fn framed_08() {
    insta::assert_snapshot!(_test_ty(r#"
    type Status(enum {pending: Text, done})

    func main() -> (
      let s = Status(.pending("hello"));
      match s {
        Status(.pending(x)) => x,
        Status(.done) => "",
      }
    )
    "#), @r"
    ");
}

#[test]
fn framed_09() {
    // framed labelled type, construction

    insta::assert_snapshot!(_test_ty(r#"
    type Date(days_since_epoch: Int32)

    func main() -> {
      Date(12),
      Date(days_since_epoch = 12),
    }
    "#), @"{Date, Date}");
}

#[test]
fn framed_10() {
    // framed labelled type, lookup

    insta::assert_snapshot!(_test_ty(r#"
    type Date(days_since_epoch: Int32)

    func main() -> {
      Date(12).0,
      Date(12).days_since_epoch,
    }
    "#), @"{Int32, days_since_epoch: Int32}");
}

#[test]
fn framed_11() {
    // framed labelled type, err messages

    insta::assert_snapshot!(_test_err(r#"
    type Date(days_since_epoch: Int32)

    func main() -> {
      Date(wrong = 12),
      Date(days_since_epoch = false),
      Date(12).1,
      Date(12).wrong,
    }
    "#), @"
    Error:
       ╭─[ <unknown>:5:12 ]
       │
     5 │       Date(wrong = 12),
       │            ─────┬────
       │                 ╰────── unknown parameter `wrong`
    ───╯
    [E0006] Error:
       ╭─[ <unknown>:6:31 ]
       │
     6 │       Date(days_since_epoch = false),
       │                               ──┬──
       │                                 ╰──── func Date expected type `Date`, but found type `Bool`
       │
       │ Note: type `Date` expands to `Int32`
    ───╯
    [E0006] Error:
       ╭─[ <unknown>:7:15 ]
       │
     7 │       Date(12).1,
       │               ─┬
       │                ╰── field .1 does not exist in type Date
       │
       │ Note: Date is a framed type. Inner value can be accessed with `.days_since_epoch` or `.0`
    ───╯
    [E0006] Error:
       ╭─[ <unknown>:8:15 ]
       │
     8 │       Date(12).wrong,
       │               ───┬──
       │                  ╰──── field .wrong does not exist in type Date
       │
       │ Note: Date is a framed type. Inner value can be accessed with `.days_since_epoch` or `.0`
    ───╯
    ");
}

#[test]
fn framed_12() {
    // framed type, as type var

    insta::assert_snapshot!(_test_ty(r#"
    type Date(days_since_epoch: Int32)

    func main() -> {
      Date(12) | x -> x.0,
    }
    "#), @"{Int32}");
}

#[test]
fn call_00() {
    // calls desugar-ed from pipelines should have span on the right operand

    insta::assert_snapshot!(_test_err(r#"
    const a = true | std::and(false, false)
    "#), @"
    Error:
       ╭─[ <unknown>:2:22 ]
       │
     2 │     const a = true | std::and(false, false)
       │                      ───────────┬──────────
       │                                 ╰──────────── func std::and expected 2 arguments, but got 3
    ───╯
    Error:
       ╭─[ <unknown>:2:22 ]
       │
     2 │     const a = true | std::and(false, false)
       │                      ───────────┬──────────
       │                                 ╰──────────── non-constant expression
       │
       │ Note: use `func` instead of `const`
    ───╯
    ");
}

#[test]
fn call_01() {
    // labelled call

    insta::assert_snapshot!(_test_ty(r#"
    func noop(a x: Int32) -> x

    func main() -> noop(a = 4)
    "#), @"Int32");
}

#[test]
fn call_02() {
    // call, bad arg label

    insta::assert_snapshot!(_test_err(r#"
    func noop(a x: Int32, y: Bool) -> x

    func main() -> noop(true, z = false)
    "#), @"
    Error:
       ╭─[ <unknown>:4:31 ]
       │
     4 │     func main() -> noop(true, z = false)
       │                               ────┬────
       │                                   ╰────── unknown parameter `z`
    ───╯
    [E0006] Error:
       ╭─[ <unknown>:4:25 ]
       │
     4 │     func main() -> noop(true, z = false)
       │                         ──┬─
       │                           ╰─── func noop expected type `Int32`, but found type `Bool`
    ───╯
    ");
}

#[test]
fn call_03() {
    // call, not enough args

    insta::assert_snapshot!(_test_err(r#"
    func noop(a x: Int32, y: Bool) -> x

    func main() -> noop(true)
    "#), @"
    Error:
       ╭─[ <unknown>:4:20 ]
       │
     4 │     func main() -> noop(true)
       │                    ─────┬────
       │                         ╰────── func noop expected 2 arguments, but got 1
    ───╯
    [E0006] Error:
       ╭─[ <unknown>:4:25 ]
       │
     4 │     func main() -> noop(true)
       │                         ──┬─
       │                           ╰─── func noop expected type `Int32`, but found type `Bool`
    ───╯
    ");
}

#[test]
fn call_04() {
    // call, too many args

    insta::assert_snapshot!(_test_err(r#"
    func noop(a x: Int32, y: Bool) -> x

    func main() -> noop(3, "hello", false)
    "#), @r#"
    Error:
       ╭─[ <unknown>:4:20 ]
       │
     4 │     func main() -> noop(3, "hello", false)
       │                    ───────────┬───────────
       │                               ╰───────────── func noop expected 2 arguments, but got 3
    ───╯
    [E0006] Error:
       ╭─[ <unknown>:4:28 ]
       │
     4 │     func main() -> noop(3, "hello", false)
       │                            ───┬───
       │                               ╰───── func noop expected type `Bool`, but found type `Text`
    ───╯
    "#);
}

#[test]
fn anno_00_happy_path() {
    insta::assert_snapshot!(_test_ty(r#"
    anno deprecated(reason: Text)

    @deprecated("use new_api instead")
    const main: Int64 = 1
    "#), @"Int64");
}

#[test]
fn anno_01_unknown() {
    insta::assert_snapshot!(_test_err(r#"
    @nonexistent
    const main: Int64 = 1
    "#), @"
    Error:
       ╭─[ <unknown>:2:6 ]
       │
     2 │     @nonexistent
       │      ─────┬─────
       │           ╰─────── name does not exist
    ───╯
    ");
}

#[test]
fn anno_02_wrong_arg_type() {
    insta::assert_snapshot!(_test_err(r#"
    anno deprecated(reason: Text)

    @deprecated(false)
    const main: Int64 = 1
    "#), @"
    [E0006] Error:
       ╭─[ <unknown>:4:17 ]
       │
     4 │     @deprecated(false)
       │                 ──┬──
       │                   ╰──── expected type `Text`, but found type `Bool`
    ───╯
    ");
}

#[test]
fn anno_03_extra_args() {
    insta::assert_snapshot!(_test_err(r#"
    anno schema()

    @schema("extra")
    const main: Int64 = 1
    "#), @r#"
    Error:
       ╭─[ <unknown>:4:6 ]
       │
     4 │     @schema("extra")
       │      ───────┬───────
       │             ╰───────── expected 0 arguments, but got 1
    ───╯
    "#);
}

#[test]
fn anno_04_missing_args() {
    insta::assert_snapshot!(_test_err(r#"
    anno deprecated(reason: Text)

    @deprecated()
    const main: Int64 = 1
    "#), @"
    Error:
       ╭─[ <unknown>:4:6 ]
       │
     4 │     @deprecated()
       │      ──────┬─────
       │            ╰─────── expected 1 arguments, but got 0
    ───╯
    ");
}

#[test]
fn anno_05_non_const_arg() {
    insta::assert_snapshot!(_test_err(r#"
    anno deprecated(reason: Text)

    func reason(): Text -> "x"

    @deprecated(reason())
    const main: Int64 = 1
    "#), @"
    Error:
       ╭─[ <unknown>:6:17 ]
       │
     6 │     @deprecated(reason())
       │                 ────┬───
       │                     ╰───── non-constant expression
    ───╯
    ");
}

#[test]
fn types_56_std_eq_composites() {
    insta::assert_snapshot!(_test_ty(r#"
        type Color: enum {red, green, blue}

        func main() -> {
          {true, "aa"} == {true, "aa"},
          [1: Int32, 2, 3] == [1, 2, 3],
          (.red: Color) == (.green: Color),
        }
    "#), @"{Bool, Bool, Bool}");
}
