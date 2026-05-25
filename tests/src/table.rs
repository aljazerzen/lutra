//! Tests for table rendering.

use insta::assert_snapshot;
use lutra_bin::Value;
use lutra_tui::table;

#[track_caller]
fn _table(ty_src: &str, value: Value) -> String {
    let (ty, ty_defs) = lutra_compiler::_test_compile_ty(ty_src);
    let data = value.encode(&ty, &ty_defs).unwrap();

    table::Table::new(&data, &ty, &ty_defs)
        .render_once(Default::default())
        .to_string()
}

#[track_caller]
fn _table_with_config(ty_src: &str, value: Value, config: table::Config) -> String {
    let (ty, ty_defs) = lutra_compiler::_test_compile_ty(ty_src);
    let data = value.encode(&ty, &ty_defs).unwrap();

    table::Table::new(&data, &ty, &ty_defs)
        .render_once(config)
        .to_string()
}

#[test]
fn test_simple_flat_columns() {
    let result = _table(
        "[{a: Int32, b: Text}]",
        Value::Array(vec![
            Value::Tuple(vec![Value::Prim32(42), Value::new_text("hello")]),
            Value::Tuple(vec![Value::Prim32(100), Value::new_text("world")]),
        ]),
    );

    assert_snapshot!(result, @"
          a b
      Int32 Text
    ─────────────
    0    42 hello
    1   100 world
    ");
}

#[test]
fn test_nested_tuple() {
    let result = _table(
        "[{id: Int32, address: {city: Text, zip: Int32}}]",
        Value::Array(vec![
            Value::Tuple(vec![
                Value::Prim32(1),
                Value::Tuple(vec![Value::new_text("NYC"), Value::Prim32(10001)]),
            ]),
            Value::Tuple(vec![
                Value::Prim32(2),
                Value::Tuple(vec![Value::new_text("LA"), Value::Prim32(90001)]),
            ]),
        ]),
    );

    assert_snapshot!(result, @"
         id  address
            city   zip
      Int32 Text Int32
    ──────────────────
    0     1 NYC  10001
    1     2 LA   90001
    ");
}

#[test]
fn test_array_expansion() {
    let result = _table(
        "[{name: Text, tags: [Text]}]",
        Value::Array(vec![
            Value::Tuple(vec![
                Value::new_text("Alice"),
                Value::Array(vec![Value::new_text("dev"), Value::new_text("rust")]),
            ]),
            Value::Tuple(vec![
                Value::new_text("Bob"),
                Value::Array(vec![Value::new_text("pm")]),
            ]),
        ]),
    );

    assert_snapshot!(result, @"
      name  tags
      Text  [Text]
    ──────────────
    0 Alice dev
            rust
    1 Bob   pm
    ");
}

#[test]
fn test_array_truncation() {
    let result = _table(
        "[{name: Text, tags: [Text]}]",
        Value::Array(vec![Value::Tuple(vec![
            Value::new_text("Alice"),
            Value::Array(vec![
                Value::new_text("a"),
                Value::new_text("b"),
                Value::new_text("c"),
                Value::new_text("d"),
                Value::new_text("e"),
            ]),
        ])]),
    );

    assert_snapshot!(result, @"
      name  tags
      Text  [Text]
    ────────────────
    0 Alice a
            b
            … 3 more
    ");
}

#[test]
fn test_non_flat_array() {
    let result = _table(
        "[{id: Int32, items: [{x: Int32}]}]",
        Value::Array(vec![
            Value::Tuple(vec![
                Value::Prim32(1),
                Value::Array(vec![Value::Tuple(vec![Value::Prim32(10)])]),
            ]),
            Value::Tuple(vec![
                Value::Prim32(2),
                Value::Array(vec![Value::Tuple(vec![Value::Prim32(20)])]),
            ]),
        ]),
    );

    assert_snapshot!(result, @"
         id items
      Int32 […]
    ─────────────
    0     1 […]
    1     2 […]
    ");
}

#[test]
fn test_enum_flat() {
    let result = _table(
        "[{status: enum{active, inactive}}]",
        Value::Array(vec![
            Value::Tuple(vec![Value::Enum(0, Box::new(Value::Tuple(vec![])))]),
            Value::Tuple(vec![Value::Enum(1, Box::new(Value::Tuple(vec![])))]),
        ]),
    );

    assert_snapshot!(result, @"
      status
      enum{active,inactive}
    ───────────────────────
    0 active
    1 inactive
    ");
}

#[test]
fn test_enum_with_flat_payload() {
    let result = _table(
        "[{opt: enum{none, some: Int32}}]",
        Value::Array(vec![
            Value::Tuple(vec![Value::Enum(0, Box::new(Value::Tuple(vec![])))]),
            Value::Tuple(vec![Value::Enum(1, Box::new(Value::Prim32(42)))]),
        ]),
    );

    assert_snapshot!(result, @"
         opt
      Int32?
    ────────
    0
    1     42
    ");
}

#[test]
fn test_text_truncation() {
    let config = table::Config {
        max_col_width: 10,
        max_array_items: 3,
        sample_rows: Some(100),
    };

    let result = _table_with_config(
        "[{msg: Text}]",
        Value::Array(vec![Value::Tuple(vec![Value::new_text(
            "This is a very long message that should be truncated",
        )])]),
        config,
    );
    assert_snapshot!(result, @"
      msg
      Text
    ────────────
    0 This is a…
    ");
}

#[test]
fn test_empty_array() {
    let result = _table("[{a: Int32}]", Value::Array(vec![]));

    assert_snapshot!(
        result,
        @"
          a
      Int32
    ───────
    "
    );
}

#[test]
fn test_single_tuple() {
    let result = _table(
        "{x: Int32, y: Text}",
        Value::Tuple(vec![Value::Prim32(42), Value::new_text("hello")]),
    );

    assert_snapshot!(result, @"
        x y
    Int32 Text
    ───────────
       42 hello
    ");
}

#[test]
fn test_alignment() {
    let result = _table(
        "[{name: Text, score: Int32, active: Bool}]",
        Value::Array(vec![
            Value::Tuple(vec![
                Value::new_text("Alice"),
                Value::Prim32(95),
                Value::Prim8(1),
            ]),
            Value::Tuple(vec![
                Value::new_text("Bob"),
                Value::Prim32(82),
                Value::Prim8(0),
            ]),
        ]),
    );

    assert_snapshot!(result, @"
      name  score active
      Text  Int32 Bool
    ────────────────────
    0 Alice    95 true
    1 Bob      82 false
    ");
}

#[test]
fn test_deeply_nested() {
    let result = _table(
        "[{a: {b: {c: Int32}}}]",
        Value::Array(vec![
            Value::Tuple(vec![Value::Tuple(vec![Value::Tuple(vec![Value::Prim32(
                1,
            )])])]),
            Value::Tuple(vec![Value::Tuple(vec![Value::Tuple(vec![Value::Prim32(
                2,
            )])])]),
        ]),
    );

    assert_snapshot!(result, @"
        a
        b
          c
      Int32
    ───────
    0     1
    1     2
    ");
}

#[test]
fn test_mixed_depth_columns() {
    // id and address should both appear in first row (top-aligned)
    let result = _table(
        "[{id: Int32, address: {street: Text, number: Int32}}]",
        Value::Array(vec![Value::Tuple(vec![
            Value::Prim32(1),
            Value::Tuple(vec![Value::new_text("Main St"), Value::Prim32(123)]),
        ])]),
    );

    assert_snapshot!(result, @"
         id    address
            street  number
      Int32 Text     Int32
    ──────────────────────
    0     1 Main St    123
    ");
}

#[test]
fn test_single_primitive() {
    let result = _table("Int32", Value::Prim32(42));

    assert_snapshot!(result, @"
    Int32
    ─────
       42
    ");
}

#[test]
fn test_array_of_primitives() {
    let result = _table(
        "[Int32]",
        Value::Array(vec![Value::Prim32(1), Value::Prim32(2), Value::Prim32(3)]),
    );

    assert_snapshot!(result, @"
      Int32
    ───────
    0     1
    1     2
    2     3
    ");
}
