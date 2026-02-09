//! Tests for table rendering.

use insta::assert_snapshot;
use lutra_bin::Value;

fn _table(ty_src: &str, value: Value) -> String {
    let ty = lutra_compiler::_test_compile_ty(ty_src);
    let data = value.encode(&ty, &[]).unwrap();

    lutra_bin::Table::new(&data, &ty, &[]).render()
}

fn _table_with_config(ty_src: &str, value: Value, config: lutra_bin::TableConfig) -> String {
    let ty = lutra_compiler::_test_compile_ty(ty_src);
    let data = value.encode(&ty, &[]).unwrap();

    lutra_bin::Table::new(&data, &ty, &[]).render_with_config(&config)
}

#[test]
fn test_simple_flat_columns() {
    let result = _table(
        "[{a: int32, b: text}]",
        Value::Array(vec![
            Value::Tuple(vec![Value::Prim32(42), Value::Text("hello".into())]),
            Value::Tuple(vec![Value::Prim32(100), Value::Text("world".into())]),
        ]),
    );

    assert_snapshot!(result, @"
          a b
      int32 text
    ─────────────
    0    42 hello
    1   100 world
    ");
}

#[test]
fn test_nested_tuple() {
    let result = _table(
        "[{id: int32, address: {city: text, zip: int32}}]",
        Value::Array(vec![
            Value::Tuple(vec![
                Value::Prim32(1),
                Value::Tuple(vec![Value::Text("NYC".into()), Value::Prim32(10001)]),
            ]),
            Value::Tuple(vec![
                Value::Prim32(2),
                Value::Tuple(vec![Value::Text("LA".into()), Value::Prim32(90001)]),
            ]),
        ]),
    );

    assert_snapshot!(result, @"
         id  address
            city   zip
      int32 text int32
    ──────────────────
    0     1 NYC  10001
    1     2 LA   90001
    ");
}

#[test]
fn test_array_expansion() {
    let result = _table(
        "[{name: text, tags: [text]}]",
        Value::Array(vec![
            Value::Tuple(vec![
                Value::Text("Alice".into()),
                Value::Array(vec![Value::Text("dev".into()), Value::Text("rust".into())]),
            ]),
            Value::Tuple(vec![
                Value::Text("Bob".into()),
                Value::Array(vec![Value::Text("pm".into())]),
            ]),
        ]),
    );

    assert_snapshot!(result, @"
      name  tags
      text  [text]
    ──────────────
    0 Alice dev
            rust
    1 Bob   pm
    ");
}

#[test]
fn test_array_truncation() {
    let result = _table(
        "[{name: text, tags: [text]}]",
        Value::Array(vec![Value::Tuple(vec![
            Value::Text("Alice".into()),
            Value::Array(vec![
                Value::Text("a".into()),
                Value::Text("b".into()),
                Value::Text("c".into()),
                Value::Text("d".into()),
                Value::Text("e".into()),
            ]),
        ])]),
    );

    assert_snapshot!(result, @"
      name  tags
      text  [text]
    ────────────────
    0 Alice a
            b
            … 3 more
    ");
}

#[test]
fn test_non_flat_array() {
    let result = _table(
        "[{id: int32, items: [{x: int32}]}]",
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
      int32 […]
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
        "[{opt: enum{none, some: int32}}]",
        Value::Array(vec![
            Value::Tuple(vec![Value::Enum(0, Box::new(Value::Tuple(vec![])))]),
            Value::Tuple(vec![Value::Enum(1, Box::new(Value::Prim32(42)))]),
        ]),
    );

    assert_snapshot!(result, @"
      opt
      enum{none,some}
    ─────────────────
    0 none
    1 some(42)
    ");
}

#[test]
fn test_text_truncation() {
    let config = lutra_bin::TableConfig {
        max_col_width: 10,
        max_array_items: 3,
        sample_rows: Some(100),
    };

    let result = _table_with_config(
        "[{msg: text}]",
        Value::Array(vec![Value::Tuple(vec![Value::Text(
            "This is a very long message that should be truncated".into(),
        )])]),
        config,
    );
    assert_snapshot!(result, @"
      msg
      text
    ────────────
    0 This is a…
    ");
}

#[test]
fn test_empty_array() {
    let result = _table("[{a: int32}]", Value::Array(vec![]));

    assert_eq!(
        result,
        r#"      a
  int32
───────
"#
    );
}

#[test]
fn test_single_tuple() {
    let result = _table(
        "{x: int32, y: text}",
        Value::Tuple(vec![Value::Prim32(42), Value::Text("hello".into())]),
    );

    assert_snapshot!(result, @"
          x y
      int32 text
    ─────────────
    0    42 hello
    ");
}

#[test]
fn test_alignment() {
    let result = _table(
        "[{name: text, score: int32, active: bool}]",
        Value::Array(vec![
            Value::Tuple(vec![
                Value::Text("Alice".into()),
                Value::Prim32(95),
                Value::Prim8(1),
            ]),
            Value::Tuple(vec![
                Value::Text("Bob".into()),
                Value::Prim32(82),
                Value::Prim8(0),
            ]),
        ]),
    );

    assert_snapshot!(result, @"
      name  score active
      text  int32 bool
    ────────────────────
    0 Alice    95 true
    1 Bob      82 false
    ");
}

#[test]
fn test_deeply_nested() {
    let result = _table(
        "[{a: {b: {c: int32}}}]",
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
      int32
    ───────
    0     1
    1     2
    ");
}

#[test]
fn test_mixed_depth_columns() {
    // id and address should both appear in first row (top-aligned)
    let result = _table(
        "[{id: int32, address: {street: text, number: int32}}]",
        Value::Array(vec![Value::Tuple(vec![
            Value::Prim32(1),
            Value::Tuple(vec![Value::Text("Main St".into()), Value::Prim32(123)]),
        ])]),
    );

    assert_snapshot!(result, @"
         id    address
            street  number
      int32 text     int32
    ──────────────────────
    0     1 Main St    123
    ");
}

#[test]
fn test_single_primitive() {
    let result = _table("int32", Value::Prim32(42));

    assert_snapshot!(result, @"
      value
      int32
    ───────
    0    42
    ");
}

#[test]
fn test_array_of_primitives() {
    let result = _table(
        "[int32]",
        Value::Array(vec![Value::Prim32(1), Value::Prim32(2), Value::Prim32(3)]),
    );

    assert_snapshot!(result, @"
      value
      int32
    ───────
    0     1
    1     2
    2     3
    ");
}
