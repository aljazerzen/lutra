use lutra_compiler::{ProgramFormat, SourceTree};
use lutra_runner_duckdb::Runner;

/// Helper function to run a Lutra program on DuckDB
#[track_caller]
#[tokio::main(flavor = "current_thread")]
pub async fn _run(source: &str, input: lutra_bin::Value) -> (String, String) {
    // Create in-memory DuckDB instance for each test
    let runner = Runner::in_memory().await.unwrap();
    _run_on(&runner, source, input).await
}

/// Run a Lutra program on a provided runner
pub async fn _run_on(
    runner: &impl lutra_runner::Run,
    source: &str,
    input: lutra_bin::Value,
) -> (String, String) {
    crate::init_logger();

    // compile
    let source = SourceTree::single("".into(), source.to_string());
    let project = match lutra_compiler::check(source, Default::default()) {
        Ok(p) => p,
        Err(e) => return (String::new(), format!("check error:\n{e}")),
    };

    // compile to sql using DuckDB dialect (tests use DuckDB runner)
    let res = lutra_compiler::compile(&project, "main", None, ProgramFormat::SqlDuckdb);
    let (program, ty) = match res {
        Ok(x) => x,
        Err(e) => return (String::new(), format!("compile error:\n{e}")),
    };

    // format sql
    let formatted_sql = {
        let program = program.as_sql_duck_db().unwrap();
        let options = sqlformat::FormatOptions::default();
        sqlformat::format(&program.sql, &sqlformat::QueryParams::None, &options)
    };

    let input = input.encode(&ty.input, &ty.defs).unwrap();

    // execute
    let program = runner.prepare(program).await.unwrap();
    let output = runner.execute(&program, &input).await.unwrap();

    // decode and print source
    let output = lutra_bin::print_source(&output, &ty.output, &ty.defs).unwrap();

    (formatted_sql, output)
}

pub fn _sql_and_output((sql, output): (String, String)) -> String {
    format!("{sql}\n---\n{output}")
}

// ============================================================================
// Basic Type Tests
// ============================================================================

#[test]
fn prim() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = 3: int16"#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      3::int2 AS value
    ---
    3
    ");
}

#[test]
fn prim_int64() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = 12345: int64"#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      12345::int8 AS value
    ---
    12345
    ");
}

#[test]
fn prim_bool() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = true"#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      TRUE AS value
    ---
    true
    ");
}

#[test]
fn prim_text() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = "hello""#,
        lutra_bin::Value::unit()
    )), @r#"
    SELECT
      'hello'::text AS value
    ---
    "hello"
    "#);
}

#[test]
fn prim_float64() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = 3.14: float64"#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      3.14::float8 AS value
    ---
    3.14
    ");
}

// ============================================================================
// Tuple Tests
// ============================================================================

#[test]
fn tuple_prim() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = {x = 1: int32, y = 2: int16}"#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      1::int4 AS _0,
      2::int2 AS _1
    ---
    {
      x = 1,
      y = 2,
    }
    ");
}

#[test]
fn tuple_mixed() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = {name = "Alice", age = 30: int32, active = true}"#,
        lutra_bin::Value::unit()
    )), @r#"
    SELECT
      'Alice'::text AS _0,
      30::int4 AS _1,
      TRUE AS _2
    ---
    {
      name = "Alice",
      age = 30,
      active = true,
    }
    "#);
}

#[test]
fn tuple_tuple_prim() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = {a = {x = 1: int32, y = 2: int16}, b = 3: int64}"#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      1::int4 AS _0_0,
      2::int2 AS _0_1,
      3::int8 AS _1
    ---
    {
      a = {
        x = 1,
        y = 2,
      },
      b = 3,
    }
    ");
}

// ============================================================================
// Array Result Tests (TOP-LEVEL ARRAYS)
// ============================================================================

#[test]
fn array_prim() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = [1, 2, 3]: [int64]"#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      r0.value
    FROM
      (
        SELECT
          0::int8 AS index,
          1::int8 AS value
        UNION
        ALL
        SELECT
          1::int8 AS index,
          2::int8 AS value
        UNION
        ALL
        SELECT
          2::int8 AS index,
          3::int8 AS value
      ) AS r0
    ORDER BY
      r0.index
    ---
    [
      1,
      2,
      3,
    ]
    ");
}

#[test]
fn array_empty() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = []: [int64]"#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      r0.value
    FROM
      (
        SELECT
          0 AS index,
          NULL::int8 AS value
        WHERE
          FALSE
      ) AS r0
    ORDER BY
      r0.index
    ---
    []
    ");
}

#[test]
fn array_tuple() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = [{x = 1: int32, y = 2: int16}, {x = 3, y = 4}]"#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      r0._0,
      r0._1
    FROM
      (
        SELECT
          0::int8 AS index,
          1::int4 AS _0,
          2::int2 AS _1
        UNION
        ALL
        SELECT
          1::int8 AS index,
          3::int4 AS _0,
          4::int2 AS _1
      ) AS r0
    ORDER BY
      r0.index
    ---
    [
      {
        x = 1,
        y = 2,
      },
      {
        x = 3,
        y = 4,
      },
    ]
    ");
}

// ============================================================================
// Tuple & array nesting
// ============================================================================

#[test]
fn tuple_array_prim() {
    // This be serialized to LIST & STRUCT types
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = {true, [1, 2, 3]: [int64], [{4: int32, "hello"}], false}"#,
        lutra_bin::Value::unit()
    )), @r#"
    SELECT
      TRUE AS _0,
      (
        SELECT
          COALESCE(
            list(
              r0.value
              ORDER BY
                r0.index
            ),
            CAST([] AS int8 [])
          ) AS value
        FROM
          (
            SELECT
              0::int8 AS index,
              1::int8 AS value
            UNION
            ALL
            SELECT
              1::int8 AS index,
              2::int8 AS value
            UNION
            ALL
            SELECT
              2::int8 AS index,
              3::int8 AS value
          ) AS r0
      ) AS _1,
      (
        SELECT
          COALESCE(
            list(
              struct_pack(_0 := r1._0, _1 := r1._1)
              ORDER BY
                r1.index
            ),
            CAST([] AS STRUCT(_0 int4, _1 text) [])
          ) AS value
        FROM
          (
            SELECT
              0::int8 AS index,
              4::int4 AS _0,
              'hello'::text AS _1
          ) AS r1
      ) AS _2,
      FALSE AS _3
    ---
    {
      true,
      [
        1,
        2,
        3,
      ],
      [
        {
          4,
          "hello",
        },
      ],
      false,
    }
    "#);
}

#[test]
fn array_array_prim() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = [[1, 2], [3, 4], [5]]: [[int64]]"#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      r3.value
    FROM
      (
        SELECT
          0::int8 AS index,
          (
            SELECT
              COALESCE(
                list(
                  r0.value
                  ORDER BY
                    r0.index
                ),
                CAST([] AS int8 [])
              ) AS value
            FROM
              (
                SELECT
                  0::int8 AS index,
                  1::int8 AS value
                UNION
                ALL
                SELECT
                  1::int8 AS index,
                  2::int8 AS value
              ) AS r0
          ) AS value
        UNION
        ALL
        SELECT
          1::int8 AS index,
          (
            SELECT
              COALESCE(
                list(
                  r1.value
                  ORDER BY
                    r1.index
                ),
                CAST([] AS int8 [])
              ) AS value
            FROM
              (
                SELECT
                  0::int8 AS index,
                  3::int8 AS value
                UNION
                ALL
                SELECT
                  1::int8 AS index,
                  4::int8 AS value
              ) AS r1
          ) AS value
        UNION
        ALL
        SELECT
          2::int8 AS index,
          (
            SELECT
              COALESCE(
                list(
                  r2.value
                  ORDER BY
                    r2.index
                ),
                CAST([] AS int8 [])
              ) AS value
            FROM
              (
                SELECT
                  0::int8 AS index,
                  5::int8 AS value
              ) AS r2
          ) AS value
      ) AS r3
    ORDER BY
      r3.index
    ---
    [
      [
        1,
        2,
      ],
      [
        3,
        4,
      ],
      [
        5,
      ],
    ]
    ");
}

// ============================================================================
// Input Parameter Tests
// ============================================================================

#[test]
fn input_prim() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"func main(x: int64): int64 -> x + 10"#,
        lutra_bin::Value::Prim64(5)
    )), @"
    SELECT
      ($1::int8 + 10::int8) AS value
    ---
    15
    ");
}

#[test]
fn input_tuple() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"func main(p: {x: int64, y: int64}): int64 -> p.x + p.y"#,
        lutra_bin::Value::Tuple(vec![
            lutra_bin::Value::Prim64(3),
            lutra_bin::Value::Prim64(4),
        ])
    )), @"
    SELECT
      ($1::int8 + $2::int8) AS value
    ---
    7
    ");
}

#[test]
fn input_text() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"func main(name: text): text -> f"Hello, {name}""#,
        lutra_bin::Value::Text("World".to_string())
    )), @r#"
    SELECT
      ('Hello, '::text || $1::text) AS value
    ---
    "Hello, World"
    "#);
}

#[test]
fn input_array_prim() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"func main(nums: [int64]): [int64] -> nums"#,
        lutra_bin::Value::Array(vec![
            lutra_bin::Value::Prim64(10),
            lutra_bin::Value::Prim64(20),
            lutra_bin::Value::Prim64(30),
        ])
    )), @"
    SELECT
      (u.unnest)::int8 AS value
    FROM
      LATERAL unnest($1::int8 []) AS u
    ---
    [
      10,
      20,
      30,
    ]
    ");
}

#[test]
fn input_array_text() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"func main(words: [text]): [text] -> words"#,
        lutra_bin::Value::Array(vec![
            lutra_bin::Value::Text("hello".to_string()),
            lutra_bin::Value::Text("world".to_string()),
        ])
    )), @r#"
    SELECT
      (u.unnest)::text AS value
    FROM
      LATERAL unnest($1::text []) AS u
    ---
    [
      "hello",
      "world",
    ]
    "#);
}

#[test]
fn input_nested_tuple_array() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"func main(data: {items: [int64], count: int32}): {items: [int64], count: int32} -> data"#,
        lutra_bin::Value::Tuple(vec![
            lutra_bin::Value::Array(vec![
                lutra_bin::Value::Prim64(100),
                lutra_bin::Value::Prim64(200),
            ]),
            lutra_bin::Value::Prim32(5),
        ])
    )), @"
    SELECT
      $1::int8 [] AS _0,
      $2::int4 AS _1
    ---
    {
      items = [
        100,
        200,
      ],
      count = 5,
    }
    ");
}

#[test]
#[ignore]
fn input_array_tuple() {
    // Arrays of tuples as input parameters don't work because:
    // 1. We pass JSON string (e.g., '[{"x":3,"y":4}]')
    // 2. SQL expects STRUCT(x int4, y int4)[]
    // 3. DuckDB cannot cast JSON/VARCHAR to STRUCT types
    //
    // This is a limitation of using JSON for input serialization.
    // Arrays of primitives work because DuckDB can cast JSON arrays to native arrays.
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"func main(points: [{x: int32, y: int32}]): [{x: int32, y: int32}] -> points"#,
        lutra_bin::Value::Array(vec![
            lutra_bin::Value::Tuple(vec![
                lutra_bin::Value::Prim16(3),
                lutra_bin::Value::Prim32(4),
            ]),
            lutra_bin::Value::Tuple(vec![
                lutra_bin::Value::Prim16(10),
                lutra_bin::Value::Prim32(20),
            ]),
        ])
    )), @"");
}

// TODO: input_enum

// ============================================================================
// Comparison Operations
// ============================================================================

#[test]
fn comparison() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"func main() -> 5: int32 > 3"#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      (3::int4 < 5::int4) AS value
    ---
    true
    ");
}

#[test]
fn equality() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"func main() -> "hello" == "hello""#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      ('hello'::text = 'hello'::text) AS value
    ---
    true
    ");
}
