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

/// Helper function to run a Lutra program on DuckDB with setup SQL
#[track_caller]
#[tokio::main(flavor = "current_thread")]
pub async fn _run_with_setup(setup: &str, source: &str, input: lutra_bin::Value) -> (String, String) {
    // Create in-memory DuckDB instance and execute setup
    let client = async_duckdb::ClientBuilder::new().open().await.unwrap();
    let setup = setup.to_string();
    client.conn(move |conn| {
        Ok(conn.execute_batch(&setup)?)
    }).await.unwrap();
    
    let runner = Runner::new(client);
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

// ============================================================================
// Table Representation Tests (sql::from and sql::insert with external tables)
// ============================================================================

#[test]
fn sql_from_primitive_table() {
    // Test reading primitives from a table - basic case
    insta::assert_snapshot!(_sql_and_output(_run_with_setup(
        r#"
        CREATE TABLE users (id int4, name text);
        INSERT INTO users VALUES (1, 'Alice'), (2, 'Bob');
        "#,
        r#"
        type Row: {id: int32, name: text}
        func main(): [Row] -> std::sql::from("users")
        "#,
        lutra_bin::Value::unit()
    )), @r#"
    SELECT
      id::int4 AS _0,
      name::text AS _1
    FROM
      users
    ---
    [
      {
        id = 1,
        name = "Alice",
      },
      {
        id = 2,
        name = "Bob",
      },
    ]
    "#);
}

#[test]
fn sql_from_flat_tuple() {
    // Test reading flat tuples (primitives only) - should flatten columns
    insta::assert_snapshot!(_sql_and_output(_run_with_setup(
        r#"
        CREATE TABLE products (id int4, name text, price float8, in_stock bool);
        INSERT INTO products VALUES (1, 'Widget', 9.99, true), (2, 'Gadget', 19.99, false);
        "#,
        r#"
        type Product: {id: int32, name: text, price: float64, in_stock: bool}
        func main(): [Product] -> std::sql::from("products")
        "#,
        lutra_bin::Value::unit()
    )), @r#"
    SELECT
      id::int4 AS _0,
      name::text AS _1,
      price::float8 AS _2,
      in_stock::bool AS _3
    FROM
      products
    ---
    [
      {
        id = 1,
        name = "Widget",
        price = 9.99,
        in_stock = true,
      },
      {
        id = 2,
        name = "Gadget",
        price = 19.99,
        in_stock = false,
      },
    ]
    "#);
}

#[test]
fn sql_from_nested_tuple() {
    // Test reading nested tuples - should use STRUCT in DuckDB
    insta::assert_snapshot!(_sql_and_output(_run_with_setup(
        r#"
        CREATE TABLE events (
            id int4,
            data STRUCT(user_id int4, items int4[])
        );
        INSERT INTO events VALUES 
            (1, {'user_id': 100, 'items': [1, 2, 3]}),
            (2, {'user_id': 200, 'items': [4, 5]});
        "#,
        r#"
        type Event: {id: int32, data: {user_id: int32, items: [int32]}}
        func main(): [Event] -> std::sql::from("events")
        "#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      id::int4 AS _0,
      data::STRUCT(user_id int4, items int4 []) AS _1_0
    FROM
      events
    ---
    [
      {
        id = 1,
        data = {
          user_id = 100,
          items = [
            1,
            2,
            3,
          ],
        },
      },
      {
        id = 2,
        data = {
          user_id = 200,
          items = [
            4,
            5,
          ],
        },
      },
    ]
    ");
}

#[test]
fn sql_from_array_column() {
    // Test reading arrays stored in LIST columns
    insta::assert_snapshot!(_sql_and_output(_run_with_setup(
        r#"
        CREATE TABLE items (id int4, tags text[]);
        INSERT INTO items VALUES (1, ['rust', 'programming']), (2, ['sql', 'database']);
        "#,
        r#"
        type Item: {id: int32, tags: [text]}
        func main(): [Item] -> std::sql::from("items")
        "#,
        lutra_bin::Value::unit()
    )), @r#"
    SELECT
      id::int4 AS _0,
      tags::text [] AS _1
    FROM
      items
    ---
    [
      {
        id = 1,
        tags = [
          "rust",
          "programming",
        ],
      },
      {
        id = 2,
        tags = [
          "sql",
          "database",
        ],
      },
    ]
    "#);
}

#[test]
fn sql_from_maybe_column() {
    // Test reading nullable columns (Maybe/Option)
    insta::assert_snapshot!(_sql_and_output(_run_with_setup(
        r#"
        CREATE TABLE people (id int4, name text, age int4);
        INSERT INTO people VALUES (1, 'Alice', 30), (2, 'Bob', NULL);
        "#,
        r#"
        type Person: {id: int32, name: text, age: enum {none, some: int32}}
        func main(): [Person] -> std::sql::from("people")
        "#,
        lutra_bin::Value::unit()
    )), @"");
}

#[test]
fn sql_from_date_column() {
    // Test reading std::Date from date column
    insta::assert_snapshot!(_sql_and_output(_run_with_setup(
        r#"
        CREATE TABLE events (id int4, event_date date);
        INSERT INTO events VALUES (1, '2024-01-15'), (2, '2024-12-25');
        "#,
        r#"
        type Event: {id: int32, event_date: std::Date}
        func main(): [Event] -> std::sql::from("events")
        "#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      id::int4 AS _0,
      (event_date::date - '1970-01-01'::date) AS _1
    FROM
      events
    ---
    [
      {
        id = 1,
        event_date = @2024-01-15,
      },
      {
        id = 0,
        event_date = @1970-01-03,
      },
    ]
    ");
}

#[test]
fn sql_insert_primitives() {
    // Test inserting primitives into a table
    insta::assert_snapshot!(_sql_and_output(_run_with_setup(
        r#"
        CREATE TABLE users (id int4, name text);
        "#,
        r#"
        type Row: {id: int32, name: text}
        const data: [Row] = [{id = 3, name = "Charlie"}, {id = 4, name = "Diana"}]
        func main(): {} -> std::sql::insert(data, "users")
        "#,
        lutra_bin::Value::unit()
    )), @"
    INSERT INTO
      users (id, name)
    SELECT
      r0._0,
      r0._1
    FROM
      (
        SELECT
          0::int8 AS index,
          3::int4 AS _0,
          'Charlie'::text AS _1
        UNION
        ALL
        SELECT
          1::int8 AS index,
          4::int4 AS _0,
          'Diana'::text AS _1
      ) AS r0
    ---
    {}
    ");
}

#[test]
fn sql_insert_flat_tuple() {
    // Test inserting flat tuples
    insta::assert_snapshot!(_sql_and_output(_run_with_setup(
        r#"
        CREATE TABLE products (id int4, name text, price float8, in_stock bool);
        "#,
        r#"
        type Product: {id: int32, name: text, price: float64, in_stock: bool}
        const products: [Product] = [
            {id = 1, name = "Widget", price = 9.99, in_stock = true},
            {id = 2, name = "Gadget", price = 19.99, in_stock = false}
        ]
        func main(): {} -> std::sql::insert(products, "products")
        "#,
        lutra_bin::Value::unit()
    )), @"
    INSERT INTO
      products (id, name, price, in_stock)
    SELECT
      r0._0,
      r0._1,
      r0._2,
      r0._3
    FROM
      (
        SELECT
          0::int8 AS index,
          1::int4 AS _0,
          'Widget'::text AS _1,
          9.99::float8 AS _2,
          TRUE AS _3
        UNION
        ALL
        SELECT
          1::int8 AS index,
          2::int4 AS _0,
          'Gadget'::text AS _1,
          19.99::float8 AS _2,
          FALSE AS _3
      ) AS r0
    ---
    {}
    ");
}

#[test]
fn sql_insert_maybe() {
    // Test inserting Maybe/Option values
    insta::assert_snapshot!(_sql_and_output(_run_with_setup(
        r#"
        CREATE TABLE people (id int4, name text, age int4);
        "#,
        r#"
        type Person: {id: int32, name: text, age: enum {none, some: int32}}
        const people: [Person] = [
            {id = 1, name = "Alice", age = .some(30)},
            {id = 2, name = "Bob", age = .none}
        ]
        func main(): {} -> std::sql::insert(people, "people")
        "#,
        lutra_bin::Value::unit()
    )), @"
    INSERT INTO
      people (id, name, age)
    SELECT
      r0._0,
      r0._1,
      r0._2
    FROM
      (
        SELECT
          0::int8 AS index,
          1::int4 AS _0,
          'Alice'::text AS _1,
          30::int4 AS _2
        UNION
        ALL
        SELECT
          1::int8 AS index,
          2::int4 AS _0,
          'Bob'::text AS _1,
          NULL::int4 AS _2
      ) AS r0
    ---
    {}
    ");
}

#[test]
fn sql_insert_date() {
    // Test inserting std::Date
    insta::assert_snapshot!(_sql_and_output(_run_with_setup(
        r#"
        CREATE TABLE events (id int4, event_date date);
        "#,
        r#"
        type Event: {id: int32, event_date: std::Date}
        const events: [Event] = [
            {id = 1, event_date = @2024-01-15},
            {id = 2, event_date = @2024-12-25}
        ]
        func main(): {} -> std::sql::insert(events, "events")
        "#,
        lutra_bin::Value::unit()
    )), @"
    INSERT INTO
      events (id, event_date)
    SELECT
      r0._0,
      ('1970-01-01'::date + r0._1)
    FROM
      (
        SELECT
          0::int8 AS index,
          1::int4 AS _0,
          19737::int4 AS _1
        UNION
        ALL
        SELECT
          1::int8 AS index,
          2::int4 AS _0,
          20082::int4 AS _1
      ) AS r0
    ---
    {}
    ");
}
