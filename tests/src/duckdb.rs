use lutra_compiler::{ProgramFormat, SourceTree};
use lutra_runner_duckdb::Runner;

/// Helper function to run a Lutra program on DuckDB
#[track_caller]
#[tokio::main(flavor = "current_thread")]
pub async fn _run(source: &str, input: lutra_bin::Value) -> (String, String) {
    // Create in-memory DuckDB instance for each test
    let runner = Runner::in_memory(None).await.unwrap();
    _run_on(&runner, source, input).await
}

/// Helper function to run a Lutra program on DuckDB with setup SQL
#[track_caller]
#[tokio::main(flavor = "current_thread")]
pub async fn _run_with_setup(
    setup: &str,
    source: &str,
    input: lutra_bin::Value,
) -> (String, String) {
    // Create in-memory DuckDB instance and execute setup
    let client = async_duckdb::ClientBuilder::new().open().await.unwrap();
    let setup = setup.to_string();
    client
        .conn(move |conn| conn.execute_batch(&setup))
        .await
        .unwrap();

    let runner = Runner::new(client, None).await.unwrap();
    _run_on(&runner, source, input).await
}

/// Run a Lutra program on a provided runner
pub async fn _run_on(
    runner: &impl lutra_runner::Run,
    source: &str,
    input: lutra_bin::Value,
) -> (String, String) {
    crate::init_logger();

    let (program, ty, formatted_sql) = match _compile(source) {
        Ok(r) => r,
        Err(e) => return ("".into(), e),
    };

    let input = input.encode(&ty.input, &ty.defs).unwrap();

    // execute
    let program = runner.prepare(program).await.unwrap();
    let output = runner.execute(&program, &input).await.unwrap();

    // decode and print source
    let output = lutra_bin::print_source(&output, &ty.output, &ty.defs).unwrap();

    (formatted_sql, output)
}

#[track_caller]
fn _compile(
    source: &str,
) -> Result<(lutra_bin::rr::Program, lutra_bin::rr::ProgramType, String), String> {
    // check
    let source = SourceTree::single("".into(), source.to_string());
    let project = match lutra_compiler::check(source, Default::default()) {
        Ok(p) => p,
        Err(e) => return Err(format!("check error:\n{e}")),
    };

    // compile
    let res = lutra_compiler::compile(&project, "main", None, ProgramFormat::SqlDuckdb);
    let (program, ty) = match res {
        Ok(x) => x,
        Err(e) => return Err(format!("compile error:\n{e}")),
    };

    // format_sql
    let formatted_sql = {
        let program = program.as_sql_duck_db().unwrap();
        let options = sqlformat::FormatOptions::default();
        sqlformat::format(&program.sql, &sqlformat::QueryParams::None, &options)
    };
    Ok((program, ty, formatted_sql))
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
      3::INT2 AS value
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
      12345::INT8 AS value
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

// ============================================================================
// Schema Introspection Tests
// ============================================================================

#[tokio::test]
async fn pull_interface_basic() {
    use lutra_runner::Run;

    let client = async_duckdb::ClientBuilder::new().open().await.unwrap();

    client
        .conn(|conn| {
            conn.execute_batch(
                r#"
                CREATE TABLE users (id INTEGER PRIMARY KEY, name VARCHAR NOT NULL, email VARCHAR);
                CREATE TABLE posts (id INTEGER PRIMARY KEY, user_id INTEGER, title VARCHAR NOT NULL, content VARCHAR);
                "#,
            )
        })
        .await
        .unwrap();

    let runner = lutra_runner_duckdb::Runner::new(client, None)
        .await
        .unwrap();
    let interface = runner.get_interface().await.unwrap();

    insta::assert_snapshot!(interface, @r#"
    ## Row of table posts
    type Post: {id: int32, user_id: enum {none, some: int32}, title: text, content: enum {none, some: text}}
    ## Read from table posts
    func from_posts(): [Post] -> std::sql::from("posts")
    ## Write into table posts
    func insert_posts(values: [Post]) -> std::sql::insert(values, "posts")
    ## Lookup in posts by constraint posts_id_pkey
    func from_posts_by_id(id: int32): enum {none, some: Post} -> (
      from_posts() | std::find(x -> x.id == id)
    )

    ## Row of table users
    type User: {id: int32, name: text, email: enum {none, some: text}}
    ## Read from table users
    func from_users(): [User] -> std::sql::from("users")
    ## Write into table users
    func insert_users(values: [User]) -> std::sql::insert(values, "users")
    ## Lookup in users by constraint users_id_pkey
    func from_users_by_id(id: int32): enum {none, some: User} -> (
      from_users() | std::find(x -> x.id == id)
    )
    "#);
}

#[tokio::test]
async fn pull_interface_types() {
    use lutra_runner::Run;

    let client = async_duckdb::ClientBuilder::new().open().await.unwrap();
    client
        .conn(|conn| {
            conn.execute_batch(
                r#"
                CREATE TABLE type_test (
                    bool_col BOOLEAN,
                    int8_col TINYINT,
                    int16_col SMALLINT,
                    int32_col INTEGER,
                    int64_col BIGINT,
                    float32_col FLOAT,
                    float64_col DOUBLE,
                    text_col VARCHAR,
                    date_col DATE
                );
                "#,
            )
        })
        .await
        .unwrap();

    let runner = lutra_runner_duckdb::Runner::new(client, None)
        .await
        .unwrap();
    let interface = runner.get_interface().await.unwrap();

    insta::assert_snapshot!(interface, @r#"
    ## Row of table type_test
    type TypeTestRow: {bool_col: enum {none, some: bool}, int8_col: enum {none, some: int8}, int16_col: enum {none, some: int16}, int32_col: enum {none, some: int32}, int64_col: enum {none, some: int64}, float32_col: enum {none, some: float32}, float64_col: enum {none, some: float64}, text_col: enum {none, some: text}, date_col: enum {none, some: std::Date}}
    ## Read from table type_test
    func from_type_test(): [TypeTestRow] -> std::sql::from("type_test")
    ## Write into table type_test
    func insert_type_test(values: [TypeTestRow]) -> std::sql::insert(values, "type_test")
    "#);
}

#[tokio::test]
async fn pull_interface_arrays() {
    use lutra_runner::Run;

    let client = async_duckdb::ClientBuilder::new().open().await.unwrap();
    client
        .conn(|conn| {
            conn.execute_batch(
                r#"
                CREATE TABLE arrays_test (
                    id INTEGER,
                    tags VARCHAR[],
                    numbers INTEGER[],
                    nested INTEGER[][]
                );
                "#,
            )
        })
        .await
        .unwrap();

    let runner = lutra_runner_duckdb::Runner::new(client, None)
        .await
        .unwrap();
    let interface = runner.get_interface().await.unwrap();

    insta::assert_snapshot!(interface, @r#"
    ## Row of table arrays_test
    type ArraysTestRow: {id: enum {none, some: int32}, tags: enum {none, some: [text]}, numbers: enum {none, some: [int32]}, nested: enum {none, some: [[int32]]}}
    ## Read from table arrays_test
    func from_arrays_test(): [ArraysTestRow] -> std::sql::from("arrays_test")
    ## Write into table arrays_test
    func insert_arrays_test(values: [ArraysTestRow]) -> std::sql::insert(values, "arrays_test")
    "#);
}

#[tokio::test]
async fn pull_interface_struct() {
    use lutra_runner::Run;

    let client = async_duckdb::ClientBuilder::new().open().await.unwrap();
    client
        .conn(|conn| {
            conn.execute_batch(
                r#"
                CREATE TABLE struct_test (
                    id INTEGER,
                    person STRUCT(name VARCHAR, age INTEGER),
                    location STRUCT(city VARCHAR, coordinates STRUCT(lat DOUBLE, lon DOUBLE))
                );
                "#,
            )
        })
        .await
        .unwrap();

    let runner = lutra_runner_duckdb::Runner::new(client, None)
        .await
        .unwrap();
    let interface = runner.get_interface().await.unwrap();

    insta::assert_snapshot!(interface, @r#"
    ## Row of table struct_test
    type StructTestRow: {id: enum {none, some: int32}, person: enum {none, some: {name: text, age: int32}}, location: enum {none, some: {city: text, coordinates: {lat: float64, lon: float64}}}}
    ## Read from table struct_test
    func from_struct_test(): [StructTestRow] -> std::sql::from("struct_test")
    ## Write into table struct_test
    func insert_struct_test(values: [StructTestRow]) -> std::sql::insert(values, "struct_test")
    "#);
}

#[tokio::test]
async fn pull_interface_indexes_basic() {
    use lutra_runner::Run;

    let client = async_duckdb::ClientBuilder::new().open().await.unwrap();
    client
        .conn(|conn| {
            conn.execute_batch(
                r#"
                CREATE TABLE users (
                    id INTEGER PRIMARY KEY,
                    email VARCHAR UNIQUE,
                    name VARCHAR
                );
                CREATE INDEX idx_name ON users(name);
                "#,
            )
        })
        .await
        .unwrap();

    let runner = lutra_runner_duckdb::Runner::new(client, None)
        .await
        .unwrap();
    let interface = runner.get_interface().await.unwrap();

    insta::assert_snapshot!(interface, @r#"
    ## Row of table users
    type User: {id: int32, email: enum {none, some: text}, name: enum {none, some: text}}
    ## Read from table users
    func from_users(): [User] -> std::sql::from("users")
    ## Write into table users
    func insert_users(values: [User]) -> std::sql::insert(values, "users")
    ## Lookup in users by constraint users_id_pkey
    func from_users_by_id(id: int32): enum {none, some: User} -> (
      from_users() | std::find(x -> x.id == id)
    )
    ## Lookup in users by constraint users_email_key
    func from_users_by_email(email: text): enum {none, some: User} -> (
      from_users() | std::find(x -> x.email == email)
    )
    ## Lookup in users by index idx_name
    func from_users_by_name(name: text): [User] -> (
      from_users() | std::filter(x -> x.name == name)
    )
    "#);
}

#[tokio::test]
async fn pull_interface_indexes_compound() {
    use lutra_runner::Run;

    let client = async_duckdb::ClientBuilder::new().open().await.unwrap();
    client
        .conn(|conn| {
            conn.execute_batch(
                r#"
                CREATE TABLE orders (
                    id INTEGER,
                    user_id INTEGER,
                    product_id INTEGER,
                    quantity INTEGER
                );
                CREATE INDEX idx_user_product ON orders(user_id, product_id);
                "#,
            )
        })
        .await
        .unwrap();

    let runner = lutra_runner_duckdb::Runner::new(client, None)
        .await
        .unwrap();
    let interface = runner.get_interface().await.unwrap();

    insta::assert_snapshot!(interface, @r#"
    ## Row of table orders
    type Order: {id: enum {none, some: int32}, user_id: enum {none, some: int32}, product_id: enum {none, some: int32}, quantity: enum {none, some: int32}}
    ## Read from table orders
    func from_orders(): [Order] -> std::sql::from("orders")
    ## Write into table orders
    func insert_orders(values: [Order]) -> std::sql::insert(values, "orders")
    ## Lookup in orders by index idx_user_product
    func from_orders_by_user_id_and_product_id(user_id: int32, product_id: int32): [Order] -> (
      from_orders() | std::filter(x -> x.user_id == user_id && x.product_id == product_id)
    )
    "#);
}

#[test]
fn prim_float32() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = 2.5: float32"#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      2.5::FLOAT4 AS value
    ---
    2.5
    ");
}

#[test]
fn prim_int32() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = 123456: int32"#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      123456::INT4 AS value
    ---
    123456
    ");
}

#[test]
fn prim_int8() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = 42: int8"#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      42::INT1 AS value
    ---
    42
    ");
}

#[test]
fn prim_uint8() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = 200: uint8"#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      200::UINT8 AS value
    ---
    200
    ");
}

#[test]
fn prim_uint16() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = 50000: uint16"#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      50000::UINT16 AS value
    ---
    50000
    ");
}

#[test]
fn prim_uint32() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = 3000000000: uint32"#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      3000000000::UINT32 AS value
    ---
    3000000000
    ");
}

#[test]
fn prim_uint64() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = 10000000000: uint64"#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      10000000000::UINT64 AS value
    ---
    10000000000
    ");
}

// ============================================================================
// Option Enum Tests
// ============================================================================

#[test]
fn option_none() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = .none: enum {none, some: int32}"#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      NULL::INT4 AS value
    ---
    none
    ");
}

#[test]
fn option_some() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = .some(42): enum {none, some: int32}"#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      42::INT4 AS value
    ---
    some(42)
    ");
}

#[test]
fn option_some_text() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = .some("hello"): enum {none, some: text}"#,
        lutra_bin::Value::unit()
    )), @r#"
    SELECT
      'hello'::text AS value
    ---
    some("hello")
    "#);
}

#[test]
fn option_array() {
    // Array of options
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = [.none, .some(1), .some(2), .none]: [enum {none, some: int32}]"#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      r0.value
    FROM
      (
        SELECT
          0::INT8 AS index,
          NULL::INT4 AS value
        UNION
        ALL
        SELECT
          1::INT8 AS index,
          1::INT4 AS value
        UNION
        ALL
        SELECT
          2::INT8 AS index,
          2::INT4 AS value
        UNION
        ALL
        SELECT
          3::INT8 AS index,
          NULL::INT4 AS value
      ) AS r0
    ORDER BY
      r0.index
    ---
    [
      none,
      some(1),
      some(2),
      none,
    ]
    ");
}

#[test]
fn tuple_with_option() {
    // Option within a tuple works (nullable column)
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = {name = "Alice", age = .some(30): enum {none, some: int32}}"#,
        lutra_bin::Value::unit()
    )), @r#"
    SELECT
      r0._0 AS name,
      r0._1 AS age
    FROM
      (
        SELECT
          'Alice'::text AS _0,
          30::INT4 AS _1
      ) AS r0
    ---
    {
      name = "Alice",
      age = some(30),
    }
    "#);
}

#[test]
fn tuple_with_option_none() {
    // Option with none value within a tuple
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"const main = {name = "Bob", age = .none: enum {none, some: int32}}"#,
        lutra_bin::Value::unit()
    )), @r#"
    SELECT
      r0._0 AS name,
      r0._1 AS age
    FROM
      (
        SELECT
          'Bob'::text AS _0,
          NULL::INT4 AS _1
      ) AS r0
    ---
    {
      name = "Bob",
      age = none,
    }
    "#);
}

// ============================================================================
// General Enum Tests (non-option enums with payloads)
// ============================================================================

#[test]
fn enum_with_payloads() {
    // General enum with various payload types
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"
        type Status: enum {
            pending,
            in_progress: int32,
            done: text
        }
        const main = .in_progress(42): Status
        "#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      CASE
        r0._t::int2
        WHEN 0 THEN union_value(pending := NULL)::
        UNION
    (pending BOOL, in_progress INT4, done TEXT)
        WHEN 1 THEN union_value(in_progress := r0._1)
        WHEN 2 THEN union_value(done := r0._2)
      END AS value
    FROM
      (
        SELECT
          1::INT2 AS _t,
          42::INT4 AS _1,
          NULL::TEXT AS _2
      ) AS r0
    ---
    in_progress(42)
    ");
}

#[test]
fn enum_array_with_payloads() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"
        type Status: enum {
            pending,
            in_progress: int32,
            done: text
        }
        const main = [.pending, .in_progress(50), .done("finished")]: [Status]
        "#,
        lutra_bin::Value::unit()
    )), @r#"
    SELECT
      CASE
        r1._t::int2
        WHEN 0 THEN union_value(pending := NULL)::
        UNION
    (pending BOOL, in_progress INT4, done TEXT)
        WHEN 1 THEN union_value(in_progress := r1._1)
        WHEN 2 THEN union_value(done := r1._2)
      END AS value
    FROM
      (
        SELECT
          r0._t,
          r0._1,
          r0._2
        FROM
          (
            SELECT
              0::INT8 AS index,
              0::INT2 AS _t,
              NULL::INT4 AS _1,
              NULL::TEXT AS _2
            UNION
            ALL
            SELECT
              1::INT8 AS index,
              1::INT2 AS _t,
              50::INT4 AS _1,
              NULL::TEXT AS _2
            UNION
            ALL
            SELECT
              2::INT8 AS index,
              2::INT2 AS _t,
              NULL::INT4 AS _1,
              'finished'::text AS _2
          ) AS r0
        ORDER BY
          r0.index
      ) AS r1
    ---
    [
      pending,
      in_progress(50),
      done("finished"),
    ]
    "#);
}

#[test]
fn tuple_with_enum() {
    // Enum within a tuple
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"
        type Status: enum {
            pending,
            in_progress: int32,
            done: text
        }
        const main = {id = 1: int32, status = .in_progress(42): Status}
        "#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      r0._0 AS id,
      CASE
        r0._1_t::int2
        WHEN 0 THEN union_value(pending := NULL)::
        UNION
    (pending BOOL, in_progress INT4, done TEXT)
        WHEN 1 THEN union_value(in_progress := r0._1_1)
        WHEN 2 THEN union_value(done := r0._1_2)
      END AS status
    FROM
      (
        SELECT
          1::INT4 AS _0,
          1::INT2 AS _1_t,
          42::INT4 AS _1_1,
          NULL::TEXT AS _1_2
      ) AS r0
    ---
    {
      id = 1,
      status = in_progress(42),
    }
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
      r0._0 AS x,
      r0._1 AS y
    FROM
      (
        SELECT
          1::INT4 AS _0,
          2::INT2 AS _1
      ) AS r0
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
      r0._0 AS name,
      r0._1 AS age,
      r0._2 AS active
    FROM
      (
        SELECT
          'Alice'::text AS _0,
          30::INT4 AS _1,
          TRUE AS _2
      ) AS r0
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
      struct_pack(x := r0._0_0, y := r0._0_1) AS a,
      r0._1 AS b
    FROM
      (
        SELECT
          1::INT4 AS _0_0,
          2::INT2 AS _0_1,
          3::INT8 AS _1
      ) AS r0
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
          0::INT8 AS index,
          1::INT8 AS value
        UNION
        ALL
        SELECT
          1::INT8 AS index,
          2::INT8 AS value
        UNION
        ALL
        SELECT
          2::INT8 AS index,
          3::INT8 AS value
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
      NULL::INT8 AS value
    WHERE
      FALSE
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
      r1._0 AS x,
      r1._1 AS y
    FROM
      (
        SELECT
          r0._0,
          r0._1
        FROM
          (
            SELECT
              0::INT8 AS index,
              1::INT4 AS _0,
              2::INT2 AS _1
            UNION
            ALL
            SELECT
              1::INT8 AS index,
              3::INT4 AS _0,
              4::INT2 AS _1
          ) AS r0
        ORDER BY
          r0.index
      ) AS r1
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
      r2._0 AS field0,
      r2._1 AS field1,
      r2._2 AS field2,
      r2._3 AS field3
    FROM
      (
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
                CAST([] AS INT8 [])
              ) AS value
            FROM
              (
                SELECT
                  0::INT8 AS index,
                  1::INT8 AS value
                UNION
                ALL
                SELECT
                  1::INT8 AS index,
                  2::INT8 AS value
                UNION
                ALL
                SELECT
                  2::INT8 AS index,
                  3::INT8 AS value
              ) AS r0
          ) AS _1,
          (
            SELECT
              COALESCE(
                list(
                  struct_pack(field0 := r1._0, field1 := r1._1)
                  ORDER BY
                    r1.index
                ),
                CAST([] AS STRUCT(field0 INT4, field1 TEXT) [])
              ) AS value
            FROM
              (
                SELECT
                  0::INT8 AS index,
                  4::INT4 AS _0,
                  'hello'::text AS _1
              ) AS r1
          ) AS _2,
          FALSE AS _3
      ) AS r2
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
          0::INT8 AS index,
          (
            SELECT
              COALESCE(
                list(
                  r0.value
                  ORDER BY
                    r0.index
                ),
                CAST([] AS INT8 [])
              ) AS value
            FROM
              (
                SELECT
                  0::INT8 AS index,
                  1::INT8 AS value
                UNION
                ALL
                SELECT
                  1::INT8 AS index,
                  2::INT8 AS value
              ) AS r0
          ) AS value
        UNION
        ALL
        SELECT
          1::INT8 AS index,
          (
            SELECT
              COALESCE(
                list(
                  r1.value
                  ORDER BY
                    r1.index
                ),
                CAST([] AS INT8 [])
              ) AS value
            FROM
              (
                SELECT
                  0::INT8 AS index,
                  3::INT8 AS value
                UNION
                ALL
                SELECT
                  1::INT8 AS index,
                  4::INT8 AS value
              ) AS r1
          ) AS value
        UNION
        ALL
        SELECT
          2::INT8 AS index,
          (
            SELECT
              COALESCE(
                list(
                  r2.value
                  ORDER BY
                    r2.index
                ),
                CAST([] AS INT8 [])
              ) AS value
            FROM
              (
                SELECT
                  0::INT8 AS index,
                  5::INT8 AS value
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
      ($1::INT8 + 10::INT8) AS value
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
      ($1::INT8 + $2::INT8) AS value
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
      ('Hello, '::text || $1::TEXT) AS value
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
      u.unnest AS value
    FROM
      LATERAL unnest($1::INT8 []) AS u
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
      u.unnest AS value
    FROM
      LATERAL unnest($1::TEXT []) AS u
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
      r0._0 AS items,
      r0._1 AS count
    FROM
      (
        SELECT
          $1::INT8 [] AS _0,
          $2::INT4 AS _1
      ) AS r0
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
        r#"func main(points: [{x: int16, y: int32}]) -> points"#,
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
      (3::INT4 < 5::INT4) AS value
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
      r1._0 AS id,
      r1._1 AS name
    FROM
      (
        SELECT
          r0.id AS _0,
          r0.name AS _1
        FROM
          users AS r0
      ) AS r1
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
      r1._0 AS id,
      r1._1 AS name,
      r1._2 AS price,
      r1._3 AS in_stock
    FROM
      (
        SELECT
          r0.id AS _0,
          r0.name AS _1,
          r0.price AS _2,
          r0.in_stock AS _3
        FROM
          products AS r0
      ) AS r1
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
      r1._0 AS id,
      struct_pack(user_id := r1._1_0, items := r1._1_1) AS data
    FROM
      (
        SELECT
          r0.id AS _0,
          r0.data.user_id AS _1_0,
          r0.data.items AS _1_1
        FROM
          events AS r0
      ) AS r1
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
      r1._0 AS id,
      r1._1 AS tags
    FROM
      (
        SELECT
          r0.id AS _0,
          r0.tags AS _1
        FROM
          items AS r0
      ) AS r1
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
fn sql_from_option_column() {
    // Test reading nullable columns
    // Lutra type uses option for nullable columns, Arrow nulls are wrapped accordingly
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
    )), @r#"
    SELECT
      r1._0 AS id,
      r1._1 AS name,
      r1._2 AS age
    FROM
      (
        SELECT
          r0.id AS _0,
          r0.name AS _1,
          r0.age AS _2
        FROM
          people AS r0
      ) AS r1
    ---
    [
      {
        id = 1,
        name = "Alice",
        age = some(30),
      },
      {
        id = 2,
        name = "Bob",
        age = none,
      },
    ]
    "#);
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
      r1._0 AS id,
      ('1970-01-01'::date + r1._1) AS event_date
    FROM
      (
        SELECT
          r0.id AS _0,
          (r0.event_date::date - '1970-01-01'::date)::int4 AS _1
        FROM
          events AS r0
      ) AS r1
    ---
    [
      {
        id = 1,
        event_date = @2024-01-15,
      },
      {
        id = 2,
        event_date = @2024-12-25,
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
      r0._0 AS id,
      r0._1 AS name
    FROM
      (
        SELECT
          0::INT8 AS index,
          3::INT4 AS _0,
          'Charlie'::text AS _1
        UNION
        ALL
        SELECT
          1::INT8 AS index,
          4::INT4 AS _0,
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
      r0._0 AS id,
      r0._1 AS name,
      r0._2 AS price,
      r0._3 AS in_stock
    FROM
      (
        SELECT
          0::INT8 AS index,
          1::INT4 AS _0,
          'Widget'::text AS _1,
          9.99::FLOAT8 AS _2,
          TRUE AS _3
        UNION
        ALL
        SELECT
          1::INT8 AS index,
          2::INT4 AS _0,
          'Gadget'::text AS _1,
          19.99::FLOAT8 AS _2,
          FALSE AS _3
      ) AS r0
    ---
    {}
    ");
}

#[test]
fn sql_insert_option() {
    // Test inserting option values
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
      r0._0 AS id,
      r0._1 AS name,
      r0._2 AS age
    FROM
      (
        SELECT
          0::INT8 AS index,
          1::INT4 AS _0,
          'Alice'::text AS _1,
          30::INT4 AS _2
        UNION
        ALL
        SELECT
          1::INT8 AS index,
          2::INT4 AS _0,
          'Bob'::text AS _1,
          NULL::INT4 AS _2
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
      r0._0 AS id,
      ('1970-01-01'::date + r0._1) AS event_date
    FROM
      (
        SELECT
          0::INT8 AS index,
          1::INT4 AS _0,
          19737::INT4 AS _1
        UNION
        ALL
        SELECT
          1::INT8 AS index,
          2::INT4 AS _0,
          20082::INT4 AS _1
      ) AS r0
    ---
    {}
    ");
}

// ============================================================================
// Serialize Tests (Array in tuple forces serialization to LIST/STRUCT)
// ============================================================================

#[test]
fn serialize_array_in_tuple() {
    // Having array in a tuple forces serialization to LIST.
    // Applying an operation on that array then forces deserialization.
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"
        func get_data() -> {a = [2, 5, 4, 3, 1, 2]: [int32]}

        func main() -> (
          get_data().a
          | std::map(func (y: int32) -> -y)
        )
        "#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      (- r0.value) AS value
    FROM
      (
        SELECT
          0::INT8 AS index,
          2::INT4 AS value
        UNION
        ALL
        SELECT
          1::INT8 AS index,
          5::INT4 AS value
        UNION
        ALL
        SELECT
          2::INT8 AS index,
          4::INT4 AS value
        UNION
        ALL
        SELECT
          3::INT8 AS index,
          3::INT4 AS value
        UNION
        ALL
        SELECT
          4::INT8 AS index,
          1::INT4 AS value
        UNION
        ALL
        SELECT
          5::INT8 AS index,
          2::INT4 AS value
      ) AS r0
    ORDER BY
      r0.index
    ---
    [
      -2,
      -5,
      -4,
      -3,
      -1,
      -2,
    ]
    ");
}

#[test]
fn serialize_array_of_tuples_in_tuple() {
    // Array of tuples serialized to LIST of STRUCT
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"
        func get_data() -> {a = [{2: int32, false}, {5, true}, {4, false}]}

        func main() -> (
          get_data().a
          | std::map(func (y: {int32, bool}) -> {-y.0, !y.1})
        )
        "#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      r1._0 AS field0,
      r1._1 AS field1
    FROM
      (
        SELECT
          (- r0._0) AS _0,
          (NOT r0._1) AS _1
        FROM
          (
            SELECT
              0::INT8 AS index,
              2::INT4 AS _0,
              FALSE AS _1
            UNION
            ALL
            SELECT
              1::INT8 AS index,
              5::INT4 AS _0,
              TRUE AS _1
            UNION
            ALL
            SELECT
              2::INT8 AS index,
              4::INT4 AS _0,
              FALSE AS _1
          ) AS r0
        ORDER BY
          r0.index
      ) AS r1
    ---
    [
      {
        -2,
        true,
      },
      {
        -5,
        false,
      },
      {
        -4,
        true,
      },
    ]
    ");
}

#[test]
fn serialize_nested_array() {
    // Nested arrays: [[int16]]
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"
        func get_data() -> [[1: int16, 2, 3], [4, 5, 6]]

        func main() -> (
          get_data() | std::map(func (y: [int16]) -> (
            std::index(y, 1)
          ))
        )
        "#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      (
        SELECT
          r4.value AS value
        FROM
          (
            SELECT
              r3.index,
              r3.value
            FROM
              (
                SELECT
                  (ROW_NUMBER() OVER () -1)::int4 AS index,
                  u.unnest AS value
                FROM
                  LATERAL unnest(r2.value) AS u
                ORDER BY
                  index OFFSET 1::INT8
              ) AS r3
            ORDER BY
              index
            LIMIT
              1::INT8
          ) AS r4
        UNION
        ALL
        SELECT
          NULL::INT2 AS value
        LIMIT
          1::INT8
      ) AS value
    FROM
      (
        SELECT
          0::INT8 AS index,
          (
            SELECT
              COALESCE(
                list(
                  r0.value
                  ORDER BY
                    r0.index
                ),
                CAST([] AS INT2 [])
              ) AS value
            FROM
              (
                SELECT
                  0::INT8 AS index,
                  1::INT2 AS value
                UNION
                ALL
                SELECT
                  1::INT8 AS index,
                  2::INT2 AS value
                UNION
                ALL
                SELECT
                  2::INT8 AS index,
                  3::INT2 AS value
              ) AS r0
          ) AS value
        UNION
        ALL
        SELECT
          1::INT8 AS index,
          (
            SELECT
              COALESCE(
                list(
                  r1.value
                  ORDER BY
                    r1.index
                ),
                CAST([] AS INT2 [])
              ) AS value
            FROM
              (
                SELECT
                  0::INT8 AS index,
                  4::INT2 AS value
                UNION
                ALL
                SELECT
                  1::INT8 AS index,
                  5::INT2 AS value
                UNION
                ALL
                SELECT
                  2::INT8 AS index,
                  6::INT2 AS value
              ) AS r1
          ) AS value
      ) AS r2
    ORDER BY
      r2.index
    ---
    [
      none,
      none,
    ]
    ");
}

#[test]
fn serialize_nested_array_map() {
    // Map over nested arrays
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"
        func get_data() -> [[1: int64, 2, 3], [4, 5, 6]]

        func main() -> (
          get_data()
          | std::map(func (y: [int64]) -> (
            y | std::map(func (z: int64) -> 6-z)
          ))
        )
        "#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      (
        SELECT
          COALESCE(
            list(
              r4.value
              ORDER BY
                r4.index
            ),
            CAST([] AS INT8 [])
          ) AS value
        FROM
          (
            SELECT
              r3.index AS index,
              (6::INT8 - r3.value) AS value
            FROM
              (
                SELECT
                  (ROW_NUMBER() OVER () -1)::int4 AS index,
                  u.unnest AS value
                FROM
                  LATERAL unnest(r2.value) AS u
              ) AS r3
          ) AS r4
      ) AS value
    FROM
      (
        SELECT
          0::INT8 AS index,
          (
            SELECT
              COALESCE(
                list(
                  r0.value
                  ORDER BY
                    r0.index
                ),
                CAST([] AS INT8 [])
              ) AS value
            FROM
              (
                SELECT
                  0::INT8 AS index,
                  1::INT8 AS value
                UNION
                ALL
                SELECT
                  1::INT8 AS index,
                  2::INT8 AS value
                UNION
                ALL
                SELECT
                  2::INT8 AS index,
                  3::INT8 AS value
              ) AS r0
          ) AS value
        UNION
        ALL
        SELECT
          1::INT8 AS index,
          (
            SELECT
              COALESCE(
                list(
                  r1.value
                  ORDER BY
                    r1.index
                ),
                CAST([] AS INT8 [])
              ) AS value
            FROM
              (
                SELECT
                  0::INT8 AS index,
                  4::INT8 AS value
                UNION
                ALL
                SELECT
                  1::INT8 AS index,
                  5::INT8 AS value
                UNION
                ALL
                SELECT
                  2::INT8 AS index,
                  6::INT8 AS value
              ) AS r1
          ) AS value
      ) AS r2
    ORDER BY
      r2.index
    ---
    [
      [
        5,
        4,
        3,
      ],
      [
        2,
        1,
        0,
      ],
    ]
    ");
}

#[test]
fn serialize_bool_array() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"
        func get_data() -> {a = [false, true, true]}

        func main() -> (
          get_data().a
          | std::map(func (y: bool) -> !y)
        )
        "#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      (NOT r0.value) AS value
    FROM
      (
        SELECT
          0::INT8 AS index,
          FALSE AS value
        UNION
        ALL
        SELECT
          1::INT8 AS index,
          TRUE AS value
        UNION
        ALL
        SELECT
          2::INT8 AS index,
          TRUE AS value
      ) AS r0
    ORDER BY
      r0.index
    ---
    [
      true,
      false,
      false,
    ]
    ");
}

#[test]
fn serialize_text_array() {
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"
        func get_data() -> {a = ["no", "yes", "neither"]}

        func main() -> (
          get_data().a
          | std::map(func (y: text) -> y)
        )
        "#,
        lutra_bin::Value::unit()
    )), @r#"
    SELECT
      r0.value AS value
    FROM
      (
        SELECT
          0::INT8 AS index,
          'no'::text AS value
        UNION
        ALL
        SELECT
          1::INT8 AS index,
          'yes'::text AS value
        UNION
        ALL
        SELECT
          2::INT8 AS index,
          'neither'::text AS value
      ) AS r0
    ORDER BY
      r0.index
    ---
    [
      "no",
      "yes",
      "neither",
    ]
    "#);
}

#[test]
fn serialize_deeply_nested() {
    // Access deeply nested serialized data
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"
        func main() -> (
          {"hello", [[true]]}.1
        )
        "#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      (
        SELECT
          COALESCE(
            list(
              r0.value
              ORDER BY
                r0.index
            ),
            CAST([] AS BOOL [])
          ) AS value
        FROM
          (
            SELECT
              0::INT8 AS index,
              TRUE AS value
          ) AS r0
      ) AS value
    ORDER BY
      0::INT8
    ---
    [
      [
        true,
      ],
    ]
    ");
}

#[test]
fn serialize_complex_nested() {
    // Complex deeply nested structure
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"
        func main() -> (
          {
            "hello",
            [
              {
                "hello",
                [{1: int32, {1: int32, [5: int32], 2: int16}, 2: int16}]
              }.1
            ]
          }.1
        )
        "#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      (
        SELECT
          COALESCE(
            list(
              struct_pack(
                field0 := r1._0,
                field1 := struct_pack(
                  field0 := r1._1_0,
                  field1 := r1._1_1,
                  field2 := r1._1_2
                ),
                field2 := r1._2
              )
              ORDER BY
                r1.index
            ),
            CAST(
              [] AS STRUCT(
                field0 INT4,
                field1 STRUCT(field0 INT4, field1 INT4 [], field2 INT2),
                field2 INT2
              ) []
            )
          ) AS value
        FROM
          (
            SELECT
              0::INT8 AS index,
              1::INT4 AS _0,
              1::INT4 AS _1_0,
              (
                SELECT
                  COALESCE(
                    list(
                      r0.value
                      ORDER BY
                        r0.index
                    ),
                    CAST([] AS INT4 [])
                  ) AS value
                FROM
                  (
                    SELECT
                      0::INT8 AS index,
                      5::INT4 AS value
                  ) AS r0
              ) AS _1_1,
              2::INT2 AS _1_2,
              2::INT2 AS _2
          ) AS r1
      ) AS value
    ORDER BY
      0::INT8
    ---
    [
      [
        {
          1,
          {
            1,
            [
              5,
            ],
            2,
          },
          2,
        },
      ],
    ]
    ");
}

#[test]
fn serialize_index_nested_array() {
    // Index into nested array
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"
        func main() -> (
          std::index([["hello"]], 0)
        )
        "#,
        lutra_bin::Value::unit()
    )), @r#"
    SELECT
      r1.value AS value
    FROM
      (
        SELECT
          0::INT8 AS index,
          (
            SELECT
              COALESCE(
                list(
                  r0.value
                  ORDER BY
                    r0.index
                ),
                CAST([] AS TEXT [])
              ) AS value
            FROM
              (
                SELECT
                  0::INT8 AS index,
                  'hello'::text AS value
              ) AS r0
          ) AS value
        ORDER BY
          index
        LIMIT
          1::INT8
      ) AS r1
    UNION
    ALL
    SELECT
      NULL::TEXT [] AS index
    LIMIT
      1::INT8
    ---
    some([
      "hello",
    ])
    "#);
}

#[test]
fn serialize_input_array_in_tuple() {
    // Input array directly placed in tuple (should avoid unnecessary serialize/deserialize)
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"
        func main(x: [int32]) -> {"hello", x}
        "#,
        lutra_bin::Value::Array(vec![
            lutra_bin::Value::Prim32(1),
            lutra_bin::Value::Prim32(2),
            lutra_bin::Value::Prim32(3),
        ])
    )), @r#"
    SELECT
      r0._0 AS field0,
      r0._1 AS x
    FROM
      (
        SELECT
          'hello'::text AS _0,
          $1::INT4 [] AS _1
      ) AS r0
    ---
    {
      "hello",
      x = [
        1,
        2,
        3,
      ],
    }
    "#);
}

#[test]
fn serialize_literal_array_in_tuple() {
    // Literal array in tuple
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"
        func main() -> {"hello", [1: int32, 2]}
        "#,
        lutra_bin::Value::unit()
    )), @r#"
    SELECT
      r1._0 AS field0,
      r1._1 AS field1
    FROM
      (
        SELECT
          'hello'::text AS _0,
          (
            SELECT
              COALESCE(
                list(
                  r0.value
                  ORDER BY
                    r0.index
                ),
                CAST([] AS INT4 [])
              ) AS value
            FROM
              (
                SELECT
                  0::INT8 AS index,
                  1::INT4 AS value
                UNION
                ALL
                SELECT
                  1::INT8 AS index,
                  2::INT4 AS value
              ) AS r0
          ) AS _1
      ) AS r1
    ---
    {
      "hello",
      [
        1,
        2,
      ],
    }
    "#);
}

// ============================================================================
// Serialize with Options
// ============================================================================

#[test]
fn serialize_option_in_array() {
    // Array of options in a tuple
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"
        func main() -> {
          data = [.none, .some(5)]: [enum { none, some: int32 }]
        }
        "#,
        lutra_bin::Value::unit()
    )), @"
    SELECT
      r1._0 AS data
    FROM
      (
        SELECT
          (
            SELECT
              COALESCE(
                list(
                  r0.value
                  ORDER BY
                    r0.index
                ),
                CAST([] AS INT4 [])
              ) AS value
            FROM
              (
                SELECT
                  0::INT8 AS index,
                  NULL::INT4 AS value
                UNION
                ALL
                SELECT
                  1::INT8 AS index,
                  5::INT4 AS value
              ) AS r0
          ) AS _0
      ) AS r1
    ---
    {
      data = [
        none,
        some(5),
      ],
    }
    ");
}

#[test]
fn serialize_tuple_with_option_array() {
    // Tuple containing both primitive and option array
    insta::assert_snapshot!(_sql_and_output(_run(
        r#"
        func main() -> {
          name = "test",
          values = [.some(1), .none, .some(3)]: [enum {none, some: int32}]
        }
        "#,
        lutra_bin::Value::unit()
    )), @r#"
    SELECT
      r1._0 AS name,
      r1._1 AS
    values
    FROM
      (
        SELECT
          'test'::text AS _0,
          (
            SELECT
              COALESCE(
                list(
                  r0.value
                  ORDER BY
                    r0.index
                ),
                CAST([] AS INT4 [])
              ) AS value
            FROM
              (
                SELECT
                  0::INT8 AS index,
                  1::INT4 AS value
                UNION
                ALL
                SELECT
                  1::INT8 AS index,
                  NULL::INT4 AS value
                UNION
                ALL
                SELECT
                  2::INT8 AS index,
                  3::INT4 AS value
              ) AS r0
          ) AS _1
      ) AS r1
    ---
    {
      name = "test",
      values = [
        some(1),
        none,
        some(3),
      ],
    }
    "#);
}

// ============================================================================
// Parquet Tests
// ============================================================================

#[test]
fn fs_read_parquet() {
    crate::init_logger();

    insta::assert_snapshot!(_compile(
    r#"func main(): [{id: int32, name: text}] -> std::fs::read_parquet("test.parquet")"#,
    ).unwrap().2, @"
    SELECT
      r1._0 AS id,
      r1._1 AS name
    FROM
      (
        SELECT
          r0.id AS _0,
          r0.name AS _1
        FROM
          read_parquet('test.parquet'::text) AS r0
      ) AS r1
    ");
}

#[test]
fn fs_write_parquet() {
    crate::init_logger();

    insta::assert_snapshot!(_compile(
    r#"func main(x: [{id: int32, name: text}]) -> std::fs::write_parquet(x, "test.parquet")"#,
    ).unwrap().2, @"
    COPY(
      (
        SELECT
          r0._0 AS id,
          r0._1 AS name
        FROM
          (
            SELECT
              $1::STRUCT(id INT4, name TEXT) [] AS _0
          ) AS r0
      )
    ) TO 'test.parquet'::text (FORMAT parquet, COMPRESSION zstd)
    ");
}
