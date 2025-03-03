#[track_caller]
fn _test_run(lutra_source: &str) -> String {
    tracing_subscriber::fmt::Subscriber::builder()
        .without_time()
        .with_max_level(tracing::Level::DEBUG)
        .try_init()
        .ok();

    let program = lutra_compiler::_test_compile(lutra_source).unwrap_or_else(|e| panic!("{e}"));
    eprintln!("--- ir:\n{}", lutra_ir::print(&program));
    let output_ty = program.get_output_ty().clone();
    let query_sql = lutra_compiler::compile_to_sql(&program);

    let params = sqlformat::QueryParams::None;
    let options = sqlformat::FormatOptions::default();
    let formatted_sql = sqlformat::format(&query_sql, &params, &options);
    eprintln!("--- sql:\n{formatted_sql}");

    let res = async {
        const POSTGRES_URL: &str = "postgresql://postgres:pass@localhost:5416";

        let (client, connection) = tokio_postgres::connect(POSTGRES_URL, tokio_postgres::NoTls)
            .await
            .unwrap();

        tokio::spawn(async move {
            if let Err(e) = connection.await {
                eprintln!("connection error: {}", e);
            }
        });

        let (rel_ty, rel_data) = lutra_db_driver::query(client, &query_sql).await.unwrap();
        let output = lutra_db_driver::repack(&rel_ty, rel_data, &output_ty);
        let output = output.flatten();

        let output = lutra_bin::Value::decode(&output, &output_ty).unwrap();
        let output_source = output.print_source(&output_ty).unwrap();

        format!("{formatted_sql}\n{output_source}")
    };

    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();

    rt.block_on(res)
}

#[test]
fn array_of_tuples() {
    insta::assert_snapshot!(_test_run(r#"
        func () -> [{3, false}, {6, true}, {12, false}]
    "#), @r#"
    SELECT
      f_0,
      f_1
    FROM
      (
        SELECT
          0 AS index,
          3::int8 AS f_0,
          false AS f_1
        UNION
        ALL
        SELECT
          1,
          6::int8,
          true
        UNION
        ALL
        SELECT
          2,
          12::int8,
          false
      )
    [
      {
        3,
        false,
      },
      {
        6,
        true,
      },
      {
        12,
        false,
      },
    ]
    "#);
}

#[test]
fn array_of_primitives() {
    insta::assert_snapshot!(_test_run(r#"
        func () -> [3, 6, 12]
    "#), @r#"
    SELECT
      value
    FROM
      (
        SELECT
          0 AS index,
          3::int8 AS value
        UNION
        ALL
        SELECT
          1,
          6::int8
        UNION
        ALL
        SELECT
          2,
          12::int8
      )
    [
      3,
      6,
      12,
    ]
    "#);
}

#[test]
fn tuple() {
    insta::assert_snapshot!(_test_run(r#"
        func () -> {3, false}
    "#), @r#"
    SELECT
      3::int8 AS f_0,
      false AS f_1
    {
      3,
      false,
    }
    "#);
}

#[test]
fn primitive() {
    insta::assert_snapshot!(_test_run(r#"
        func () -> 3
    "#), @r#"
    SELECT
      3::int8 AS value
    3
    "#);
}

#[test]
fn std_mul() {
    insta::assert_snapshot!(_test_run("func () -> 2 * 3"), @r#"
    SELECT
      (2::int8 * 3::int8) AS value
    6
    "#);

    insta::assert_snapshot!(_test_run("func () -> 2.1 * 3.5"), @r#"
    SELECT
      (2.1::float8 * 3.5::float8) AS value
    7.3500000000000005
    "#);
}

#[test]
fn std_div() {
    insta::assert_snapshot!(_test_run("func () -> 10 / 6"), @r#"
    SELECT
      (10::int8 / 6::int8) AS value
    1
    "#);
    insta::assert_snapshot!(_test_run("func () -> -10 / 6"), @r#"
    SELECT
      ((- 10::int8) / 6::int8) AS value
    -1
    "#);
    insta::assert_snapshot!(_test_run("func () -> 10 / -6"), @r#"
    SELECT
      (10::int8 / (- 6::int8)) AS value
    -1
    "#);
    insta::assert_snapshot!(_test_run("func () -> -10 / -6"), @r#"
    SELECT
      ((- 10::int8) / (- 6::int8)) AS value
    1
    "#);

    insta::assert_snapshot!(_test_run("func () -> 10.0 / 6.0"), @r#"
    SELECT
      (10.0::float8 / 6.0::float8) AS value
    1.6666666666666667
    "#);
    insta::assert_snapshot!(_test_run("func () -> -10.0 / 6.0"), @r#"
    SELECT
      ((- 10.0::float8) / 6.0::float8) AS value
    -1.6666666666666667
    "#);
    insta::assert_snapshot!(_test_run("func () -> 10.0 / -6.0"), @r#"
    SELECT
      (10.0::float8 / (- 6.0::float8)) AS value
    -1.6666666666666667
    "#);
    insta::assert_snapshot!(_test_run("func () -> -10.0 / -6.0"), @r#"
    SELECT
      ((- 10.0::float8) / (- 6.0::float8)) AS value
    1.6666666666666667
    "#);
}

#[test]
fn std_mod() {
    insta::assert_snapshot!(_test_run("func () -> 10 % 6"), @r#"
    SELECT
      (10::int8 % 6::int8) AS value
    4
    "#);
    insta::assert_snapshot!(_test_run("func () -> -10 % 6"), @r#"
    SELECT
      ((- 10::int8) % 6::int8) AS value
    -4
    "#);
    insta::assert_snapshot!(_test_run("func () -> 10 % -6"), @r#"
    SELECT
      (10::int8 % (- 6::int8)) AS value
    4
    "#);
    insta::assert_snapshot!(_test_run("func () -> -10 % -6"), @r#"
    SELECT
      ((- 10::int8) % (- 6::int8)) AS value
    -4
    "#);

    insta::assert_snapshot!(_test_run("func () -> 10.0 % 6.0"), @r#"
    SELECT
      MOD(10.0::float8::numeric, 6.0::float8::numeric)::float8 AS value
    4
    "#);
    insta::assert_snapshot!(_test_run("func () -> -10.0 % 6.0"), @r#"
    SELECT
      MOD((- 10.0::float8)::numeric, 6.0::float8::numeric)::float8 AS value
    -4
    "#);
    insta::assert_snapshot!(_test_run("func () -> 10.0 % -6.0"), @r#"
    SELECT
      MOD(10.0::float8::numeric, (- 6.0::float8)::numeric)::float8 AS value
    4
    "#);
    insta::assert_snapshot!(_test_run("func () -> -10.0 % -6.0"), @r#"
    SELECT
      MOD(
        (- 10.0::float8)::numeric,
        (- 6.0::float8)::numeric
      )::float8 AS value
    -4
    "#);
}

#[test]
fn std_add() {
    insta::assert_snapshot!(_test_run("func () -> {30 + 2, 2 + 30}"), @r#"
    SELECT
      (30::int8 + 2::int8) AS f_0,
      (2::int8 + 30::int8) AS f_1
    {
      32,
      32,
    }
    "#);

    insta::assert_snapshot!(_test_run("func () -> 30.2 + 2.30"), @r#"
    SELECT
      (30.2::float8 + 2.3::float8) AS value
    32.5
    "#);
    insta::assert_snapshot!(_test_run("func () -> 2.30 + 30.2"), @r#"
    SELECT
      (2.3::float8 + 30.2::float8) AS value
    32.5
    "#);
}

#[test]
fn std_sub() {
    insta::assert_snapshot!(_test_run("func () -> {30 - 2, 2 - 30}"), @r#"
    SELECT
      (30::int8 - 2::int8) AS f_0,
      (2::int8 - 30::int8) AS f_1
    {
      28,
      -28,
    }
    "#);

    insta::assert_snapshot!(_test_run("func () -> 30.2 - 2.30"), @r#"
    SELECT
      (30.2::float8 - 2.3::float8) AS value
    27.9
    "#);
    insta::assert_snapshot!(_test_run("func () -> 2.30 - 30.2"), @r#"
    SELECT
      (2.3::float8 - 30.2::float8) AS value
    -27.9
    "#);
}

#[test]
fn std_neg() {
    insta::assert_snapshot!(_test_run("func () -> {-2, - (-3)}"), @r#"
    SELECT
      (- 2::int8) AS f_0,
      (- (- 3::int8)) AS f_1
    {
      -2,
      3,
    }
    "#);
    insta::assert_snapshot!(_test_run("func () -> {-2.1, - (-3.1)}"), @r#"
    SELECT
      (- 2.1::float8) AS f_0,
      (- (- 3.1::float8)) AS f_1
    {
      -2.1,
      3.1,
    }
    "#);
}

#[test]
fn std_eq() {
    insta::assert_snapshot!(_test_run("func () -> {30 == 2, 30 == 30}"), @r#"
    SELECT
      (30::int8 = 2::int8) AS f_0,
      (30::int8 = 30::int8) AS f_1
    {
      false,
      true,
    }
    "#);
}

#[test]
fn std_ne() {
    insta::assert_snapshot!(_test_run("func () -> {30 != 2, 30 != 30}"), @r#"
    SELECT
      (30::int8 <> 2::int8) AS f_0,
      (30::int8 <> 30::int8) AS f_1
    {
      true,
      false,
    }
    "#);
}

#[test]
fn std_gt() {
    insta::assert_snapshot!(_test_run("func () -> {3 > 2, 2 > 3, 2 > 2}"), @r#"
    SELECT
      (3::int8 > 2::int8) AS f_0,
      (2::int8 > 3::int8) AS f_1,
      (2::int8 > 2::int8) AS f_2
    {
      true,
      false,
      false,
    }
    "#);
}

#[test]
fn std_lt() {
    insta::assert_snapshot!(_test_run("func () -> {3 < 2, 2 < 3, 2 < 2}"), @r#"
    SELECT
      (3::int8 < 2::int8) AS f_0,
      (2::int8 < 3::int8) AS f_1,
      (2::int8 < 2::int8) AS f_2
    {
      false,
      true,
      false,
    }
    "#);
}

#[test]
fn std_gte() {
    insta::assert_snapshot!(_test_run("func () -> {3 >= 2, 2 >= 3, 2 >= 2}"), @r#"
    SELECT
      (3::int8 >= 2::int8) AS f_0,
      (2::int8 >= 3::int8) AS f_1,
      (2::int8 >= 2::int8) AS f_2
    {
      true,
      false,
      true,
    }
    "#);
}

#[test]
fn std_lte() {
    insta::assert_snapshot!(_test_run("func () -> {3 <= 2, 2 <= 3, 2 <= 2}"), @r#"
    SELECT
      (3::int8 <= 2::int8) AS f_0,
      (2::int8 <= 3::int8) AS f_1,
      (2::int8 <= 2::int8) AS f_2
    {
      false,
      true,
      true,
    }
    "#);
}

#[test]
fn std_and() {
    insta::assert_snapshot!(_test_run("func () -> {false && false, false && true, true && false, true && true}"), @r#"
    SELECT
      (
        false
        AND false
      ) AS f_0,
      (
        false
        AND true
      ) AS f_1,
      (
        true
        AND false
      ) AS f_2,
      (
        true
        AND true
      ) AS f_3
    {
      false,
      false,
      false,
      true,
    }
    "#);
}

#[test]
fn std_or() {
    insta::assert_snapshot!(_test_run("func () -> {false || false, false || true, true || false, true || true}"), @r#"
    SELECT
      (
        false
        OR false
      ) AS f_0,
      (
        false
        OR true
      ) AS f_1,
      (
        true
        OR false
      ) AS f_2,
      (
        true
        OR true
      ) AS f_3
    {
      false,
      true,
      true,
      true,
    }
    "#);
}

#[test]
fn std_not() {
    insta::assert_snapshot!(_test_run("func () -> {!false, !true}"), @r#"
    SELECT
      (NOT false) AS f_0,
      (NOT true) AS f_1
    {
      true,
      false,
    }
    "#);
}

#[test]
fn std_slice() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> (
        [{3, false}, {6, true}, {12, false}]
        | std::slice(1, 3)
    )
    "#), @r#"
    SELECT
      f_0,
      f_1
    FROM
      (
        SELECT
          0 AS index,
          3::int8 AS f_0,
          false AS f_1
        UNION
        ALL
        SELECT
          1,
          6::int8,
          true
        UNION
        ALL
        SELECT
          2,
          12::int8,
          false
        LIMIT
          (3::int8 - 1::int8) OFFSET 1::int8
      )
    [
      {
        6,
        true,
      },
      {
        12,
        false,
      },
    ]
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> (
        [3, 6, 12, 24]
        | std::slice(1, 3)
    )
    "#), @r#"
    SELECT
      value
    FROM
      (
        SELECT
          0 AS index,
          3::int8 AS value
        UNION
        ALL
        SELECT
          1,
          6::int8
        UNION
        ALL
        SELECT
          2,
          12::int8
        UNION
        ALL
        SELECT
          3,
          24::int8
        LIMIT
          (3::int8 - 1::int8) OFFSET 1::int8
      )
    [
      6,
      12,
    ]
    "#);
    insta::assert_snapshot!(_test_run(r#"
    func () -> (
        [3, 6, 12, 24, 48]
        | std::slice(1, 4)
        | std::slice(1, 3)
    )
    "#), @r#"
    SELECT
      value
    FROM
      (
        SELECT
          index,
          value
        FROM
          (
            SELECT
              0 AS index,
              3::int8 AS value
            UNION
            ALL
            SELECT
              1,
              6::int8
            UNION
            ALL
            SELECT
              2,
              12::int8
            UNION
            ALL
            SELECT
              3,
              24::int8
            UNION
            ALL
            SELECT
              4,
              48::int8
            LIMIT
              (4::int8 - 1::int8) OFFSET 1::int8
          )
        LIMIT
          (3::int8 - 1::int8) OFFSET 1::int8
      )
    [
      12,
      24,
    ]
    "#);
}
