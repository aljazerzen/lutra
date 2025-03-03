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
    ORDER BY
      index
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
    ORDER BY
      index
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
    insta::assert_snapshot!(_test_run(r#"
    func () -> 2 * 3
    "#), @r#"
    SELECT
      (2::int8 * 3::int8) AS value
    6
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> 2.1 * 3.5
    "#), @r#"
    SELECT
      (2.1::float8 * 3.5::float8) AS value
    7.3500000000000005
    "#);
}

#[test]
fn std_div() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> 10 / 6
    "#), @r#"
    SELECT
      (10::int8 / 6::int8) AS value
    1
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> -10 / 6
    "#), @r#"
    SELECT
      ((- 10::int8) / 6::int8) AS value
    -1
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> 10 / -6
    "#), @r#"
    SELECT
      (10::int8 / (- 6::int8)) AS value
    -1
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> -10 / -6
    "#), @r#"
    SELECT
      ((- 10::int8) / (- 6::int8)) AS value
    1
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> 10.0 / 6.0
    "#), @r#"
    SELECT
      (10.0::float8 / 6.0::float8) AS value
    1.6666666666666667
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> -10.0 / 6.0
    "#), @r#"
    SELECT
      ((- 10.0::float8) / 6.0::float8) AS value
    -1.6666666666666667
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> 10.0 / -6.0
    "#), @r#"
    SELECT
      (10.0::float8 / (- 6.0::float8)) AS value
    -1.6666666666666667
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> -10.0 / -6.0
    "#), @r#"
    SELECT
      ((- 10.0::float8) / (- 6.0::float8)) AS value
    1.6666666666666667
    "#);
}

#[test]
fn std_mod() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> 10 % 6
    "#), @r#"
    SELECT
      (10::int8 % 6::int8) AS value
    4
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> -10 % 6
    "#), @r#"
    SELECT
      ((- 10::int8) % 6::int8) AS value
    -4
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> 10 % -6
    "#), @r#"
    SELECT
      (10::int8 % (- 6::int8)) AS value
    4
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> -10 % -6
    "#), @r#"
    SELECT
      ((- 10::int8) % (- 6::int8)) AS value
    -4
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> 10.0 % 6.0
    "#), @r#"
    SELECT
      MOD(10.0::float8::numeric, 6.0::float8::numeric)::float8 AS value
    4
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> -10.0 % 6.0
    "#), @r#"
    SELECT
      MOD((- 10.0::float8)::numeric, 6.0::float8::numeric)::float8 AS value
    -4
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> 10.0 % -6.0
    "#), @r#"
    SELECT
      MOD(10.0::float8::numeric, (- 6.0::float8)::numeric)::float8 AS value
    4
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> -10.0 % -6.0
    "#), @r#"
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
    insta::assert_snapshot!(_test_run(r#"
    func () -> {30 + 2, 2 + 30}
    "#), @r#"
    SELECT
      (30::int8 + 2::int8) AS f_0,
      (2::int8 + 30::int8) AS f_1
    {
      32,
      32,
    }
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> 30.2 + 2.30
    "#), @r#"
    SELECT
      (30.2::float8 + 2.3::float8) AS value
    32.5
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> 2.30 + 30.2
    "#), @r#"
    SELECT
      (2.3::float8 + 30.2::float8) AS value
    32.5
    "#);
}

#[test]
fn std_sub() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> {30 - 2, 2 - 30}
    "#), @r#"
    SELECT
      (30::int8 - 2::int8) AS f_0,
      (2::int8 - 30::int8) AS f_1
    {
      28,
      -28,
    }
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> 30.2 - 2.30
    "#), @r#"
    SELECT
      (30.2::float8 - 2.3::float8) AS value
    27.9
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> 2.30 - 30.2
    "#), @r#"
    SELECT
      (2.3::float8 - 30.2::float8) AS value
    -27.9
    "#);
}

#[test]
fn std_neg() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> {-2, - (-3)}
    "#), @r#"
    SELECT
      (- 2::int8) AS f_0,
      (- (- 3::int8)) AS f_1
    {
      -2,
      3,
    }
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> {-2.1, - (-3.1)}
    "#), @r#"
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
    insta::assert_snapshot!(_test_run(r#"
    func () -> {30 == 2, 30 == 30}
    "#), @r#"
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
    insta::assert_snapshot!(_test_run(r#"
    func () -> {30 != 2, 30 != 30}
    "#), @r#"
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
    insta::assert_snapshot!(_test_run(r#"
    func () -> {3 > 2, 2 > 3, 2 > 2}
    "#), @r#"
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
    insta::assert_snapshot!(_test_run(r#"
    func () -> {3 < 2, 2 < 3, 2 < 2}
    "#), @r#"
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
    insta::assert_snapshot!(_test_run(r#"
    func () -> {3 >= 2, 2 >= 3, 2 >= 2}
    "#), @r#"
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
    insta::assert_snapshot!(_test_run(r#"
    func () -> {3 <= 2, 2 <= 3, 2 <= 2}
    "#), @r#"
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
    insta::assert_snapshot!(_test_run(r#"
    func () -> {false && false, false && true, true && false, true && true}
    "#), @r#"
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
    insta::assert_snapshot!(_test_run(r#"
    func () -> {false || false, false || true, true || false, true || true}
    "#), @r#"
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
    insta::assert_snapshot!(_test_run(r#"
    func () -> {!false, !true}
    "#), @r#"
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
fn std_index() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::index([5,3,65,3,2], 3)
    "#), @r#"
    SELECT
      value
    FROM
      (
        SELECT
          0 AS index,
          5::int8 AS value
        UNION
        ALL
        SELECT
          1,
          3::int8
        UNION
        ALL
        SELECT
          2,
          65::int8
        UNION
        ALL
        SELECT
          3,
          3::int8
        UNION
        ALL
        SELECT
          4,
          2::int8
        ORDER BY
          index
        LIMIT
          1::int8 OFFSET 3::int8
      )
    ORDER BY
      index
    3
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> [1, 2, 3].2
    "#), @r#"
    SELECT
      value
    FROM
      (
        SELECT
          0 AS index,
          1::int8 AS value
        UNION
        ALL
        SELECT
          1,
          2::int8
        UNION
        ALL
        SELECT
          2,
          3::int8
        ORDER BY
          index
        LIMIT
          1::int8 OFFSET 2::int8
      )
    ORDER BY
      index
    3
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::index([5.3,3.2,65.4,3.1,2.0], 3)
    "#), @r#"
    SELECT
      value
    FROM
      (
        SELECT
          0 AS index,
          5.3::float8 AS value
        UNION
        ALL
        SELECT
          1,
          3.2::float8
        UNION
        ALL
        SELECT
          2,
          65.4::float8
        UNION
        ALL
        SELECT
          3,
          3.1::float8
        UNION
        ALL
        SELECT
          4,
          2.0::float8
        ORDER BY
          index
        LIMIT
          1::int8 OFFSET 3::int8
      )
    ORDER BY
      index
    3.1
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> [1.1, 2.2, 3.3].2
    "#), @r#"
    SELECT
      value
    FROM
      (
        SELECT
          0 AS index,
          1.1::float8 AS value
        UNION
        ALL
        SELECT
          1,
          2.2::float8
        UNION
        ALL
        SELECT
          2,
          3.3::float8
        ORDER BY
          index
        LIMIT
          1::int8 OFFSET 2::int8
      )
    ORDER BY
      index
    3.3
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::index([false, false, false, true, false], 3)
    "#), @r#"
    SELECT
      value
    FROM
      (
        SELECT
          0 AS index,
          false AS value
        UNION
        ALL
        SELECT
          1,
          false
        UNION
        ALL
        SELECT
          2,
          false
        UNION
        ALL
        SELECT
          3,
          true
        UNION
        ALL
        SELECT
          4,
          false
        ORDER BY
          index
        LIMIT
          1::int8 OFFSET 3::int8
      )
    ORDER BY
      index
    true
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> [true, true, false].2
    "#), @r#"
    SELECT
      value
    FROM
      (
        SELECT
          0 AS index,
          true AS value
        UNION
        ALL
        SELECT
          1,
          true
        UNION
        ALL
        SELECT
          2,
          false
        ORDER BY
          index
        LIMIT
          1::int8 OFFSET 2::int8
      )
    ORDER BY
      index
    false
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> ["hello", "world", "!"].2
    "#), @r#"
    SELECT
      value
    FROM
      (
        SELECT
          0 AS index,
          'hello' AS value
        UNION
        ALL
        SELECT
          1,
          'world'
        UNION
        ALL
        SELECT
          2,
          '!'
        ORDER BY
          index
        LIMIT
          1::int8 OFFSET 2::int8
      )
    ORDER BY
      index
    "!"
    "#);
}

#[test]
fn std_slice() {
    insta::assert_snapshot!(_test_run(r#"
    func () -> std::slice([5,3,65,3,2], 1, 3)
    "#), @r#"
    SELECT
      value
    FROM
      (
        SELECT
          0 AS index,
          5::int8 AS value
        UNION
        ALL
        SELECT
          1,
          3::int8
        UNION
        ALL
        SELECT
          2,
          65::int8
        UNION
        ALL
        SELECT
          3,
          3::int8
        UNION
        ALL
        SELECT
          4,
          2::int8
        ORDER BY
          index
        LIMIT
          (3::int8 - 1::int8) OFFSET 1::int8
      )
    ORDER BY
      index
    [
      3,
      65,
    ]
    "#);

    // insta::assert_snapshot!(_test_run(r#"
    // func () -> std::slice([5,3,65,3,2], 1, -1)
    // "#), @r#"
    // [
    //   3,
    //   65,
    //   3,
    // ]
    // "#);

    // insta::assert_snapshot!(_test_run(r#"
    // func () -> std::slice([5,3,65,3,2], 4, 2)
    // "#), @"[]");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::slice([5,3,65,3,2], 6, 7)
    "#), @r#"
    SELECT
      value
    FROM
      (
        SELECT
          0 AS index,
          5::int8 AS value
        UNION
        ALL
        SELECT
          1,
          3::int8
        UNION
        ALL
        SELECT
          2,
          65::int8
        UNION
        ALL
        SELECT
          3,
          3::int8
        UNION
        ALL
        SELECT
          4,
          2::int8
        ORDER BY
          index
        LIMIT
          (7::int8 - 6::int8) OFFSET 6::int8
      )
    ORDER BY
      index
    []
    "#);

    // insta::assert_snapshot!(_test_run(r#"
    // func () -> std::slice([5,3,65,3,2], -7, 0)
    // "#), @"[]");

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::slice([false,true,false,false,true], 1, 4)
    "#), @r#"
    SELECT
      value
    FROM
      (
        SELECT
          0 AS index,
          false AS value
        UNION
        ALL
        SELECT
          1,
          true
        UNION
        ALL
        SELECT
          2,
          false
        UNION
        ALL
        SELECT
          3,
          false
        UNION
        ALL
        SELECT
          4,
          true
        ORDER BY
          index
        LIMIT
          (4::int8 - 1::int8) OFFSET 1::int8
      )
    ORDER BY
      index
    [
      true,
      false,
      false,
    ]
    "#);

    insta::assert_snapshot!(_test_run(r#"
    func () -> std::slice([{false,"hello"}, {false,"world"},{true, "!"},{false,"foo"},{true, "bar"}], 1, 4)
    "#), @r#"
    SELECT
      f_0,
      f_1
    FROM
      (
        SELECT
          0 AS index,
          false AS f_0,
          'hello' AS f_1
        UNION
        ALL
        SELECT
          1,
          false,
          'world'
        UNION
        ALL
        SELECT
          2,
          true,
          '!'
        UNION
        ALL
        SELECT
          3,
          false,
          'foo'
        UNION
        ALL
        SELECT
          4,
          true,
          'bar'
        ORDER BY
          index
        LIMIT
          (4::int8 - 1::int8) OFFSET 1::int8
      )
    ORDER BY
      index
    [
      {
        false,
        "world",
      },
      {
        true,
        "!",
      },
      {
        false,
        "foo",
      },
    ]
    "#);
}

#[test]
fn bindings() {
    insta::assert_snapshot!(_test_run(r#"
    let a = 4
    func () -> [{a, a + 1}, {a + 2, a + 3}]
    "#), @r#"
    SELECT
      f_0,
      f_1
    FROM
      (
        WITH t0 AS (
          SELECT
            4::int8 AS value
        )
        SELECT
          0 AS index,
          (
            SELECT
              value
            FROM
              t0
          ) AS f_0,
          (
            (
              SELECT
                value
              FROM
                t0
            ) + 1::int8
          ) AS f_1
        UNION
        ALL
        SELECT
          1,
          (
            (
              SELECT
                value
              FROM
                t0
            ) + 2::int8
          ),
          (
            (
              SELECT
                value
              FROM
                t0
            ) + 3::int8
          )
      )
    ORDER BY
      index
    [
      {
        4,
        5,
      },
      {
        6,
        7,
      },
    ]
    "#);

    insta::assert_snapshot!(_test_run(r#"
    let a = {3, false}
    func () -> [{a.0, a.1}, {a.0 + 1, !a.1}]
    "#), @r#"
    SELECT
      f_0,
      f_1
    FROM
      (
        WITH t0 AS (
          SELECT
            3::int8 AS f_0,
            false AS f_1
        )
        SELECT
          0 AS index,
          (
            SELECT
              f_0
            FROM
              t0
          ) AS f_0,
          (
            SELECT
              f_1
            FROM
              t0
          ) AS f_1
        UNION
        ALL
        SELECT
          1,
          (
            (
              SELECT
                f_0
              FROM
                t0
            ) + 1::int8
          ),
          (
            NOT (
              SELECT
                f_1
              FROM
                t0
            )
          )
      )
    ORDER BY
      index
    [
      {
        3,
        false,
      },
      {
        4,
        true,
      },
    ]
    "#);

    insta::assert_snapshot!(_test_run(r#"
    let a = [1, 2]
    func () -> [{a.0, a.1}, {a.0 + 1, a.1 + 1}]
    "#), @r#"
    SELECT
      f_0,
      f_1
    FROM
      (
        WITH t0 AS (
          SELECT
            0 AS index,
            1::int8 AS value
          UNION
          ALL
          SELECT
            1,
            2::int8
        )
        SELECT
          0 AS index,
          (
            SELECT
              value
            FROM
              (
                SELECT
                  index,
                  value
                FROM
                  t0
                ORDER BY
                  index
                LIMIT
                  1::int8 OFFSET 0::int8
              )
            ORDER BY
              index
          ) AS f_0,
          (
            SELECT
              value
            FROM
              (
                SELECT
                  index,
                  value
                FROM
                  t0
                ORDER BY
                  index
                LIMIT
                  1::int8 OFFSET 1::int8
              )
            ORDER BY
              index
          ) AS f_1
        UNION
        ALL
        SELECT
          1,
          (
            (
              SELECT
                value
              FROM
                (
                  SELECT
                    index,
                    value
                  FROM
                    t0
                  ORDER BY
                    index
                  LIMIT
                    1::int8 OFFSET 0::int8
                )
              ORDER BY
                index
            ) + 1::int8
          ),
          (
            (
              SELECT
                value
              FROM
                (
                  SELECT
                    index,
                    value
                  FROM
                    t0
                  ORDER BY
                    index
                  LIMIT
                    1::int8 OFFSET 1::int8
                )
              ORDER BY
                index
            ) + 1::int8
          )
      )
    ORDER BY
      index
    [
      {
        1,
        2,
      },
      {
        2,
        3,
      },
    ]
    "#);
}
