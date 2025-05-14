use lutra_compiler::{pr, SourceTree};

#[track_caller]
pub fn _run(source: &str, params: Vec<lutra_bin::Value>) -> (String, String) {
    crate::init_logger();

    // compile
    let source = SourceTree::single("".into(), source.to_string());
    let project = match lutra_compiler::compile(source, Default::default()) {
        Ok(p) => p,
        Err(e) => panic!("{e}"),
    };

    // compile to sql
    let (program, query_sql) =
        lutra_compiler::compile_to_sql(&project, &pr::Path::from_name("main"));

    // format sql
    let options = sqlformat::FormatOptions::default();
    let formatted_sql = sqlformat::format(&query_sql, &sqlformat::QueryParams::None, &options);
    tracing::debug!("sql:\n{formatted_sql}");

    let params: Vec<_> = std::iter::zip(params, program.get_input_tys())
        .map(|(value, ty)| value.encode(ty, &program.types).unwrap())
        .collect();

    // execute
    async fn inner(
        query_sql: &str,
        params: &[Vec<u8>],
        expected_ty: &lutra_bin::ir::Ty,
        ty_defs: &[lutra_bin::ir::TyDef],
    ) -> Result<lutra_bin::bytes::Bytes, tokio_postgres::Error> {
        const POSTGRES_URL: &str = "postgresql://postgres:pass@localhost:5416";

        let (client, connection) = tokio_postgres::connect(POSTGRES_URL, tokio_postgres::NoTls)
            .await
            .unwrap();

        tokio::spawn(async move {
            if let Err(e) = connection.await {
                eprintln!("connection error: {}", e);
            }
        });

        let params = params.iter().map(|p| p.as_slice());
        lutra_db_driver::query(client, query_sql, params, expected_ty, ty_defs).await
    }
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();

    let rel_data = rt
        .block_on(inner(
            &query_sql,
            &params,
            program.get_output_ty(),
            &program.types,
        ))
        .unwrap();

    let output =
        lutra_bin::Value::decode(&rel_data, program.get_output_ty(), &program.types).unwrap();
    let output = output
        .print_source(program.get_output_ty(), &program.types)
        .unwrap();

    (formatted_sql, output)
}

#[track_caller]
pub fn _run_to_str(lutra_source: &str) -> String {
    let (sql, output) = _run(lutra_source, vec![]);
    format!("{sql}\n---\n{output}")
}

#[test]
fn prim() {
    insta::assert_snapshot!(_run_to_str(r#"
        func () -> 3
    "#), @r#"
    SELECT
      r0.value
    FROM
      (
        SELECT
          3::int8 AS value
      ) AS r0
    ---
    3
    "#);
}

#[test]
fn tuple_prim() {
    insta::assert_snapshot!(_run_to_str(r#"
        func () -> {3, false}
    "#), @r#"
    SELECT
      r0._0,
      r0._1
    FROM
      (
        SELECT
          3::int8 AS _0,
          FALSE AS _1
      ) AS r0
    ---
    {
      3,
      false,
    }
    "#);
}

#[test]
fn array_prim() {
    insta::assert_snapshot!(_run_to_str(r#"
        func () -> [3, 6, 12]
    "#), @r#"
    SELECT
      r0.value
    FROM
      (
        SELECT
          0::int8 AS index,
          3::int8 AS value
        UNION
        ALL
        SELECT
          1::int8,
          6::int8
        UNION
        ALL
        SELECT
          2::int8,
          12::int8
      ) AS r0
    ORDER BY
      index
    ---
    [
      3,
      6,
      12,
    ]
    "#);
}

#[test]
fn array_empty() {
    insta::assert_snapshot!(_run_to_str(r#"
        func () -> []: [bool]
    "#), @r#"
    SELECT
      r0.value
    FROM
      (
        SELECT
          0 AS index,
          NULL::bool AS value
        WHERE
          false
      ) AS r0
    ORDER BY
      index
    ---
    []
    "#);
}

#[test]
fn tuple_tuple_prim() {
    insta::assert_snapshot!(_run_to_str(r#"
        func () -> {3, {false, true, {"hello"}, 4}}
    "#), @r#"
    SELECT
      r0._0,
      r0._1_0,
      r0._1_1,
      r0._1_2_0,
      r0._1_3
    FROM
      (
        SELECT
          3::int8 AS _0,
          FALSE AS _1_0,
          TRUE AS _1_1,
          'hello' AS _1_2_0,
          4::int8 AS _1_3
      ) AS r0
    ---
    {
      3,
      {
        false,
        true,
        {
          "hello",
        },
        4,
      },
    }
    "#);
}

#[test]
fn tuple_array_prim() {
    insta::assert_snapshot!(_run_to_str(r#"
        func () -> {true, [1, 2, 3], [4], false}
    "#), @r#"
    SELECT
      r2._0,
      r2._1,
      r2._2,
      r2._3
    FROM
      (
        SELECT
          TRUE AS _0,
          (
            SELECT
              COALESCE(
                jsonb_agg(
                  r0.value
                  ORDER BY
                    r0.index
                ),
                '[]'::jsonb
              )
            FROM
              (
                SELECT
                  0::int8 AS index,
                  1::int8 AS value
                UNION
                ALL
                SELECT
                  1::int8,
                  2::int8
                UNION
                ALL
                SELECT
                  2::int8,
                  3::int8
              ) AS r0
          ) AS _1,
          (
            SELECT
              COALESCE(
                jsonb_agg(
                  r1.value
                  ORDER BY
                    r1.index
                ),
                '[]'::jsonb
              )
            FROM
              (
                SELECT
                  0::int8 AS index,
                  4::int8 AS value
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
        4,
      ],
      false,
    }
    "#);
}

#[test]
fn tuple_array_empty() {
    insta::assert_snapshot!(_run_to_str(r#"
        func () -> {true, []: [int64], false}
    "#), @r#"
    SELECT
      r1._0,
      r1._1,
      r1._2
    FROM
      (
        SELECT
          TRUE AS _0,
          (
            SELECT
              COALESCE(
                jsonb_agg(
                  r0.value
                  ORDER BY
                    r0.index
                ),
                '[]'::jsonb
              )
            FROM
              (
                SELECT
                  0 AS index,
                  NULL::int8 AS value
                WHERE
                  false
              ) AS r0
          ) AS _1,
          FALSE AS _2
      ) AS r1
    ---
    {
      true,
      [],
      false,
    }
    "#);
}

#[test]
fn array_array_prim() {
    insta::assert_snapshot!(_run_to_str(r#"
        func () -> [[1, 2, 3], [4, 5]]
    "#), @r#"
    SELECT
      r2.value
    FROM
      (
        SELECT
          0::int8 AS index,
          (
            SELECT
              COALESCE(
                jsonb_agg(
                  r0.value
                  ORDER BY
                    r0.index
                ),
                '[]'::jsonb
              )
            FROM
              (
                SELECT
                  0::int8 AS index,
                  1::int8 AS value
                UNION
                ALL
                SELECT
                  1::int8,
                  2::int8
                UNION
                ALL
                SELECT
                  2::int8,
                  3::int8
              ) AS r0
          ) AS value
        UNION
        ALL
        SELECT
          1::int8,
          (
            SELECT
              COALESCE(
                jsonb_agg(
                  r1.value
                  ORDER BY
                    r1.index
                ),
                '[]'::jsonb
              )
            FROM
              (
                SELECT
                  0::int8 AS index,
                  4::int8 AS value
                UNION
                ALL
                SELECT
                  1::int8,
                  5::int8
              ) AS r1
          )
      ) AS r2
    ORDER BY
      index
    ---
    [
      [
        1,
        2,
        3,
      ],
      [
        4,
        5,
      ],
    ]
    "#);
}

#[test]
fn array_tuple_prim() {
    insta::assert_snapshot!(_run_to_str(r#"
        func () -> [{3, false}, {6, true}, {12, false}]
    "#), @r#"
    SELECT
      r0._0,
      r0._1
    FROM
      (
        SELECT
          0::int8 AS index,
          3::int8 AS _0,
          FALSE AS _1
        UNION
        ALL
        SELECT
          1::int8,
          6::int8,
          TRUE
        UNION
        ALL
        SELECT
          2::int8,
          12::int8,
          FALSE
      ) AS r0
    ORDER BY
      index
    ---
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
fn tuple_array_tuple_prim() {
    insta::assert_snapshot!(_run_to_str(r#"
        func () -> {
            "hello",
            [{3, false}, {6, true}, {12, false}],
        }
    "#), @r#"
    SELECT
      r1._0,
      r1._1
    FROM
      (
        SELECT
          'hello' AS _0,
          (
            SELECT
              COALESCE(
                jsonb_agg(
                  jsonb_build_array(r0._0, r0._1)
                  ORDER BY
                    r0.index
                ),
                '[]'::jsonb
              )
            FROM
              (
                SELECT
                  0::int8 AS index,
                  3::int8 AS _0,
                  FALSE AS _1
                UNION
                ALL
                SELECT
                  1::int8,
                  6::int8,
                  TRUE
                UNION
                ALL
                SELECT
                  2::int8,
                  12::int8,
                  FALSE
              ) AS r0
          ) AS _1
      ) AS r1
    ---
    {
      "hello",
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
      ],
    }
    "#);
}

#[test]
fn param_00() {
    insta::assert_snapshot!(_run(r#"
    func (x: int64, y: text) -> {x, y}
    "#,
    vec![lutra_bin::Value::Int64(3), lutra_bin::Value::Text("hello".into())]
    ).1, @r#"
    {
      3,
      "hello",
    }
    "#);
}

#[test]
fn tuple_unpacking_00() {
    insta::assert_snapshot!(_run(r#"
    func () -> {
      4,
      ([{id = 3, title = "Hello world!"}] | std::index(0)),
    }
    "#,
    vec![]
    ).1, @r#"
    {
      4,
      {
        id = 3,
        title = "Hello world!",
      },
    }
    "#);
}

#[test]
fn json_pack_00() {
    // Having array in a tuple forces it to be packed to JSON.
    // Applying an operation of that array then forces it to unpack.

    insta::assert_snapshot!(_run(r#"
    let get_data = func () -> {a = [2, 5, 4, 3, 1, 2]}

    func () -> (
      get_data().a
      | std::map(func (y: int64) -> -y)
    )
    "#, vec![]).1, @r#"
    [
      -2,
      -5,
      -4,
      -3,
      -1,
      -2,
    ]
    "#);
}

#[test]
fn json_pack_01() {
    // Having array in a tuple forces it to be packed to JSON.
    // Applying an operation of that array then forces it to unpack.

    insta::assert_snapshot!(_run(r#"
    let get_data = func () -> {a = [{2, false}, {5, true}, {4, false}]}

    func () -> (
      get_data().a
      | std::map(func (y: {int64, bool}) -> {-y.0, !y.1})
    )
    "#, vec![]).1, @r#"
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
    "#);
}

#[test]
fn json_pack_02() {
    insta::assert_snapshot!(_run(r#"
    let get_data = func () -> [[1, 2, 3], [4, 5, 6]]

    func () -> (
      get_data() | std::map(func (y: [int64]) -> (
        std::index(y, 1)
      ))
    )
    "#, vec![]).1, @r#"
    [
      2,
      5,
    ]
    "#);
}

#[test]
fn json_pack_03() {
    insta::assert_snapshot!(_run(r#"
    let get_data = func () -> [[1, 2, 3], [4, 5, 6]]

    func () -> (
      get_data()
      | std::map(func (y: [int64]) -> (
        y | std::map(func (z: int64) -> 6-z)
      ))
    )
    "#, vec![]).1, @r#"
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
    "#);
}

#[test]
fn json_pack_04() {
    insta::assert_snapshot!(_run(r#"
    let get_data = func () -> {a = [false, true, true]}

    func () -> (
      get_data().a
      | std::map(func (y: bool) -> !y)
    )
    "#, vec![]).1, @r#"
    [
      true,
      false,
      false,
    ]
    "#);
}

#[test]
fn json_pack_05() {
    insta::assert_snapshot!(_run(r#"
    let get_data = func () -> {a = ["no", "yes", "neither"]}

    func () -> (
      get_data().a
      | std::map(func (y: text) -> y)
    )
    "#, vec![]).1, @r#"
    [
      "no",
      "yes",
      "neither",
    ]
    "#);
}
