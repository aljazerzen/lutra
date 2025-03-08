use lutra_compiler::{pr, SourceTree};

#[track_caller]
pub fn _run(source: &str) -> (String, String) {
    crate::init_logger();

    // compile
    let source = SourceTree::single("".into(), source.to_string());
    let project =
        lutra_compiler::compile(source, Default::default()).unwrap_or_else(|e| panic!("{e}"));

    // compile to sql
    let (program, query_sql) =
        lutra_compiler::compile_to_sql(&project, &pr::Path::from_name("main"));

    // format sql
    let params = sqlformat::QueryParams::None;
    let options = sqlformat::FormatOptions::default();
    let formatted_sql = sqlformat::format(&query_sql, &params, &options);
    tracing::debug!("sql:\n{formatted_sql}");

    // execute
    async fn inner(
        query_sql: &str,
    ) -> Result<(lutra_bin::ir::Ty, lutra_bin::Data), tokio_postgres::Error> {
        const POSTGRES_URL: &str = "postgresql://postgres:pass@localhost:5416";

        let (client, connection) = tokio_postgres::connect(POSTGRES_URL, tokio_postgres::NoTls)
            .await
            .unwrap();

        tokio::spawn(async move {
            if let Err(e) = connection.await {
                eprintln!("connection error: {}", e);
            }
        });

        lutra_db_driver::query(client, query_sql).await
    }
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();

    let (rel_ty, rel_data) = rt.block_on(inner(&query_sql)).unwrap();

    let output = lutra_db_driver::repack(&rel_ty, rel_data, program.get_output_ty());
    let output = output.flatten();

    let output =
        lutra_bin::Value::decode(&output, program.get_output_ty(), &program.types).unwrap();
    let output = output
        .print_source(program.get_output_ty(), &program.types)
        .unwrap();

    (formatted_sql, output)
}

#[track_caller]
pub fn _run_to_str(lutra_source: &str) -> String {
    let (sql, output) = _run(lutra_source);
    format!("{sql}\n---\n{output}")
}

#[test]
fn prim() {
    insta::assert_snapshot!(_run_to_str(r#"
        func () -> 3
    "#), @r#"
    SELECT
      3::int8 AS value
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
      3::int8 AS f_0,
      false AS f_1
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
      value
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
      )
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
fn tuple_tuple_prim() {
    insta::assert_snapshot!(_run_to_str(r#"
        func () -> {3, {false, true, {"hello"}, 4}}
    "#), @r#"
    SELECT
      3::int8 AS f_0,
      false AS f_1_0,
      true AS f_1_1,
      'hello' AS f_1_2_0,
      4::int8 AS f_1_3
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
        func () -> {true, [1, 2, 3], false}
    "#), @r#"
    SELECT
      true AS f_0,
      (
        SELECT
          json_agg(
            value
            ORDER BY
              index
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
          )
      ) AS f_1,
      false AS f_2
    ---
    {
      true,
      [
        1,
        2,
        3,
      ],
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
      value
    FROM
      (
        SELECT
          0::int8 AS index,
          (
            SELECT
              json_agg(
                value
                ORDER BY
                  index
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
              )
          ) AS value
        UNION
        ALL
        SELECT
          1::int8,
          (
            SELECT
              json_agg(
                value
                ORDER BY
                  index
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
              )
          )
      )
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
      f_0,
      f_1
    FROM
      (
        SELECT
          0::int8 AS index,
          3::int8 AS f_0,
          false AS f_1
        UNION
        ALL
        SELECT
          1::int8,
          6::int8,
          true
        UNION
        ALL
        SELECT
          2::int8,
          12::int8,
          false
      )
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
fn complex_00() {
    insta::assert_snapshot!(_run(r#"
        let x = [
          1, 4, 2, 3, 2, 3, 4, 5, 1, 2
        ]

        func () -> {
          a = 1 + 2,
          {3, 3 + 1},
          {2, 2 + 1},
          hello = (
            x
            | std::map(func (y: int) -> y + 1)
            | std::filter(func (y: int) -> !(y > 3))
            | std::sort(func (x: int) -> x)
            | std::map(func (y: int) -> y % 3)
          )
        }
    "#).1, @r#"
    {
      a = 3,
      {
        3,
        4,
      },
      {
        2,
        3,
      },
      hello = [
        2,
        2,
        0,
        0,
        0,
      ],
    }
    "#);
}

#[test]
#[ignore]
fn complex_01() {
    insta::assert_snapshot!(_run(r#"
    module chinook {
      type album = {id = int, title = text}

      let get_albums: func (): [album]

      let get_album_by_id = func (album_id: int): album -> (
        get_albums()
        | std::filter(func (this: album) -> this.id == album_id)
        | std::index(0)
      )
    }

    module box_office {
      type album_sale = {id = int, total = float}

      let get_album_sales: func (): [album_sale]

      let get_album_sales_by_id = func (album_id: int): album_sale -> (
        get_album_sales()
        | std::filter(func (this: album_sale) -> this.id == album_id)
        | std::index(0)
      )
    }
 
    func (album_id: int) -> {
      chinook::get_albums(),
      chinook::get_album_by_id(album_id),
      box_office::get_album_sales_by_id(album_id),
    }
    "#).1, @r#"
    {
      a = 3,
      {
        3,
        4,
      },
      {
        2,
        3,
      },
      hello = [
        2,
        2,
        0,
        0,
        0,
      ],
    }
    "#);
}
