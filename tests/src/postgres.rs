use lutra_compiler::{ProgramFormat, SourceTree};

const POSTGRES_URL: &str = "postgresql://postgres:pass@localhost:5416";

#[track_caller]
pub fn _run(source: &str, args: Vec<lutra_bin::Value>) -> (String, String) {
    crate::init_logger();

    // compile
    let source = SourceTree::single("".into(), source.to_string());
    let project = match lutra_compiler::check(source, Default::default()) {
        Ok(p) => p,
        Err(e) => panic!("{e}"),
    };

    // compile to sql
    let (program, ty) =
        lutra_compiler::compile(&project, "main", None, ProgramFormat::SqlPg).unwrap();
    let program = program.as_sql_pg().unwrap();

    // format sql
    let options = sqlformat::FormatOptions::default();
    let formatted_sql = sqlformat::format(&program.sql, &sqlformat::QueryParams::None, &options);
    tracing::debug!("sql:\n{formatted_sql}");

    let mut input_writer = lutra_bin::TupleWriter::new_for_ty(&ty.input);
    for (arg, a_ty) in std::iter::zip(args, ty.input.iter_fields()) {
        input_writer.write_field(lutra_bin::Data::new(arg.encode(a_ty, &ty.ty_defs).unwrap()));
    }
    let input = input_writer.finish().flatten();

    // execute
    let mut client = postgres::Client::connect(POSTGRES_URL, postgres::NoTls).unwrap();
    let rel_data = lutra_runner_postgres::execute(&mut client, program, &input).unwrap();

    // decode and print source
    let output = lutra_bin::Value::decode(&rel_data, &ty.output, &ty.ty_defs).unwrap();
    let output = output.print_source(&ty.output, &ty.ty_defs).unwrap();

    (formatted_sql, output)
}

#[track_caller]
pub fn _run_sql_output(lutra_source: &str) -> String {
    let (sql, output) = _run(lutra_source, vec![]);
    format!("{sql}\n---\n{output}")
}

#[test]
fn prim() {
    insta::assert_snapshot!(_run_sql_output(r#"
        3: int16
    "#), @r"
    SELECT
      r0.value
    FROM
      (
        SELECT
          3::int2 AS value
      ) AS r0
    ---
    3
    ");
}

#[test]
fn tuple_prim() {
    insta::assert_snapshot!(_run_sql_output(r#"
        {3: int16, false}
    "#), @r"
    SELECT
      r0._0,
      r0._1
    FROM
      (
        SELECT
          3::int2 AS _0,
          FALSE AS _1
      ) AS r0
    ---
    {
      3,
      false,
    }
    ");
}

#[test]
fn array_prim() {
    insta::assert_snapshot!(_run_sql_output(r#"
        [3, 6, 12]: [int16]
    "#), @r"
    SELECT
      r3.value
    FROM
      (
        SELECT
          0::int8 AS index,
          3::int2 AS value
        UNION
        ALL
        SELECT
          1::int8 AS index,
          6::int2 AS value
        UNION
        ALL
        SELECT
          2::int8 AS index,
          12::int2 AS value
      ) AS r3
    ORDER BY
      r3.index
    ---
    [
      3,
      6,
      12,
    ]
    ");
}

#[test]
fn array_empty() {
    insta::assert_snapshot!(_run_sql_output(r#"
        []: [bool]
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
      r0.index
    ---
    []
    "#);
}

#[test]
fn tuple_tuple_prim() {
    insta::assert_snapshot!(_run_sql_output(r#"
        {3: int16, {false, true, {"hello"}, 4: int32}}
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
          3::int2 AS _0,
          FALSE AS _1_0,
          TRUE AS _1_1,
          'hello' AS _1_2_0,
          4::int4 AS _1_3
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
    insta::assert_snapshot!(_run_sql_output(r#"
        {true, [1, 2, 3]: [int64], [4]: [int32], false}
    "#), @r"
    SELECT
      r6._0,
      r6._1,
      r6._2,
      r6._3
    FROM
      (
        SELECT
          TRUE AS _0,
          (
            SELECT
              COALESCE(
                jsonb_agg(
                  r3.value
                  ORDER BY
                    r3.index
                ),
                '[]'::jsonb
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
              ) AS r3
          ) AS _1,
          (
            SELECT
              COALESCE(
                jsonb_agg(
                  r5.value
                  ORDER BY
                    r5.index
                ),
                '[]'::jsonb
              ) AS value
            FROM
              (
                SELECT
                  0::int8 AS index,
                  4::int4 AS value
              ) AS r5
          ) AS _2,
          FALSE AS _3
      ) AS r6
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
    ");
}

#[test]
fn tuple_array_empty() {
    insta::assert_snapshot!(_run_sql_output(r#"
        {true, []: [int64], false}
    "#), @r"
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
              ) AS value
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
    ");
}

#[test]
fn array_array_prim() {
    insta::assert_snapshot!(_run_sql_output(r#"
        [[1, 2, 3], [4, 5]]: [[int64]]
    "#), @r"
    SELECT
      r9.value
    FROM
      (
        SELECT
          0::int8 AS index,
          (
            SELECT
              COALESCE(
                jsonb_agg(
                  r3.value
                  ORDER BY
                    r3.index
                ),
                '[]'::jsonb
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
              ) AS r3
          ) AS value
        UNION
        ALL
        SELECT
          1::int8 AS index,
          (
            SELECT
              COALESCE(
                jsonb_agg(
                  r7.value
                  ORDER BY
                    r7.index
                ),
                '[]'::jsonb
              ) AS value
            FROM
              (
                SELECT
                  0::int8 AS index,
                  4::int8 AS value
                UNION
                ALL
                SELECT
                  1::int8 AS index,
                  5::int8 AS value
              ) AS r7
          ) AS value
      ) AS r9
    ORDER BY
      r9.index
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
    ");
}

#[test]
fn array_tuple_prim() {
    insta::assert_snapshot!(_run_sql_output(r#"
        [{3: int64, false}, {6, true}, {12, false}]
    "#), @r#"
    SELECT
      r3._0,
      r3._1
    FROM
      (
        SELECT
          0::int8 AS index,
          3::int8 AS _0,
          FALSE AS _1
        UNION
        ALL
        SELECT
          1::int8 AS index,
          6::int8 AS _0,
          TRUE AS _1
        UNION
        ALL
        SELECT
          2::int8 AS index,
          12::int8 AS _0,
          FALSE AS _1
      ) AS r3
    ORDER BY
      r3.index
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
    insta::assert_snapshot!(_run_sql_output(r#"
        {
            "hello",
            [{3: int16, false}, {6, true}, {12, false}],
        }
    "#), @r#"
    SELECT
      r4._0,
      r4._1
    FROM
      (
        SELECT
          'hello' AS _0,
          (
            SELECT
              COALESCE(
                jsonb_agg(
                  jsonb_build_array(r3._0, r3._1)
                  ORDER BY
                    r3.index
                ),
                '[]'::jsonb
              ) AS value
            FROM
              (
                SELECT
                  0::int8 AS index,
                  3::int2 AS _0,
                  FALSE AS _1
                UNION
                ALL
                SELECT
                  1::int8 AS index,
                  6::int2 AS _0,
                  TRUE AS _1
                UNION
                ALL
                SELECT
                  2::int8 AS index,
                  12::int2 AS _0,
                  FALSE AS _1
              ) AS r3
          ) AS _1
      ) AS r4
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
      4: int16,
      ([{id = 3: int32, title = "Hello world!"}] | std::index(0)),
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
    let get_data = func () -> {a = [2, 5, 4, 3, 1, 2]: [int32]}

    func () -> (
      get_data().a
      | std::map(func (y: int32) -> -y)
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
    let get_data = func () -> {a = [{2: int32, false}, {5, true}, {4, false}]}

    func () -> (
      get_data().a
      | std::map(func (y: {int32, bool}) -> {-y.0, !y.1})
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
    let get_data = func () -> [[1: int16, 2, 3], [4, 5, 6]]

    func () -> (
      get_data() | std::map(func (y: [int16]) -> (
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
    let get_data = func () -> [[1: int64, 2, 3], [4, 5, 6]]

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

#[test]
fn match_04() {
    insta::assert_snapshot!(_run_sql_output(r#"
    type Animal: enum {
      Cat: text,
      Dog: enum {Generic, Collie: text},
    }

    func () -> (
      [
        Animal::Cat("Whiskers"),
        Animal::Dog(Animal::Dog::Collie("Belie")),
        Animal::Dog(Animal::Dog::Generic),
      ]
      | std::map(func (animal: Animal) -> match animal {
        .Cat(name) => f"Hello {name}",
        .Dog(.Generic) => "Who's a good boy?",
        .Dog(.Collie(name)) => f"Come here {name}",
      })
    )
    "#), @r#"
    SELECT
      r23.value
    FROM
      (
        SELECT
          r3.index AS index,
          (
            SELECT
              r21.value
            FROM
              (
                WITH r5 AS (
                  SELECT
                    r6._t,
                    r6._0,
                    r6._1_t,
                    r6._1_1
                  FROM
                    (
                      SELECT
                        r3._t AS _t,
                        r3._0 AS _0,
                        r3._1_t AS _1_t,
                        r3._1_1 AS _1_1
                    ) AS r6
                )
                SELECT
                  CASE
                    WHEN (
                      SELECT
                        (r8.value = 0::"char") AS value
                      FROM
                        (
                          SELECT
                            r7._t AS value
                          FROM
                            r5 AS r7
                        ) AS r8
                    ) THEN (
                      SELECT
                        ('Hello ' || r10.value) AS value
                      FROM
                        (
                          SELECT
                            r9._0 AS value
                          FROM
                            r5 AS r9
                        ) AS r10
                    )
                    WHEN (
                      SELECT
                        (
                          (r12.value = 1::"char")
                          AND (r14.value = 0::"char")
                        ) AS value
                      FROM
                        (
                          SELECT
                            r11._t AS value
                          FROM
                            r5 AS r11
                        ) AS r12,
                        (
                          SELECT
                            r13._1_t AS value
                          FROM
                            r5 AS r13
                        ) AS r14
                    ) THEN 'Who''s a good boy?'
                    WHEN (
                      SELECT
                        (
                          (r16.value = 1::"char")
                          AND (r18.value = 1::"char")
                        ) AS value
                      FROM
                        (
                          SELECT
                            r15._t AS value
                          FROM
                            r5 AS r15
                        ) AS r16,
                        (
                          SELECT
                            r17._1_t AS value
                          FROM
                            r5 AS r17
                        ) AS r18
                    ) THEN (
                      SELECT
                        ('Come here ' || r20.value) AS value
                      FROM
                        (
                          SELECT
                            r19._1_1 AS value
                          FROM
                            r5 AS r19
                        ) AS r20
                    )
                  END AS value
              ) AS r21
          ) AS value
        FROM
          (
            SELECT
              0::int8 AS index,
              0::"char" AS _t,
              'Whiskers' AS _0,
              NULL::"char" AS _1_t,
              NULL::text AS _1_1
            UNION
            ALL
            SELECT
              1::int8 AS index,
              1::"char" AS _t,
              NULL::text AS _0,
              1::"char" AS _1_t,
              'Belie' AS _1_1
            UNION
            ALL
            SELECT
              2::int8 AS index,
              1::"char" AS _t,
              NULL::text AS _0,
              0::"char" AS _1_t,
              NULL::text AS _1_1
          ) AS r3
      ) AS r23
    ORDER BY
      r23.index
    ---
    [
      "Hello Whiskers",
      "Come here Belie",
      "Who's a good boy?",
    ]
    "#);
}

#[test]
fn sql_from_00() {
    let mut client = postgres::Client::connect(POSTGRES_URL, postgres::NoTls).unwrap();
    client
        .batch_execute(
            r#"
        drop table if exists movies;
        create table movies (id int4 primary key, title text, release_year int2);
        insert into movies values (1, 'Forrest Gump', 1994), (2, 'Prestige', 2009);
            "#,
        )
        .unwrap();

    insta::assert_snapshot!(_run_sql_output(r#"
    type Movie: {
      id: int32,
      title: text,
      release_year: int16
    }

    func (): [Movie] -> std::sql::from("movies")
    "#), @r#"
    SELECT
      r0._0,
      r0._1,
      r0._2
    FROM
      (
        SELECT
          NULL AS index,
          id AS _0,
          title AS _1,
          release_year AS _2
        FROM
          movies
      ) AS r0
    ORDER BY
      r0.index
    ---
    [
      {
        id = 1,
        title = "Forrest Gump",
        release_year = 1994,
      },
      {
        id = 2,
        title = "Prestige",
        release_year = 2009,
      },
    ]
    "#);
}

#[test]
fn group_00() {
    insta::assert_snapshot!(_run_sql_output(r#"
    let values: [int64] = [1, 1, 1, 3, 2, 3]

    func () -> (
      values
      | std::group(func (x) -> x)
      | std::map(func (this) -> {
        value = this.key,
        sum = std::sum(this.values),
      })
    )
    "#), @r"
    SELECT
      r16._0,
      r16._1
    FROM
      (
        SELECT
          r7.index AS index,
          r7._0 AS _0,
          (
            SELECT
              r13.value
            FROM
              (
                SELECT
                  COALESCE(SUM(r12.value), 0)::int8 AS value
                FROM
                  LATERAL (
                    SELECT
                      (ROW_NUMBER() OVER ())::int4 AS index,
                      j.value::text::int8 AS value
                    FROM
                      jsonb_array_elements(r7._1) AS j
                  ) AS r12
              ) AS r13
          ) AS _1
        FROM
          (
            SELECT
              (ROW_NUMBER() OVER ())::int4 AS index,
              r6.value AS _0,
              COALESCE(
                jsonb_agg(
                  r6.value
                  ORDER BY
                    r6.index
                ),
                '[]'::jsonb
              ) AS _1
            FROM
              (
                SELECT
                  0::int8 AS index,
                  1::int8 AS value
                UNION
                ALL
                SELECT
                  1::int8 AS index,
                  1::int8 AS value
                UNION
                ALL
                SELECT
                  2::int8 AS index,
                  1::int8 AS value
                UNION
                ALL
                SELECT
                  3::int8 AS index,
                  3::int8 AS value
                UNION
                ALL
                SELECT
                  4::int8 AS index,
                  2::int8 AS value
                UNION
                ALL
                SELECT
                  5::int8 AS index,
                  3::int8 AS value
              ) AS r6
            GROUP BY
              r6.value
          ) AS r7
      ) AS r16
    ORDER BY
      r16.index
    ---
    [
      {
        value = 1,
        sum = 3,
      },
      {
        value = 2,
        sum = 2,
      },
      {
        value = 3,
        sum = 6,
      },
    ]
    ");
}
