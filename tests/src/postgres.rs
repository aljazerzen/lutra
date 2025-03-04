#[track_caller]
pub fn _run(lutra_source: &str) -> (String, String) {
    tracing_subscriber::fmt::Subscriber::builder()
        .without_time()
        .with_max_level(tracing::Level::DEBUG)
        .try_init()
        .ok();

    // compile
    let program = lutra_compiler::_test_compile(lutra_source).unwrap_or_else(|e| panic!("{e}"));
    tracing::debug!("ir:\n{}", lutra_ir::print(&program));
    let output_ty = program.get_output_ty().clone();

    // compile to sql
    let query_sql = lutra_compiler::compile_to_sql(&program);

    // format sql
    let params = sqlformat::QueryParams::None;
    let options = sqlformat::FormatOptions::default();
    let formatted_sql = sqlformat::format(&query_sql, &params, &options);
    tracing::debug!("sql:\n{formatted_sql}");

    // execute
    async fn inner(
        query_sql: &str,
        output_ty: lutra_bin::ir::Ty,
    ) -> Result<String, tokio_postgres::Error> {
        const POSTGRES_URL: &str = "postgresql://postgres:pass@localhost:5416";

        let (client, connection) = tokio_postgres::connect(POSTGRES_URL, tokio_postgres::NoTls)
            .await
            .unwrap();

        tokio::spawn(async move {
            if let Err(e) = connection.await {
                eprintln!("connection error: {}", e);
            }
        });

        let (rel_ty, rel_data) = lutra_db_driver::query(client, query_sql).await?;
        let output = lutra_db_driver::repack(&rel_ty, rel_data, &output_ty);
        let output = output.flatten();

        let output = lutra_bin::Value::decode(&output, &output_ty).unwrap();
        Ok(output.print_source(&output_ty).unwrap())
    }
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    let output = rt.block_on(inner(&query_sql, output_ty)).unwrap();

    (formatted_sql, output)
}

#[track_caller]
pub fn _run_to_str(lutra_source: &str) -> String {
    let (sql, output) = _run(lutra_source);
    format!("{sql}\n{output}")
}

#[test]
fn array_of_tuples() {
    insta::assert_snapshot!(_run_to_str(r#"
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
    insta::assert_snapshot!(_run_to_str(r#"
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
    insta::assert_snapshot!(_run_to_str(r#"
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
    insta::assert_snapshot!(_run_to_str(r#"
        func () -> 3
    "#), @r#"
    SELECT
      3::int8 AS value
    3
    "#);
}
