#[track_caller]
fn _test_run(source: &str) -> String {
    // tracing_subscriber::fmt::Subscriber::builder()
    //     .without_time()
    //     .with_max_level(tracing::Level::DEBUG)
    //     .try_init()
    //     .ok();

    let program = lutra_compiler::_test_compile(source).unwrap_or_else(|e| panic!("{e}"));
    eprintln!("--- ir:\n{}\n---", lutra_ir::print(&program));

    lutra_compiler::compile_to_sql(&program)
}

#[test]
fn sql_literals() {
    insta::assert_snapshot!(
        _test_run(r#"
            func () -> 4
        "#),
        @"SELECT 4 AS value"
    );

    insta::assert_snapshot!(
        _test_run(r#"
            func () -> {5, "hello"}
        "#),
        @"SELECT 5 AS f_0, 'hello' AS f_1"
    );

    insta::assert_snapshot!(
        _test_run(r#"
            func () -> [{false, "hello"}, {true, "world"}, {true, "world"}]
        "#),
        @"SELECT 0 AS index, false AS f_0, 'hello' AS f_1 UNION ALL SELECT 1, true, 'world' UNION ALL SELECT 2, true, 'world'"
    );
}

#[test]
fn sql_table() {
    insta::assert_snapshot!(
        _test_run(r#"
            let my_table: func (): [{a = int64, b = text}]

            func () -> my_table()
        "#),
        @"SELECT NULL AS index, a.a AS f_0, a.b AS f_1 FROM my_table AS a"
    );
}

#[test]
fn sql_slice() {
    insta::assert_snapshot!(
        _test_run(r#"
            let my_table: func (): [{a = int64, b = text}]

            func () -> std::slice(my_table(), 3, 6)
        "#),
        @"SELECT NULL AS index, a.a AS f_0, a.b AS f_1 FROM my_table AS a LIMIT (6 - 3) OFFSET 3"
    );
    insta::assert_snapshot!(
        _test_run(r#"
            let my_table: func (): [{a = int64, b = text}]

            func () -> std::slice(my_table(), 2 * 5, 23 - 4 + 1)
        "#),
        @"SELECT NULL AS index, a.a AS f_0, a.b AS f_1 FROM my_table AS a LIMIT (((23 - 4) + 1) - (2 * 5)) OFFSET (2 * 5)"
    );
}
