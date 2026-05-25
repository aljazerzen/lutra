use lutra_compiler as lc;
use lutra_compiler::ProgramRepr as Repr;

#[track_caller]
fn _compile(source: &str, repr: lc::ProgramRepr) -> Result<(), String> {
    _compile_with(source, repr, vec![])
}

#[track_caller]
fn _compile_with(
    source: &str,
    repr: lc::ProgramRepr,
    externals: Vec<&'static str>,
) -> Result<(), String> {
    crate::init_logger();

    let source_tree = lc::SourceTree::single("".into(), source.to_string());
    let project = lc::check(source_tree, Default::default()).map_err(|e| format!("{e}"))?;
    let params = lc::CompileParams::new("main", repr).with_externals(externals);
    lc::compile(&project, &params)
        .map(|_| ())
        .map_err(|e| format!("{e}"))
}

#[test]
fn sql_code_compiles_for_sql_pg() {
    let source = r#"
        func main(): [{id: Int32, title: Text}] -> std::sql::from("movies")
    "#;
    _compile(source, Repr::SqlPg).unwrap();
}

#[test]
fn sql_code_compiles_for_sql_duckdb() {
    let source = r#"
        func main(): [{id: Int32, title: Text}] -> std::sql::from("movies")
    "#;
    _compile(source, Repr::SqlDuckdb).unwrap();
}

#[test]
fn sql_code_rejected_for_bytecode() {
    let source = r#"
        func main(): [{id: Int32, title: Text}] -> std::sql::from("movies")
    "#;
    let err = _compile(source, Repr::BytecodeLt).unwrap_err();
    assert!(
        err.contains("std::sql"),
        "expected error about std::sql, got: {err}"
    );
}

#[test]
fn fs_code_compiles_for_bytecode() {
    let source = r#"
        func main(): [{a: Int32}] -> std::fs::read_parquet("test.parquet")
    "#;
    _compile_with(source, Repr::BytecodeLt, vec!["std::fs"]).unwrap();
}

#[test]
fn fs_code_rejected_for_sql_pg() {
    let source = r#"
        func main(): [{a: Int32}] -> std::fs::read_parquet("test.parquet")
    "#;
    let err = _compile(source, Repr::SqlPg).unwrap_err();
    assert!(
        err.contains("std::fs"),
        "expected error about std::fs, got: {err}"
    );
}

#[test]
fn fs_code_compiles_for_duckdb() {
    let source = r#"
        func main(): [{a: Int32}] -> std::fs::read_parquet("test.parquet")
    "#;
    _compile(source, Repr::SqlDuckdb).unwrap();
}

#[test]
fn no_externals_compiles_everywhere() {
    let source = r#"
        func main(): Int32 -> 42
    "#;
    _compile(source, Repr::BytecodeLt).unwrap();
    _compile(source, Repr::SqlPg).unwrap();
    _compile(source, Repr::SqlDuckdb).unwrap();
}

#[test]
fn externals_compiles_no_where() {
    let source = r#"
        func main(): Int32
    "#;
    _compile(source, Repr::BytecodeLt).unwrap_err();
    _compile(source, Repr::SqlPg).unwrap_err();
    _compile(source, Repr::SqlDuckdb).unwrap_err();
}

#[test]
fn transitive_requirement() {
    let source = r#"
        func read_movies(): [{id: Int32, title: Text}] -> std::sql::from("movies")
        func main(): [{id: Int32, title: Text}] -> read_movies()
    "#;
    // should succeed for SQL
    _compile(source, Repr::SqlPg).unwrap();
    // should fail for bytecode
    let err = _compile(source, Repr::BytecodeLt).unwrap_err();
    assert!(
        err.contains("std::sql"),
        "expected std::sql error, got: {err}"
    );
}
