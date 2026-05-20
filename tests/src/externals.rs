use lutra_compiler as lc;

#[track_caller]
fn compile_for(source: &str, repr: lc::ProgramRepr) -> Result<(), String> {
    crate::init_logger();

    let source_tree = lc::SourceTree::single("".into(), source.to_string());
    let project = lc::check(source_tree, Default::default()).map_err(|e| format!("{e}"))?;
    lc::compile(&project, &lc::CompileParams::new("main", repr))
        .map(|_| ())
        .map_err(|e| format!("{e}"))
}

#[test]
fn sql_code_compiles_for_sql_pg() {
    let source = r#"
        func main(): [{id: int32, title: text}] -> std::sql::from("movies")
    "#;
    compile_for(source, lc::ProgramRepr::SqlPg).unwrap();
}

#[test]
fn sql_code_compiles_for_sql_duckdb() {
    let source = r#"
        func main(): [{id: int32, title: text}] -> std::sql::from("movies")
    "#;
    compile_for(source, lc::ProgramRepr::SqlDuckdb).unwrap();
}

#[test]
#[ignore] // we still implicitly allow the whole std::
fn sql_code_rejected_for_bytecode() {
    let source = r#"
        func main(): [{id: int32, title: text}] -> std::sql::from("movies")
    "#;
    let err = compile_for(source, lc::ProgramRepr::BytecodeLt).unwrap_err();
    assert!(
        err.contains("std::sql"),
        "expected error about std::sql, got: {err}"
    );
    assert!(
        err.contains("bytecode-lt"),
        "expected error to mention bytecode-lt, got: {err}"
    );
}

#[test]
fn fs_code_compiles_for_bytecode() {
    let source = r#"
        func main(): [{a: int32}] -> std::fs::read_parquet("test.parquet")
    "#;
    compile_for(source, lc::ProgramRepr::BytecodeLt).unwrap();
}

#[test]
#[ignore] // we still implicitly allow the whole std::
fn fs_code_rejected_for_sql_pg() {
    let source = r#"
        func main(): [{a: int32}] -> std::fs::read_parquet("test.parquet")
    "#;
    let err = compile_for(source, lc::ProgramRepr::SqlPg).unwrap_err();
    assert!(
        err.contains("std::fs"),
        "expected error about std::fs, got: {err}"
    );
}

#[test]
fn fs_code_compiles_for_duckdb() {
    let source = r#"
        func main(): [{a: int32}] -> std::fs::read_parquet("test.parquet")
    "#;
    compile_for(source, lc::ProgramRepr::SqlDuckdb).unwrap();
}

#[test]
fn no_externals_compiles_everywhere() {
    let source = r#"
        func main(): int32 -> 42
    "#;
    compile_for(source, lc::ProgramRepr::BytecodeLt).unwrap();
    compile_for(source, lc::ProgramRepr::SqlPg).unwrap();
    compile_for(source, lc::ProgramRepr::SqlDuckdb).unwrap();
}

#[test]
#[ignore] // we still implicitly allow the whole std::
fn transitive_requirement() {
    let source = r#"
        func read_movies(): [{id: int32, title: text}] -> std::sql::from("movies")
        func main(): [{id: int32, title: text}] -> read_movies()
    "#;
    // should succeed for SQL
    compile_for(source, lc::ProgramRepr::SqlPg).unwrap();
    // should fail for bytecode
    let err = compile_for(source, lc::ProgramRepr::BytecodeLt).unwrap_err();
    assert!(
        err.contains("std::sql"),
        "expected std::sql error, got: {err}"
    );
}

#[test]
#[ignore] // we still implicitly allow the whole std::
fn user_requires_annotation() {
    let source = r#"
        @requires(["std::sql"])
        module db {
            func get_data(): int32 -> 42
        }
        func main(): int32 -> db::get_data()
    "#;
    // should fail for bytecode even though the function body doesn't use SQL
    let err = compile_for(source, lc::ProgramRepr::BytecodeLt).unwrap_err();
    assert!(
        err.contains("std::sql"),
        "expected std::sql error, got: {err}"
    );
    // should succeed for SQL
    compile_for(source, lc::ProgramRepr::SqlPg).unwrap();
}
