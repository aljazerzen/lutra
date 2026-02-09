fn main() {
    println!("cargo::rerun-if-changed=build.rs");
    lutra_codegen::check_and_generate(
        "src/introspection.lt",
        lutra_codegen::GenerateOptions::default()
            .generate_programs("", lutra_codegen::ProgramFormat::SqlDuckdb),
    );
}
