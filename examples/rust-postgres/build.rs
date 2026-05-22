use lutra_codegen as codegen;

fn main() {
    codegen::check_and_generate(
        "src/main.lt",
        codegen::GenerateOptions::default()
            .generate_programs("", codegen::ProgramRepr::SqlPg)
            .generate_client(),
    );
}
