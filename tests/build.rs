fn main() {
    lutra_codegen::check_and_generate(
        "lutra",
        lutra_codegen::GenerateOptions::default()
            .generate_function_traits()
            .generate_programs("runtime", lutra_codegen::ProgramRepr::BytecodeLt)
            .generate_client(),
    );
}
