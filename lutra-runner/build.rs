fn main() {
    lutra_codegen::check_and_generate(
        "src/binary/messages.lt",
        lutra_codegen::GenerateOptions::default()
            .function_traits()
            .no_std(),
    );
}
