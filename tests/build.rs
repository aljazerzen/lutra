fn main() {
    println!("cargo::rerun-if-changed=build.rs");

    lutra_codegen::check_and_generate(
        "lutra",
        lutra_codegen::GenerateOptions::default().generate_function_traits(),
    );
}
