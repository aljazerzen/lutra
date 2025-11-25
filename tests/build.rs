use std::{env, path::Path};

fn main() {
    println!("cargo::rerun-if-changed=build.rs");

    let project_dir = Path::new("lutra");
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let out_file = Path::new(&out_dir).join("lutra.rs");

    let opts = lutra_codegen::GenerateOptions::default().generate_function_traits();

    let input_files = lutra_codegen::generate(project_dir, &out_file, opts);
    for f in input_files {
        println!("cargo::rerun-if-changed={}", f.to_str().unwrap());
    }
}
