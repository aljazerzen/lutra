use std::{env, path};

fn main() {
    println!("cargo::rerun-if-changed=build.rs");

    let in_dir = path::Path::new("main.lt");

    let out_dir = env::var_os("OUT_DIR").unwrap();
    let out_file = path::Path::new(&out_dir).join("lutra.rs");

    let opts = lutra_codegen::GenerateOptions::default()
        .no_generate_function_traits()
        .generate_sr_in_module("");

    let input_files = lutra_codegen::generate(in_dir, &out_file, opts);
    for f in input_files {
        println!("cargo::rerun-if-changed={}", f.to_str().unwrap());
    }
}
