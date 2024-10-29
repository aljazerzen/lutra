use std::{env, path::Path};

fn main() {
    println!("cargo::rerun-if-changed=build.rs");

    let in_dir = Path::new("src");
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let out_dir = Path::new(&out_dir);

    let compiled_files = lutra_codegen::compile_dir(&in_dir, &out_dir);

    for f in compiled_files {
        println!("cargo::rerun-if-changed={}", f.to_str().unwrap());
    }
}
