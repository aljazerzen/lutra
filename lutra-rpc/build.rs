use std::{env, path::Path};

fn main() {
    println!("cargo::rerun-if-changed=build.rs");

    let project_dir = Path::new("src");
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let out_file = Path::new(&out_dir).join("messages.rs");

    let input_files = lutra_codegen::generate(project_dir, &out_file, Default::default());
    for f in input_files {
        println!("cargo::rerun-if-changed={}", f.to_str().unwrap());
    }
}
