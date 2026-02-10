use lutra_codegen as codegen;
use std::{env, path};

fn main() {
    // generated file should reside in $OUT_DIR (./target/.../out)
    let out_file = path::Path::new(&env::var("OUT_DIR").unwrap()).join("generated.rs");

    // we want to compile all programs in the root module
    let opts =
        codegen::GenerateOptions::default().generate_programs("", codegen::ProgramFormat::SqlPg);

    // run codegen
    let input_files = codegen::generate("src/main.lt", &out_file, opts);

    // print all input files, so cargo knows when to rerun codegen
    for f in input_files {
        println!("cargo::rerun-if-changed={}", f.to_str().unwrap());
    }
}
