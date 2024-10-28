use std::{env, fs, io::Write, path::Path};

fn main() {
    println!("cargo::rerun-if-changed=build.rs");

    for entry in std::fs::read_dir("src").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();

        if path.extension().map_or(false, |x| x == "lt") && path.is_file() {
            compile_file(&path);
        }
    }
}

fn compile_file(source_path: &Path) {
    println!("cargo::rerun-if-changed={}", source_path.to_str().unwrap());

    let source_stem = source_path.file_stem().unwrap();

    let source = fs::read_to_string(source_path).unwrap();

    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join(source_stem).with_extension("rs");

    let mut file = fs::File::create(dest_path).unwrap();
    write!(file, "{}", lutra_codegen::codegen(&source).unwrap()).unwrap();
}
