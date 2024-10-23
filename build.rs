use std::{env, fs::File, io::Write, path::Path};

fn main() {
    let source = r#"
        type x = {int, a = text, b = [bool]} 
    "#;

    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("hello.rs");

    let mut file = File::create(dest_path).unwrap();
    write!(file, "{}", lutra_codegen::codegen(source).unwrap()).unwrap();

    println!("cargo::rerun-if-changed=build.rs");
}
