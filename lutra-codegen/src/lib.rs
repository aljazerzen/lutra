mod codegen;

use std::path::PathBuf;

pub use codegen::codegen;

pub fn compile_dir(source_dir: &std::path::Path, out_dir: &std::path::Path) -> Vec<PathBuf> {
    let mut compiled_files = Vec::new();
    for entry in std::fs::read_dir(source_dir).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();

        if path.extension().map_or(false, |x| x == "lt") && path.is_file() {
            compile_file(&path, out_dir);
            compiled_files.push(path);
        }
    }
    compiled_files
}

pub fn compile_file(source_path: &std::path::Path, out_dir: &std::path::Path) {
    use std::fs;
    use std::io::Write;

    let source_stem = source_path.file_stem().unwrap();
    let source = fs::read_to_string(source_path).unwrap();

    let dest_path = out_dir.join(source_stem).with_extension("rs");

    let mut file = fs::File::create(dest_path).unwrap();
    write!(file, "{}", codegen(&source).unwrap()).unwrap();
}
