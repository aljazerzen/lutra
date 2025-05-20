use lutra_codegen::GenerateOptions;

fn main() {
    let mut args = std::env::args();

    // executable
    args.next();

    let project_dir = args.next().unwrap();
    let project_dir = std::path::Path::new(&project_dir);

    let output_file = args.next().unwrap();
    let output_file = std::path::Path::new(&output_file);

    if output_file.extension().is_some_and(|x| x == "py") {
        let options = GenerateOptions::default().generate_sr_in_module("");

        lutra_codegen::generate_python(project_dir, output_file, options);
    } else {
        let options = GenerateOptions::default().with_lutra_bin_path("crate".into());

        lutra_codegen::generate(project_dir, output_file, options);
    }
    println!("Done.")
}
