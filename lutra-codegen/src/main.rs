use lutra_codegen::GenerateOptions;

fn main() {
    let mut args = std::env::args();

    // executable
    args.next();

    let project_dir = args.next().unwrap();
    let project_dir = std::path::Path::new(&project_dir);

    let output_file = args.next().unwrap();
    let output_file = std::path::Path::new(&output_file);

    let options = GenerateOptions::default().no_generate_encode_decode();

    lutra_codegen::generate(project_dir, output_file, options);
    println!("Done.")
}
