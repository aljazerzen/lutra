mod codegen;

use std::fs;
use std::io::Write;
use std::path::PathBuf;

use lutra_frontend::{CompileParams, DiscoverParams};

pub fn generate_types(project_dir: &std::path::Path, out_file: &std::path::Path) -> Vec<PathBuf> {
    let source = lutra_frontend::discover(DiscoverParams {
        project_path: project_dir.into(),
    })
    .unwrap();

    let project = lutra_frontend::compile(source, CompileParams {}).unwrap();

    // write types
    let mut file = fs::File::create(out_file).unwrap();
    let generated = codegen::codegen_types(&project.root_module.module).unwrap();
    write!(file, "{generated}").unwrap();

    // return vec of input files
    project.source.get_sources().map(|s| s.0.clone()).collect()
}
