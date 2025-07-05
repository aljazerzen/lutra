use std::ffi::OsStr;
use std::fs;
use std::path::PathBuf;

use walkdir::WalkDir;

use crate::project;

#[cfg_attr(feature = "clap", derive(clap::Parser))]
pub struct DiscoverParams {
    /// Path to the project directory
    #[cfg_attr(feature = "clap", arg(default_value = "."))]
    pub project_path: PathBuf,
}

pub fn discover(params: DiscoverParams) -> Result<project::SourceTree, std::io::Error> {
    let source_extension = Some(OsStr::new("lt"));

    let mut project = project::SourceTree {
        root: params.project_path,
        ..Default::default()
    };

    if project.root.is_file() {
        let file_contents = fs::read_to_string(&project.root)?;
        project.insert(project.root.clone(), file_contents);
        return Ok(project);
    }

    for entry in WalkDir::new(&project.root) {
        let entry = entry?;
        let path = entry.path();
        let relative_path = path.strip_prefix(&project.root).unwrap().to_path_buf();

        if path.is_file() {
            match path.extension() {
                e if e == source_extension => {
                    let file_contents = fs::read_to_string(path)?;

                    project.insert(relative_path, file_contents);
                }

                // ignore
                _ => {}
            }
        }
    }

    Ok(project)
}
