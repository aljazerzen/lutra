use std::ffi::OsStr;
use std::fs;
use std::path::PathBuf;

use walkdir::WalkDir;

use crate::project;

#[cfg_attr(feature = "clap", derive(clap::Parser))]
pub struct DiscoverParams {
    /// Path to the project directory
    #[cfg_attr(feature = "clap", arg(long))]
    pub project: Option<PathBuf>,
}

pub fn discover(params: DiscoverParams) -> Result<project::SourceTree, std::io::Error> {
    let mut project = project::SourceTree::empty();

    if let Some(root) = params.project {
        project.root = root;
    } else {
        return Ok(project);
    };

    if project.root.is_file() {
        let file_contents = fs::read_to_string(&project.root)?;
        project.insert(project.root.clone(), file_contents);
        return Ok(project);
    }

    let source_extension = Some(OsStr::new("lt"));
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
