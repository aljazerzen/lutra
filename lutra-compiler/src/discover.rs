use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};

use crate::error::Error;
use crate::project;

#[cfg_attr(feature = "clap", derive(clap::Parser))]
#[derive(Clone)]
pub struct DiscoverParams {
    /// Path to the project directory
    #[cfg_attr(feature = "clap", arg(long))]
    pub project: Option<PathBuf>,
}

pub fn discover(params: DiscoverParams) -> Result<project::SourceTree, Error> {
    let mut project = project::SourceTree::empty();
    let Some(mut root) = params.project else {
        return Ok(project);
    };
    root = root.canonicalize()?;

    // walk up the module tree
    tracing::debug!("searching for project root");
    let mut loaded_files = HashMap::new();
    if root.is_dir() {
        root.push("module.lt");
    }
    loop {
        let file_contents =
            fs::read_to_string(&root).map_err(|io| Error::CannotReadSourceFile {
                file: root.clone(),
                io,
            })?;

        let is_submodule = crate::parser::is_submodule(&file_contents).unwrap_or(false);

        loaded_files.insert(root.clone(), file_contents);
        if is_submodule {
            root = parent_module(&root).ok_or(Error::CannotFindProjectRoot)?;
        } else {
            break;
        }
    }
    if root.ends_with("module.lt") {
        project.root = root.parent().unwrap().to_path_buf();
    } else {
        project.root = root.clone();
    }

    tracing::debug!("project root: {}", root.display());

    // walk down the module tree
    tracing::debug!("loading project files");
    let target_extension = Some(OsStr::new("lt"));
    let mut paths_to_load = vec![root];
    while let Some(path) = paths_to_load.pop() {
        tracing::debug!("  path: {}", path.display());
        let content = if let Some(c) = loaded_files.remove(&path) {
            // use file that was read before
            c
        } else {
            // read the file
            let content = fs::read_to_string(&path)?;

            // but include it only if it is a submodule
            let is_submodule = crate::parser::is_submodule(&content).unwrap_or(true);
            if !is_submodule {
                continue;
            }
            content
        };

        let relative_path = path.strip_prefix(&project.root).unwrap().to_path_buf();
        project.insert(relative_path, content);

        // for module files, read the whole dir
        if path.ends_with("module.lt") {
            let Some(dir_path) = path.parent() else {
                continue;
            };
            tracing::debug!("  reading dir: {}", dir_path.display());
            for entry in fs::read_dir(dir_path)? {
                let entry = entry?;
                let entry_path = entry.path();
                let metadata = entry.metadata()?;

                if metadata.is_dir() {
                    paths_to_load.push(entry.path().join("module.lt"));
                } else if metadata.is_file()
                    && entry.file_name() != "module.lt" // we've already read that
                    && entry_path.extension() == target_extension
                {
                    paths_to_load.push(entry.path());
                }
            }
        }
    }

    Ok(project)
}

fn parent_module(path: &Path) -> Option<PathBuf> {
    Some(path.parent()?.join("module.lt"))
}
