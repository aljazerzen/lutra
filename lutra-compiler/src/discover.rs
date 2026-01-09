use std::collections::HashMap;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::{fs, io};

use crate::error::Error;
use crate::project;

#[cfg_attr(feature = "clap", derive(clap::Parser))]
#[derive(Clone)]
pub struct DiscoverParams {
    /// Path to a project file
    #[cfg_attr(feature = "clap", arg(long))]
    pub project: Option<PathBuf>,
}

pub fn discover(params: DiscoverParams) -> Result<project::SourceTree, Error> {
    let Some(a_project_file) = params.project else {
        return Ok(project::SourceTree::empty());
    };

    // walk up the module tree
    tracing::debug!("searching for project root");
    let mut root_file = a_project_file.canonicalize()?;
    if root_file.is_dir() {
        root_file.push("module.lt");
    }
    let mut loaded_files = HashMap::new();
    loop {
        let file_contents =
            fs::read_to_string(&root_file).map_err(|io| Error::CannotReadSourceFile {
                file: root_file.clone(),
                io,
            })?;

        let is_submodule = crate::parser::is_submodule(&file_contents).unwrap_or(false);

        loaded_files.insert(root_file.clone(), file_contents);
        if is_submodule {
            root_file = parent_module(&root_file).ok_or(Error::CannotFindProjectRoot)?;
        } else {
            break;
        }
    }

    let root = if root_file.ends_with("module.lt") {
        root_file.parent().unwrap().to_path_buf()
    } else {
        root_file.clone()
    };
    tracing::debug!("project root: {}", root.display());
    let mut project = project::SourceTree::new([], root.clone());

    // walk down the module tree
    tracing::debug!("loading project files");
    let target_extension = Some(OsStr::new("lt"));
    let mut paths_to_load = vec![root_file];
    while let Some(path) = paths_to_load.pop() {
        tracing::debug!("  path: {}", path.display());
        let content = if let Some(c) = loaded_files.remove(&path) {
            // use file that was read before
            c
        } else {
            // read the file
            let content = match fs::read_to_string(&path) {
                Ok(c) => c,
                Err(e) if path.ends_with("module.lt") && e.kind() == io::ErrorKind::NotFound => {
                    // subdir/module.lt is allowed not to exist
                    continue;
                }
                Err(e) => {
                    return Err(Error::CannotReadSourceFile {
                        file: path.to_path_buf(),
                        io: e,
                    });
                }
            };

            // but include it only if it is a submodule
            let is_submodule = crate::parser::is_submodule(&content).unwrap_or(true);
            if !is_submodule {
                continue;
            }
            content
        };

        let relative_path = project.get_relative_path(&path).unwrap().to_path_buf();
        project.insert(relative_path, content);

        // for module files, read the whole dir
        if path.ends_with("module.lt") {
            let Some(dir_path) = path.parent() else {
                continue;
            };
            tracing::debug!("  reading dir: {}", dir_path.display());
            let mut new_paths = Vec::new();
            for entry in fs::read_dir(dir_path)? {
                let entry = entry?;
                let entry_path = entry.path();
                let metadata = entry.metadata()?;

                if metadata.is_dir() {
                    new_paths.push(entry.path().join("module.lt"));
                } else if metadata.is_file()
                    && entry.file_name() != "module.lt" // we've already read that
                    && entry_path.extension() == target_extension
                {
                    new_paths.push(entry.path());
                }
            }
            new_paths.sort();
            paths_to_load.extend(new_paths);
        }
    }

    Ok(project)
}

fn parent_module(path: &Path) -> Option<PathBuf> {
    Some(path.parent()?.join("module.lt"))
}
