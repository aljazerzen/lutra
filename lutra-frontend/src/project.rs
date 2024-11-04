use std::collections::HashMap;
use std::path::PathBuf;

use crate::ir::decl::RootModule;

/// Project, resolved.
#[derive(Debug)]
pub struct Project {
    /// Discovered sources
    pub source: SourceTree,

    /// Resolved declarations
    pub root_module: RootModule,
}

/// All paths are relative to the project root.
// We use `SourceTree` to represent both a single file (including a "file" piped
// from stdin), and a collection of files. (Possibly this could be implemented
// as a Trait with a Struct for each type, which would use structure over values
// (i.e. `Option<PathBuf>` below signifies whether it's a project or not). But
// waiting until it's necessary before splitting it out.)
#[derive(Debug, Clone, Default)]
pub struct SourceTree {
    /// Path to the root of the source tree.
    pub root: PathBuf,

    /// Mapping from file paths into into their contents.
    /// Paths are relative to the root.
    pub sources: HashMap<PathBuf, String>,

    /// Index of source ids to paths. Used to keep [error::Span] lean.
    pub(crate) source_ids: HashMap<u16, PathBuf>,
}

impl SourceTree {
    pub fn single(path: PathBuf, content: String) -> Self {
        SourceTree {
            sources: [(path.clone(), content)].into(),
            source_ids: [(1, path)].into(),
            root: PathBuf::new(),
        }
    }

    pub fn new<I>(iter: I, root: PathBuf) -> Self
    where
        I: IntoIterator<Item = (PathBuf, String)>,
    {
        let mut res = SourceTree {
            sources: HashMap::new(),
            source_ids: HashMap::new(),
            root,
        };

        for (index, (path, content)) in iter.into_iter().enumerate() {
            res.sources.insert(path.clone(), content);
            res.source_ids.insert((index + 1) as u16, path);
        }
        res
    }

    pub fn insert(&mut self, path: PathBuf, content: String) {
        let last_id = self.source_ids.keys().max().cloned().unwrap_or(0);
        self.sources.insert(path.clone(), content);
        self.source_ids.insert(last_id + 1, path);
    }

    pub fn get_path(&self, source_id: u16) -> Option<&PathBuf> {
        self.source_ids.get(&source_id)
    }
}

impl std::fmt::Display for SourceTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut r = format!("path: {}\nsources:\n", self.root.to_string_lossy());

        for source in self.sources.keys() {
            r += "- ";
            r += &source.to_string_lossy();
            r += "\n";
        }

        f.write_str(&r)
    }
}
