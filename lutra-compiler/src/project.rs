use std::collections::HashMap;
use std::path;
use std::str::FromStr;

use itertools::Itertools;

use crate::pr;

/// Project, checked.
#[derive(Debug)]
pub struct Project {
    /// Discovered sources
    pub source: SourceTree,

    /// Resolved definitions
    pub root_module: pr::ModuleDef,

    /// Resolution ordering of definitions
    // TODO: make a more efficient "a ordered vec of unordered groups" data structure
    pub ordering: Vec<Vec<pr::Path>>,

    pub dependencies: Vec<Dependency>,
}

#[derive(Debug)]
pub struct Dependency {
    pub name: String,

    pub inner: Project,
}

/// Sources used to resolve the project.
/// All paths are relative to the project root.
// We use `SourceTree` to represent both a single file (including a "file" piped
// from stdin), and a collection of files. (Possibly this could be implemented
// as a Trait with a Struct for each type, which would use structure over values
// (i.e. `Option<PathBuf>` below signifies whether it's a project or not). But
// waiting until it's necessary before splitting it out.)
#[derive(Debug, Clone)]
pub struct SourceTree {
    /// Path to the root of the source tree.
    /// Can be a directory that contains module.lt or a .lt file.
    pub root: path::PathBuf,

    /// Mapping from file paths into into their contents.
    /// Paths are relative to the root.
    pub(crate) sources: HashMap<path::PathBuf, String>,

    /// Index of source ids to paths. Used to keep [crate::Span] lean.
    pub(crate) source_ids: HashMap<u16, path::PathBuf>,
}

impl SourceTree {
    pub fn empty() -> Self {
        SourceTree {
            sources: Default::default(),
            source_ids: Default::default(),
            root: path::PathBuf::new(),
        }
    }

    pub fn single(path: path::PathBuf, content: String) -> Self {
        SourceTree {
            sources: [(path.clone(), content)].into(),
            source_ids: [(1, path)].into(),
            root: path::PathBuf::new(),
        }
    }

    pub fn new<I>(iter: I, root: path::PathBuf) -> Self
    where
        I: IntoIterator<Item = (path::PathBuf, String)>,
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

    pub fn insert(&mut self, path: path::PathBuf, content: String) {
        let last_id = self.source_ids.keys().max().cloned().unwrap_or(0);
        self.sources.insert(path.clone(), content);
        self.source_ids.insert(last_id + 1, path);
    }

    pub fn replace(&mut self, path: &path::Path, content: String) -> Option<String> {
        let source = self.sources.get_mut(path)?;
        Some(std::mem::replace(source, content))
    }

    pub fn get_source_ids(&self) -> impl Iterator<Item = &u16> {
        self.source_ids.keys()
    }
    pub fn get_sources(&self) -> impl Iterator<Item = (&path::PathBuf, &String)> {
        self.sources.iter()
    }

    pub fn get_source_display_paths<'a>(&'a self) -> impl Iterator<Item = &'a path::Path> {
        self.sources
            .keys()
            .map(|path| self.get_display_path(path).unwrap())
    }

    /// Converts a "project path" into an absolute path in the file-system.
    pub fn get_absolute_path(&self, path: impl AsRef<path::Path>) -> path::PathBuf {
        let path = path.as_ref();
        if path.as_os_str().is_empty() {
            self.root.to_path_buf()
        } else {
            self.get_project_dir().join(path)
        }
    }

    /// Converts an absolute path into a path relative to the root.
    /// Not that this is not relative to "project dir".
    pub fn get_relative_path<'a>(
        &self,
        absolute_path: &'a path::Path,
    ) -> Result<&'a path::Path, path::StripPrefixError> {
        absolute_path.strip_prefix(&self.root)
    }

    /// Returns project dir: the directory in which the project files reside.
    /// For example,
    /// - if root is `/some_path/project/`, then that is also the project dir,
    /// - if root is `/some_path/my_file.lt`, then project dir is `/some_path/`.
    pub fn get_project_dir(&self) -> &path::Path {
        if self.root.extension().is_some_and(|e| e == "lt") {
            self.root.parent().unwrap()
        } else {
            &self.root
        }
    }

    /// Converts a path (either absolute or relative to project root) into a
    /// "display path", which is path relative to "project dir".
    /// This is equivalent to "relative path", except for single-file projects,
    /// where root is a file and "project dir" is its parent directory.
    pub fn get_display_path<'a>(&'a self, path: &'a path::Path) -> Option<&'a path::Path> {
        if path.is_absolute() {
            path.strip_prefix(self.get_project_dir()).ok()
        } else if path.as_os_str().is_empty() {
            self.root.file_name().map(|p| path::Path::new(p))
        } else {
            Some(path)
        }
    }

    pub fn get_path(&self, source_id: u16) -> Option<&path::Path> {
        self.source_ids.get(&source_id).map(|x| x.as_path())
    }

    pub fn get_source(&self, path: &path::Path) -> Option<&str> {
        self.sources.get(path).map(|s| s.as_str())
    }
}

impl std::fmt::Display for SourceTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut r = format!("path: {}\nsources:\n", self.root.to_string_lossy());

        for source in self.sources.keys().sorted() {
            r += "- ";
            r += &source.to_string_lossy();
            r += "\n";
        }

        f.write_str(&r)
    }
}

/// Project source (in [SourceTree]) and a code snippet
pub struct SourceOverlay<'a> {
    tree: &'a SourceTree,

    snippet_path: path::PathBuf,
    snippet: &'a str,
}

impl<'a> SourceOverlay<'a> {
    pub fn new(tree: &'a SourceTree, snippet: &'a str, snippet_path: Option<&str>) -> Self {
        Self {
            tree,
            snippet,
            snippet_path: snippet_path
                .map(|s| path::PathBuf::from_str(s).unwrap())
                .unwrap_or_default(),
        }
    }
}

pub(crate) trait SourceProvider {
    fn get_root(&self) -> &path::Path;

    fn get_path(&self, id: u16) -> Option<&path::Path>;

    fn get_source(&self, path: &path::Path) -> Option<&str>;
}

impl SourceProvider for SourceTree {
    fn get_root(&self) -> &path::Path {
        &self.root
    }
    fn get_path(&self, id: u16) -> Option<&path::Path> {
        self.source_ids.get(&id).map(|x| x.as_path())
    }

    fn get_source(&self, path: &path::Path) -> Option<&str> {
        self.sources.get(path).map(|x| x.as_str())
    }
}

impl<'a> SourceProvider for SourceOverlay<'a> {
    fn get_root(&self) -> &path::Path {
        &self.tree.root
    }

    fn get_path(&self, id: u16) -> Option<&path::Path> {
        if id == 0 {
            Some(self.snippet_path.as_path())
        } else {
            self.tree.get_path(id)
        }
    }

    fn get_source(&self, path: &path::Path) -> Option<&str> {
        if path == self.snippet_path {
            Some(self.snippet)
        } else {
            self.tree.get_source(path)
        }
    }
}
