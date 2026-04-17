mod analysis;

pub use analysis::{SymbolInfo, TargetMap, TargetSpan};

use std::path;
use std::str::FromStr;
use std::sync::Arc;

use indexmap::IndexMap;
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

    /// Index of all resolved identifier references, keyed by source location.
    /// Built once after name resolution; used for go-to-definition and similar queries.
    pub target_map: analysis::TargetMap,
}

#[derive(Debug, Clone)]
pub struct Dependency {
    pub name: String,

    pub inner: Arc<Project>,
}

impl Project {
    /// Search the resolved module tree recursively for definitions that carry
    /// the annotation `@<annotation_name>` (bare identifier).
    ///
    /// Returns a vec of `(path, def)` pairs where `path` is the fully-qualified
    /// path of the definition within the module tree.
    pub fn find_by_annotation(&self, name: &str) -> Vec<pr::Path> {
        let mut result = Vec::new();
        find_by_annotation_in(&self.root_module, name, pr::Path::empty(), &mut result);
        result
    }
}

fn find_by_annotation_in(
    module: &pr::ModuleDef,
    annotation_name: &str,
    mut path: pr::Path,
    result: &mut Vec<pr::Path>,
) {
    if has_annotation(&module.annotations, annotation_name) {
        result.push(path.clone());
    }

    for (name, def) in &module.defs {
        path.push(name.clone());

        if has_annotation(&def.annotations, annotation_name) {
            result.push(path.clone());
        }

        if let pr::DefKind::Module(inner) = &def.kind {
            find_by_annotation_in(inner, annotation_name, path.clone(), result);
        }

        path.pop();
    }
}

fn has_annotation(annotations: &[pr::Annotation], annotation_name: &str) -> bool {
    annotations
        .iter()
        .any(|ann| is_named(&ann.expr, annotation_name))
}

fn is_named(expr: &pr::Expr, name: &str) -> bool {
    is_ident(expr, name)
        || matches!(&expr.kind, pr::ExprKind::Call(call) if is_ident(&call.subject, name))
}

fn is_ident(expr: &pr::Expr, name: &str) -> bool {
    matches!(&expr.kind, pr::ExprKind::Ident(path) if (path.len() == 1 && path.first() == name))
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
    root: path::PathBuf,

    /// Mapping from file paths into into their contents.
    /// Paths are relative to the root.
    sources: IndexMap<path::PathBuf, String>,
}

impl SourceTree {
    pub fn empty() -> Self {
        SourceTree {
            sources: Default::default(),
            root: path::PathBuf::new(),
        }
    }

    pub fn single(path: path::PathBuf, content: String) -> Self {
        SourceTree {
            sources: [(path.clone(), content)].into(),
            root: path::PathBuf::new(),
        }
    }

    pub fn new<I>(iter: I, root: path::PathBuf) -> Self
    where
        I: IntoIterator<Item = (path::PathBuf, String)>,
    {
        SourceTree {
            sources: IndexMap::from_iter(iter),
            root,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.sources.len() == 0
    }

    pub fn insert(&mut self, path: path::PathBuf, content: String) {
        self.sources.insert(path, content);
    }

    pub fn replace(&mut self, path: &path::Path, content: String) -> Option<String> {
        let source = self.sources.get_mut(path)?;
        Some(std::mem::replace(source, content))
    }

    pub fn get_ids(&self) -> impl Iterator<Item = u16> {
        0..self.sources.len() as u16
    }
    pub fn get_sources(&self) -> impl Iterator<Item = (&path::PathBuf, &String)> {
        self.sources.iter()
    }
    pub fn get_root(&self) -> &std::path::Path {
        self.root.as_path()
    }

    pub fn get_by_id(&self, source_id: u16) -> Option<(&path::Path, &str)> {
        self.sources
            .get_index(source_id as usize)
            .map(|(p, c)| (p.as_path(), c.as_str()))
    }

    pub fn get_by_path(&self, path: &path::Path) -> Option<(u16, &str)> {
        self.sources
            .get_full(path)
            .map(|(i, _, content)| (i as u16, content.as_str()))
    }

    pub fn get_source_display_paths(&self) -> impl Iterator<Item = &path::Path> {
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
            self.root.file_name().map(path::Path::new)
        } else {
            Some(path)
        }
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

    pub const fn overlay_id() -> u16 {
        u16::MAX
    }
}

pub(crate) trait SourceProvider {
    fn get_root(&self) -> &path::Path;

    fn get_by_id(&self, id: u16) -> Option<(&path::Path, &str)>;

    fn get_by_path(&self, path: &path::Path) -> Option<(u16, &str)>;
}

impl SourceProvider for SourceTree {
    fn get_root(&self) -> &path::Path {
        &self.root
    }
    fn get_by_id(&self, id: u16) -> Option<(&path::Path, &str)> {
        SourceTree::get_by_id(self, id)
    }

    fn get_by_path(&self, path: &path::Path) -> Option<(u16, &str)> {
        SourceTree::get_by_path(self, path)
    }
}

impl<'a> SourceProvider for SourceOverlay<'a> {
    fn get_root(&self) -> &path::Path {
        &self.tree.root
    }

    fn get_by_id(&self, id: u16) -> Option<(&path::Path, &str)> {
        if id == SourceOverlay::overlay_id() {
            Some((self.snippet_path.as_path(), self.snippet))
        } else {
            self.tree.get_by_id(id)
        }
    }

    fn get_by_path(&self, path: &path::Path) -> Option<(u16, &str)> {
        if path == self.snippet_path {
            Some((SourceOverlay::overlay_id(), self.snippet))
        } else {
            self.tree.get_by_path(path)
        }
    }
}
