mod analysis;
mod source_tree;

pub use analysis::{SymbolInfo, TargetMap, TargetSpan};
pub(crate) use source_tree::SourceProvider;
pub use source_tree::{SourceOverlay, SourceTree};

use std::sync::Arc;

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
    /// Returns name defined with `@!project(name = "...")`.
    ///
    /// Represents the default name of this project when included as a dependency.
    pub fn get_name(&self) -> Option<&str> {
        self.root_module
            .get_anno_at(&pr::Path::empty(), pr::Anno::as_std_package)
    }

    /// Search the resolved module tree recursively for definitions that carry
    /// the annotation `@<annotation_name>`.
    ///
    /// Returns a vec of fully-qualified paths of matching defs.
    pub fn find_by_anno<'a, R: 'a>(
        &'a self,
        matcher: impl Fn(&'a pr::Anno) -> Option<R> + Copy,
    ) -> Vec<(pr::Path, R)> {
        let mut result = Vec::new();
        let empty = pr::Path::empty();
        if let Some(r) = self.root_module.get_anno_at(&empty, matcher) {
            result.push((empty.clone(), r));
        }
        find_by_anno_re(&self.root_module, matcher, empty, &mut result);
        result
    }
}

fn find_by_anno_re<'a, R: 'a>(
    module: &'a pr::ModuleDef,
    matcher: impl Fn(&'a pr::Anno) -> Option<R> + Copy,
    mut path: pr::Path,
    result: &mut Vec<(pr::Path, R)>,
) {
    for (name, def) in &module.defs {
        path.push(name.clone());

        if let Some(r) = def.get_anno(matcher) {
            result.push((path.clone(), r));
        }

        if let pr::DefKind::Module(inner) = &def.kind {
            find_by_anno_re(inner, matcher, path.clone(), result);
        }

        path.pop();
    }
}
