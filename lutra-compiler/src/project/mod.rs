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
    /// Search the resolved module tree recursively for definitions that carry
    /// the annotation `@<annotation_name>` (bare identifier).
    ///
    /// Returns a vec of `(path, def)` pairs where `path` is the fully-qualified
    /// path of the definition within the module tree.
    pub fn find_by_annotation(&self, name: &str) -> Vec<pr::Path> {
        let mut result = Vec::new();
        find_by_annotation_re(&self.root_module, name, pr::Path::empty(), &mut result);
        result
    }

    /// Returns name defined with `@!project(name = "...")`.
    ///
    /// Represents the default name of this project when included as a dependency.
    pub fn get_name(&self) -> Option<&str> {
        let args = find_annotation(&self.root_module.annotations, "project")?;
        let val = find_annotation_arg(args, "name")?;
        Some(val.kind.as_literal()?.as_text()?)
    }
}

fn find_by_annotation_re(
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
            find_by_annotation_re(inner, annotation_name, path.clone(), result);
        }

        path.pop();
    }
}

fn has_annotation(annotations: &[pr::Annotation], annotation_name: &str) -> bool {
    annotations
        .iter()
        .any(|ann| as_named_annotation(&ann.expr, annotation_name).is_some())
}

fn find_annotation<'a>(annotations: &'a [pr::Annotation], name: &str) -> Option<&'a [pr::CallArg]> {
    annotations
        .iter()
        .find_map(|ann| as_named_annotation(&ann.expr, name))
}

fn find_annotation_arg<'a>(args: &'a [pr::CallArg], name: &str) -> Option<&'a pr::Expr> {
    for arg in args {
        if arg.label.as_ref().is_some_and(|l| l == name) {
            return Some(&arg.expr);
        }
    }
    None
}

fn as_named_annotation<'a>(expr: &'a pr::Expr, name: &str) -> Option<&'a [pr::CallArg]> {
    if is_ident(expr, name) {
        return Some(&[]);
    }
    if let pr::ExprKind::Call(call) = &expr.kind
        && is_ident(&call.subject, name)
    {
        return Some(&call.args);
    }
    None
}

fn is_ident(expr: &pr::Expr, name: &str) -> bool {
    matches!(&expr.kind, pr::ExprKind::Ident(path) if (path.len() == 1 && path.first() == name))
}
