use std::collections::HashMap;
use std::path::{Path, PathBuf};

use itertools::Itertools;

use crate::diagnostic::Diagnostic;
use crate::error;
use crate::error::Error;
use crate::pr;
use crate::project;
use crate::project::SourceOverlay;
use crate::project::SourceProvider;
use crate::{SourceTree, diagnostic};

#[cfg_attr(feature = "clap", derive(clap::Parser))]
#[derive(Default)]
pub struct CheckParams {}

pub fn check(
    mut source: project::SourceTree,
    _: CheckParams,
) -> Result<project::Project, error::Error> {
    if source.sources.is_empty() {
        source.insert(PathBuf::from(""), "".into());
    }

    crate::resolver::load_std_lib(&mut source);
    let source = source;

    let source_files = linearize_tree(&source)?;

    let mut project = parse(&source, source_files)
        .and_then(crate::resolver::resolve)
        .map_err(|e| Error::from_diagnostics(e, &source))?;
    project.source = source;
    Ok(project)
}

fn parse(tree: &SourceTree, files: Vec<SourceFile>) -> Result<pr::ModuleDef, Vec<Diagnostic>> {
    // reverse the id->file_path map
    let ids: HashMap<_, _> = tree
        .source_ids
        .iter()
        .map(|(a, b)| (b.as_path(), a))
        .collect();

    // init the root module def
    let mut root = pr::ModuleDef {
        defs: Default::default(),
    };

    // parse and insert into the root
    let mut diagnostics = Vec::new();
    for source_file in files {
        let id = ids
            .get(&source_file.file_path)
            .map(|x| **x)
            .expect("source tree has malformed ids");

        let (ast, errs) = crate::parser::parse_source(source_file.content, id);

        diagnostics.extend(errs);
        if let Some(module) = ast {
            diagnostics.extend(insert_module_at_path(
                &mut root,
                source_file.module_path,
                module,
            ));
        }
    }
    if diagnostics.is_empty() {
        Ok(root)
    } else {
        Err(diagnostics)
    }
}

pub fn check_overlay(
    project: &project::Project,
    overlay: &str,
    overlay_name: Option<&str>,
) -> Result<pr::Expr, error::Error> {
    let source = crate::project::SourceOverlay::new(&project.source, overlay, overlay_name);

    parse_overlay(&source)
        .and_then(|expr| crate::resolver::resolve_overlay_expr(&project.root_module, expr))
        .map_err(|e| Error::from_diagnostics(e, &source))
}

fn parse_overlay(overlay: &SourceOverlay) -> Result<pr::Expr, Vec<Diagnostic>> {
    let snippet = overlay.get_source(overlay.get_path(0).unwrap()).unwrap();
    let (ast, diagnostics) = crate::parser::parse_expr(snippet, 0);
    if diagnostics.is_empty() {
        Ok(ast.unwrap())
    } else {
        Err(diagnostics)
    }
}

pub(super) struct SourceFile<'a> {
    pub file_path: &'a Path,
    pub module_path: Vec<String>,
    pub content: &'a str,
}

pub fn linearize_tree(tree: &SourceTree) -> Result<Vec<SourceFile<'_>>, error::Error> {
    // find root
    let root_path;

    let std = Path::new("std.lt");
    let sources: Vec<_> = tree.sources.keys().filter(|p| p != &std).collect();

    if sources.len() == 1 {
        // if there is only one file, use that as the root
        root_path = sources.into_iter().next().unwrap();
    } else if let Some(root) = tree.sources.get_key_value(&PathBuf::from("")) {
        // if there is an empty path, that's the root
        root_path = root.0;
    } else if let Some(root) = sources.iter().cloned().find(path_starts_with_uppercase) {
        root_path = root;
    } else {
        if tree.sources.is_empty() {
            return Err(error::Error::InvalidSourceStructure {
                problem: "No `.lt` files found".to_string(),
            });
        }

        let file_names = sources
            .iter()
            .map(|p| format!(" - {}", p.to_str().unwrap_or_default()))
            .sorted()
            .join("\n");

        return Err(error::Error::InvalidSourceStructure {
            problem: format!(
                "Cannot determine the root module within the following files:\n{file_names}"
            ),
        });
    }

    let mut sources: Vec<_> = Vec::with_capacity(tree.sources.len());

    // prepare paths
    for (path, source) in &tree.sources {
        if path == root_path {
            continue;
        }

        let module_path = os_path_to_prql_path(path)?;

        sources.push(SourceFile {
            file_path: path,
            module_path,
            content: source,
        });
    }

    // sort to make this deterministic
    sources.sort_by(|a, b| a.module_path.cmp(&b.module_path));

    // add root
    let root_content = tree.sources.get(root_path).unwrap();
    sources.push(SourceFile {
        file_path: root_path,
        module_path: Vec::new(),
        content: root_content,
    });

    Ok(sources)
}

pub fn insert_module_at_path(
    module: &mut pr::ModuleDef,
    mut path: Vec<String>,
    to_insert: pr::ModuleDef,
) -> Vec<diagnostic::Diagnostic> {
    if path.is_empty() {
        let mut d = Vec::new();
        for (name, def) in to_insert.defs {
            let conflict = module.defs.insert(name, def);
            if let Some(conflict) = conflict {
                d.push(
                    diagnostic::Diagnostic::new_custom("duplicate name").with_span(conflict.span),
                );
            }
        }
        return d;
    }

    let step = path.remove(0);

    // find submodule def
    let submodule = module.defs.entry(step).or_insert_with(|| {
        pr::Def::new(pr::DefKind::Module(pr::ModuleDef {
            defs: Default::default(),
        }))
    });
    let pr::DefKind::Module(submodule) = &mut submodule.kind else {
        return vec![
            diagnostic::Diagnostic::new_custom("duplicate name").with_span(submodule.span),
        ];
    };
    insert_module_at_path(submodule, path, to_insert)
}

fn path_starts_with_uppercase(p: &&PathBuf) -> bool {
    p.components()
        .next()
        .and_then(|x| x.as_os_str().to_str())
        .and_then(|x| x.chars().next())
        .is_some_and(|x| x.is_uppercase())
}

fn os_path_to_prql_path(path: &Path) -> Result<Vec<String>, error::Error> {
    // remove file format extension
    let path = path.with_extension("");

    // split by /
    path.components()
        .map(|x| {
            x.as_os_str()
                .to_str()
                .map(str::to_string)
                .ok_or_else(|| error::Error::InvalidPath { path: path.clone() })
        })
        .try_collect()
}
