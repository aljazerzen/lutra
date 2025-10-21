use std::collections::HashMap;
use std::path::{Path, PathBuf};

use itertools::Itertools;

use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::error::Error;
use crate::pr;
use crate::project::SourceOverlay;
use crate::project::SourceProvider;
use crate::project::{self, Dependency};
use crate::resolver::NS_STD;
use crate::{SourceTree, diagnostic};
use crate::{Span, error};

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

    let std_lib = Dependency {
        name: NS_STD.into(),
        inner: compile_std_lib()?,
    };

    let mut project = parse(&source)?
        .and_then(|ast| crate::resolver::resolve(ast, vec![std_lib]))
        .map_err(|e| Error::from_diagnostics(e, &source))?;
    project.source = source;
    Ok(project)
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

fn parse(tree: &SourceTree) -> Result<Result<pr::ModuleDef, Vec<Diagnostic>>, error::Error> {
    // reverse the id->file_path map
    let ids: HashMap<_, _> = tree.source_ids.iter().map(|(a, b)| (b, a)).collect();

    // init the root module def
    let mut root = pr::ModuleDef {
        defs: Default::default(),
    };

    // parse and insert into the root
    let mut diags = Vec::new();
    for (file_path, content) in tree.get_sources() {
        let id = ids
            .get(&file_path)
            .map(|x| **x)
            .expect("source tree has malformed ids");

        let module_path = os_path_to_mod_path(file_path)?;

        let (parsed, errs, _) = crate::parser::parse_source(content, id);
        diags.extend(errs);
        if let Some(parsed) = parsed {
            // TODO: improve these error messages

            if module_path.is_empty() && parsed.is_submodule {
                diags.push(
                    Diagnostic::new_custom("cannot load the project root")
                        .with_span(Some(Span {
                            start: 0,
                            len: 1,
                            source_id: id,
                        }))
                        .push_hint(format!("file {} is a submodule", file_path.display())),
                );
            }
            let included = module_path.is_empty() || parsed.is_submodule;
            if included {
                diags.extend(insert_module_at_path(&mut root, module_path, parsed.root));
            }
        }
    }
    Ok(if diags.is_empty() {
        Ok(root)
    } else {
        Err(diags)
    })
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

fn os_path_to_mod_path(path: &Path) -> Result<Vec<String>, error::Error> {
    let path = if path.ends_with("module.lt") {
        // remove module.lt suffix
        path.parent().unwrap().to_path_buf()
    } else {
        // remove file format extension
        path.with_extension("")
    };

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

fn compile_std_lib() -> Result<crate::Project, error::Error> {
    let source = SourceTree::single(
        std::path::PathBuf::new(),
        include_str!("std.lt").to_string(),
    );

    let mut project = parse(&source)?
        .and_then(|root| crate::resolver::resolve(root, vec![]))
        .map_err(|e| Error::from_diagnostics(e, &source))?;
    project.source = source;
    Ok(project)
}
