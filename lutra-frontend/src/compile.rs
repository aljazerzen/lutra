use std::collections::HashMap;

use lutra_parser::pr;
// use prqlc::ir::decl::RootModule;
// use prqlc::ir::pl::{Ident, Literal};
// use prqlc::{semantic, Error, ErrorMessages, Errors, Options, SourceTree, Target, WithErrorInfo};

use crate::error;
use crate::error::Diagnostic;
use crate::ir::decl;
use crate::project;

pub use linearize::is_mod_def_for;

#[cfg_attr(feature = "clap", derive(clap::Parser))]
#[derive(Default)]
pub struct CompileParams {}

pub fn compile(
    source: project::SourceTree,
    _: CompileParams,
) -> Result<project::Project, crate::error::Error> {
    let root_module = parse_and_compile(&source)?;
    // .map_err(|e| e.composed(&source_tree))?;

    Ok(crate::Project {
        source,
        root_module,
    })
}

fn parse_and_compile(source_tree: &project::SourceTree) -> Result<decl::RootModule, error::Error> {
    // parse and resolve
    let ast_tree = parse(source_tree)?;
    crate::semantic::resolve(ast_tree)
        .map_err(Diagnostic::from_prql)
        .map_err(Diagnostic::into_error)
}

fn parse(file_tree: &project::SourceTree) -> Result<pr::ModuleDef, error::Error> {
    let source_files = linearize::linearize_tree(file_tree)?;

    // reverse the id->file_path map
    let ids: HashMap<_, _> = file_tree
        .source_ids
        .iter()
        .map(|(a, b)| (b.as_path(), a))
        .collect();

    // init the root module def
    let mut root = pr::ModuleDef {
        name: "Project".to_string(),
        stmts: Vec::new(),
    };

    // parse and insert into the root
    let mut errors = Vec::new();
    for source_file in source_files {
        let id = ids
            .get(&source_file.file_path)
            .map(|x| **x)
            .expect("source tree has malformed ids");

        let (ast, errs) = lutra_parser::parse_source(source_file.content, id);

        if let Some(stmts) = ast {
            linearize::insert_stmts_at_path(&mut root, source_file.module_path, stmts);
        } else {
            errors.extend(errs);
        }
    }
    if errors.is_empty() {
        Ok(root)
    } else {
        let diagnostics = errors.into_iter().map(Diagnostic::from_prql).collect();
        Err(error::Error::InvalidSource { diagnostics })
    }
}

mod linearize {
    use std::path::{Path, PathBuf};

    use itertools::Itertools;
    use lutra_parser::pr;

    use crate::error;
    use crate::project::SourceTree;

    pub(super) struct SourceFile<'a> {
        pub file_path: &'a Path,
        pub module_path: Vec<String>,
        pub content: &'a str,
    }

    pub fn linearize_tree(tree: &SourceTree) -> Result<Vec<SourceFile>, error::Error> {
        // find root
        let root_path;

        if tree.sources.len() == 1 {
            // if there is only one file, use that as the root
            root_path = tree.sources.keys().next().unwrap();
        } else if let Some(root) = tree.sources.get_key_value(&PathBuf::from("")) {
            // if there is an empty path, that's the root
            root_path = root.0;
        } else if let Some(root) = tree.sources.keys().find(path_starts_with_uppercase) {
            root_path = root;
        } else {
            if tree.sources.is_empty() {
                // TODO: should we allow non `.prql` files? We could require `.prql`
                // for modules but then allow any file if a single file is passed
                // (python allows this, for example)
                return Err(error::Error::InvalidSourceStructure {
                    problem: "No `.lt` files found".to_string(),
                });
            }

            let file_names = tree
                .sources
                .keys()
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

    pub fn insert_stmts_at_path(
        module: &mut pr::ModuleDef,
        mut path: Vec<String>,
        stmts: Vec<pr::Stmt>,
    ) {
        if path.is_empty() {
            module.stmts.extend(stmts);
            return;
        }

        let step = path.remove(0);

        // find submodule def
        let submodule = module.stmts.iter_mut().find(|x| is_mod_def_for(x, &step));
        let submodule = if let Some(sm) = submodule {
            sm
        } else {
            // insert new module def
            let new_stmt = pr::Stmt::new(pr::StmtKind::ModuleDef(pr::ModuleDef {
                name: step,
                stmts: Vec::new(),
            }));
            module.stmts.push(new_stmt);
            module.stmts.last_mut().unwrap()
        };
        let submodule = submodule.kind.as_module_def_mut().unwrap();

        insert_stmts_at_path(submodule, path, stmts);
    }

    pub fn is_mod_def_for(stmt: &pr::Stmt, name: &str) -> bool {
        stmt.kind.as_module_def().map_or(false, |x| x.name == name)
    }

    fn path_starts_with_uppercase(p: &&PathBuf) -> bool {
        p.components()
            .next()
            .and_then(|x| x.as_os_str().to_str())
            .and_then(|x| x.chars().next())
            .map_or(false, |x| x.is_uppercase())
    }

    pub fn os_path_to_prql_path(path: &Path) -> Result<Vec<String>, error::Error> {
        // remove file format extension
        let path = path.with_extension("");

        // split by /
        path.components()
            .map(|x| {
                x.as_os_str().to_str().map(str::to_string).ok_or_else(|| {
                    error::Diagnostic::custom(format!("Invalid file path: {path:?}")).into_error()
                })
            })
            .try_collect()
    }
}
