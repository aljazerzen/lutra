use std::path;

use lutra_compiler::{Project, codespan, pr};

/// Find the unique `@schema`-annotated module in the project.
///
/// Returns:
/// - `Ok(Some(target))` – exactly one `@schema` annotation found.
/// - `Ok(None)` – no `@schema` annotation found; caller should fall back to stdout.
/// - `Err(msg)` – more than one `@schema` annotation found.
pub fn find_schema_module_def(project: &Project) -> Result<Option<(pr::Path, &pr::Def)>, String> {
    let annotated = project.find_by_annotation("schema");

    let targets: Vec<(pr::Path, &pr::Def)> = annotated
        .into_iter()
        .filter(|(_, def)| {
            // Only consider module defs.
            matches!(def.kind, pr::DefKind::Module(_))
        })
        .collect();

    match targets.len() {
        0 => Ok(None),
        1 => Ok(Some(targets.into_iter().next().unwrap())),
        n => Err(format!(
            "found {n} modules annotated with @schema, expected at most one"
        )),
    }
}

pub fn rewrite_module_contents<'p>(
    project: &'p lutra_compiler::Project,
    def: &'p lutra_compiler::pr::Def,
    contents: String,
) -> Result<&'p path::Path, anyhow::Error> {
    let lutra_compiler::pr::DefKind::Module(module_def) = &def.kind else {
        unreachable!("find_schema_module_def only returns module defs")
    };
    let span_content = module_def
        .span_content
        .ok_or_else(|| anyhow::anyhow!("@schema module has no content span"))?;

    let edit = codespan::TextEdit {
        span: span_content,
        new_text: contents,
    };
    write_edit(&project.source, edit)
}

fn write_edit(
    source_tree: &lutra_compiler::SourceTree,
    edit: codespan::TextEdit,
) -> Result<&path::Path, anyhow::Error> {
    // find source
    let (rel_path, source) = (source_tree.get_by_id(edit.span.source_id))
        .ok_or_else(|| anyhow::anyhow!("source file not found"))?;

    // apply
    let new_content = codespan::apply_text_edits(source, &[edit]);

    // write
    let abs_path = source_tree.get_absolute_path(rel_path);
    std::fs::write(&abs_path, &new_content)?;

    Ok(rel_path)
}
