use std::path;

use lutra_compiler::{Project, codespan, pr};

pub fn update_schema(
    project: &Project,
    schema: &str,
) -> Result<Option<path::PathBuf>, anyhow::Error> {
    let Some(def_path) = find_schema_module_def(project)? else {
        return Ok(None);
    };

    let edit = rewrite_module_contents(project, &def_path, schema)?;
    let (path_written, _new_content) = write_edit(&project.source, edit)?;
    Ok(Some(path_written.to_path_buf()))
}

/// Find the unique `@schema`-annotated module in the project.
fn find_schema_module_def(project: &Project) -> Result<Option<pr::Path>, anyhow::Error> {
    let annotated = project.find_by_annotation("schema");

    match annotated.len() {
        0 => Ok(None),
        1 => Ok(Some(annotated.into_iter().next().unwrap())),
        n => Err(anyhow::anyhow!(
            "found {n} modules annotated with @schema, expected at most one"
        )),
    }
}

fn rewrite_module_contents<'p, 'c>(
    project: &'p Project,
    def: &'p pr::Path,
    contents: &'c str,
) -> Result<codespan::TextEdit<'c>, anyhow::Error> {
    let module_def = project.root_module.get_submodule(def.as_steps()).unwrap();
    let span_content = module_def
        .span_content
        .ok_or_else(|| anyhow::anyhow!("@schema module has no content span"))?;

    Ok(codespan::TextEdit::new(span_content, contents))
}

fn write_edit<'s>(
    source_tree: &'s lutra_compiler::SourceTree,
    edit: codespan::TextEdit<'_>,
) -> Result<(&'s path::Path, String), anyhow::Error> {
    // find source
    let (rel_path, source) = (source_tree.get_by_id(edit.span.source_id))
        .ok_or_else(|| anyhow::anyhow!("source file not found"))?;

    // apply
    let new_content = codespan::apply_text_edits(source, &[edit]);

    // write to fs
    let abs_path = source_tree.get_absolute_path(rel_path);
    std::fs::write(&abs_path, &new_content)?;

    Ok((rel_path, new_content))
}
