use lutra_compiler::{Project, SourceTree, pr};

use crate::completions::DefIcon;
use crate::terminal::{Line, Span, Style, View};

/// If `program` is a bare path that resolves to a module or type definition,
/// return a formatted string showing that definition.  Returns `None` otherwise,
/// causing the caller to fall through to normal compilation.
pub fn inspect_definition<'p>(program: &str, project: &'p Project) -> Option<View<'p>> {
    let program = program.trim();
    let path = if program == "project" || program == "module" {
        pr::Path::empty()
    } else {
        lutra_compiler::parse_path(program)?
    };

    if let Some(module_def) = project.root_module.get_submodule(path.as_steps()) {
        return Some(format_module(path, module_def, project));
    }

    let def = project.root_module.get(&path)?;
    if !def.kind.is_ty() {
        return None;
    }
    extract_type_source(&path, def, project)
}

/// Format a module's contents into a [`View`], with the module name bold and underlined.
fn format_module<'a>(
    path: pr::Path,
    module: &'a pr::ModuleDef,
    project: &'a lutra_compiler::Project,
) -> View<'a> {
    let mut view = View::new();

    if path.is_empty() {
        view.push_line(vec![Span::new("module")]);
    } else {
        view.push_line(vec![
            Span::new("module "),
            Span::styled(path.to_string(), Style::new().bold().underline()),
        ]);
    };

    let doc = (project.root_module.get(&path)).and_then(|def| def.doc_comment.as_ref());
    if let Some(doc) = doc {
        for line in doc.content.lines() {
            view.push_line(Line::styled(line.trim(), Style::muted()));
        }
    }

    for (name, def) in module.iter_defs() {
        let Some(icon) = DefIcon::from_def(def) else {
            continue;
        };
        view.push_line(vec![
            Span::styled(icon.char().to_string(), icon.style()),
            Span::new(" "),
            Span::new(name),
        ]);
    }

    view
}

/// Extract the full source text of a definition.
///
/// Uses `path` to identify the correct [`SourceTree`]: when the first path
/// segment matches a dependency name (e.g. `std`) the source is read from that
/// dependency's project.  Falls back to `None` when the span cannot be resolved
/// (e.g. a std-library type whose source tree lives in a different project).
fn extract_type_source<'p>(
    path: &pr::Path,
    def: &pr::Def,
    project: &'p Project,
) -> Option<View<'p>> {
    let span = def.span?;
    let source_tree = find_source_tree(path, project);
    let (_, source) = source_tree.get_by_id(span.source_id)?;
    let text = span.get_slice(source).trim();
    let mut view = View::new();
    for line in text.lines() {
        view.push_line(Line::new(line));
    }
    Some(view)
}

/// Walk the dependency tree to find the [`SourceTree`] that owns a definition
/// at `path`.  When the path's first segment names a known dependency, recurse
/// into that dependency's project; otherwise return the project's own sources.
fn find_source_tree<'a>(path: &pr::Path, project: &'a Project) -> &'a SourceTree {
    for dep in &project.dependencies {
        if path.starts_with_part(&dep.name) {
            return find_source_tree(path, &dep.inner);
        }
    }
    &project.source
}
