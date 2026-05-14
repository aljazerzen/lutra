use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::{fs, path};

use anyhow::Context;
use lutra_compiler::Project;
use lutra_compiler::pr;
use lutra_compiler::printer;

#[derive(Debug, Clone)]
pub struct MarkdownPage {
    pub path: PathBuf,
    pub content: String,
}

pub fn generate_md_pages(project: &Project) -> anyhow::Result<Vec<MarkdownPage>> {
    let path_prefix = project
        .get_name()
        .map(pr::Path::from_name)
        .unwrap_or_else(pr::Path::empty);

    let dependencies: HashSet<&str> = project
        .dependencies
        .iter()
        .map(|d| d.name.as_str())
        .collect();

    render_module(
        &project.root_module,
        &project.root_module,
        &pr::Path::empty(),
        &path_prefix,
        &dependencies,
    )
}

pub fn write_md_pages(output_dir: &Path, pages: &[MarkdownPage]) -> anyhow::Result<()> {
    if output_dir.exists() {
        fs::remove_dir_all(output_dir)
            .with_context(|| format!("failed to remove {}", output_dir.display()))?;
    }
    fs::create_dir_all(output_dir)
        .with_context(|| format!("failed to create {}", output_dir.display()))?;

    for page in pages {
        let path = output_dir.join(&page.path);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("failed to create {}", parent.display()))?;
        }
        fs::write(&path, &page.content)
            .with_context(|| format!("failed to write {}", path.display()))?;
    }

    Ok(())
}

/// Renders one module page and recursively renders all child module pages.
/// `root` is the project's root `ModuleDef` and is threaded through unchanged
/// so that `relative_link` / `resolve_doc_links` can look up any fully-qualified
/// path via `root.get(fq_path)` — replacing the flat `Index` that previously
/// required a separate collection pass.
fn render_module<'a>(
    module: &'a pr::ModuleDef,
    root: &'a pr::ModuleDef,
    path: &pr::Path,
    path_prefix: &pr::Path,
    dependencies: &HashSet<&'a str>,
) -> anyhow::Result<Vec<MarkdownPage>> {
    let doc = root.get_doc_at(path).unwrap_or_default();
    let (title, doc) = parse_doc_comment(doc);
    let path_display = path_prefix.clone() + path.clone();

    let mut content = String::new();

    // --- Page header ---
    content += "# ";
    if !path_display.is_empty() {
        content += "`";
        content += &path_display.to_string();
        content += "`";
    }
    if let Some(title) = title {
        if !path_display.is_empty() {
            content += " - ";
        }
        content += title;
    }
    content += "\n\n";

    if !doc.is_empty() {
        content += &resolve_doc_links(doc, path, root);
        content += "\n\n";
    }

    // Partition defs into child modules and leaf definitions (source order preserved).
    let mut sub_modules: Vec<(&String, &pr::Def)> = Vec::new();
    let mut leafs: Vec<(&String, &pr::Def)> = Vec::new();
    for (name, def) in module.iter_defs() {
        if path.is_empty() && dependencies.contains(name.as_str()) {
            continue;
        }
        match &def.kind {
            pr::DefKind::Import(_) => {}
            pr::DefKind::Module(_) => sub_modules.push((name, def)),
            _ => leafs.push((name, def)),
        }
    }

    // --- Modules section ---
    if !sub_modules.is_empty() {
        content += "## Modules\n\n";
        for (name, def) in &sub_modules {
            let child_path = path.clone() + pr::Path::from_name(*name);
            let Some(link) = relative_link(path, &child_path, root) else {
                continue;
            };
            let child_doc = def.get_doc().unwrap_or_default();
            let (child_title, _) = parse_doc_comment(child_doc);

            content += &format!("- [`{name}`]({link})");
            if let Some(child_title) = child_title {
                content += &format!(" - {}", resolve_doc_links(child_title, path, root));
            }
            content += "\n";
        }
        content += "\n";
    }

    // --- Leaf definitions ---
    for (name, def) in &leafs {
        let kind = match &def.kind {
            pr::DefKind::Expr(e) if e.constant => "const",
            pr::DefKind::Expr(_) => "func",
            pr::DefKind::Ty(_) => "type",
            pr::DefKind::Anno(_) => "anno",
            _ => unreachable!(),
        };
        let signature = printer::print_def_signature(name, def)
            .with_context(|| format!("failed to render signature for {name}"))?;

        content += &format!("## `{kind}` {name}\n\n");
        content += "```lutra\n";
        content += &signature;
        content += "\n```\n";
        if let Some(doc) = def.get_doc() {
            content += "\n";
            content += &resolve_doc_links(doc, path, root);
            content += "\n";
        }
        content += "\n";
    }

    // Collect this page first, then child pages (DFS pre-order).
    let mut pages = vec![MarkdownPage {
        path: page_filepath(path.as_steps()),
        content,
    }];

    for (name, def) in sub_modules {
        let child_path = path.clone() + pr::Path::from_name(name);
        let sub_module = def.kind.as_module().unwrap();
        pages.extend(render_module(
            sub_module,
            root,
            &child_path,
            path_prefix,
            dependencies,
        )?);
    }

    Ok(pages)
}

fn parse_doc_comment(doc: &str) -> (Option<&str>, &str) {
    let doc_trimmed = doc.trim_start();
    let title = doc_trimmed.lines().next();

    let Some(title) = title else {
        return (None, doc);
    };

    let content_start = (doc.len() - doc_trimmed.len()) + title.len();
    let content = &doc[content_start..];
    (Some(title), content.trim_start())
}

fn page_filepath(path: &[String]) -> PathBuf {
    let mut filepath = PathBuf::new();
    for step in path {
        filepath.push(step);
    }
    filepath.push("index.md");
    filepath
}

/// Returns a relative URL from the page at `current_mod` to `target_fq`.
/// Uses `root.get(target_fq)` to determine whether the target is a module
/// (links to its `index.md`) or a leaf definition (links with an `#anchor`).
fn relative_link(
    current_mod: &pr::Path,
    target_fq: &pr::Path,
    root: &pr::ModuleDef,
) -> Option<String> {
    let is_module = target_fq.is_empty() || root.get(target_fq)?.kind.is_module();
    let (target_mod, anchor) = if is_module {
        (target_fq.as_steps(), None)
    } else {
        (target_fq.parent()?, Some(target_fq.last()))
    };

    let from_file = page_filepath(current_mod.as_steps());
    let to_file = page_filepath(target_mod);

    let common = std::iter::zip(from_file.components(), to_file.components())
        .filter(|(a, b)| a == b)
        .count();
    let mut rel = PathBuf::new();
    for _ in 0..(from_file.components().count() - common).saturating_sub(1) {
        rel.push(path::Component::ParentDir);
    }
    for c in to_file.components().skip(common) {
        rel.push(c);
    }

    let mut url = rel.to_str()?.to_string();
    if let Some(anchor) = anchor {
        url += "#";
        url += anchor;
    }
    Some(url)
}

fn resolve_doc_links(text: &str, current_mod: &pr::Path, root: &pr::ModuleDef) -> String {
    let mut out = String::new();
    let chars: Vec<char> = text.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        if chars[i] != '[' {
            out.push(chars[i]);
            i += 1;
            continue;
        }

        let mut j = i + 1;
        while j < chars.len() && chars[j] != ']' {
            j += 1;
        }
        if j == chars.len() {
            out.push(chars[i]);
            i += 1;
            continue;
        }

        let inner: String = chars[(i + 1)..j].iter().collect();
        let next = chars.get(j + 1).copied();
        if next == Some('(') {
            out.push(chars[i]);
            i += 1;
            continue;
        }

        if let Some(target_key) = resolve_reference(&inner, current_mod)
            && let Some(link) = relative_link(current_mod, &target_key, root)
        {
            out += &format!("[`{inner}`]({link})");
            i = j + 1;
            continue;
        }

        out.push(chars[i]);
        i += 1;
    }

    out
}

fn resolve_reference(reference: &str, current_module: &pr::Path) -> Option<pr::Path> {
    let mut parts: Vec<_> = reference.split("::").map(str::to_string).collect();
    if parts.is_empty() {
        return None;
    }

    let mut r = match parts[0].as_str() {
        "project" => {
            parts.remove(0);
            pr::Path::empty()
        }
        "super" => {
            parts.remove(0);
            pr::Path::new(current_module.parent()?.to_vec())
        }
        _ => current_module.clone(),
    };
    for p in parts {
        r.push(p);
    }
    Some(r)
}
