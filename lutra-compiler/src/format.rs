use crate::SourceTree;
use crate::error;

pub fn format(source_tree: &SourceTree) -> (Option<error::Error>, SourceTree) {
    let mut formatted_tree = SourceTree::empty();
    let mut diagnostics = Vec::new();

    for (id, path) in &source_tree.source_ids {
        let content = source_tree.sources.get(path).unwrap();

        let (parsed, diags, trivia) = crate::parser::parse_source(content, *id);

        if diagnostics.is_empty()
            && let Some(parsed) = parsed
        {
            let formatted = crate::printer::print_source(&parsed.root, Some(&trivia));

            formatted_tree.insert(path.clone(), formatted);
        }

        diagnostics.extend(diags);
    }

    let err = if diagnostics.is_empty() {
        None
    } else {
        Some(error::Error::from_diagnostics(diagnostics, source_tree))
    };

    (err, formatted_tree)
}
