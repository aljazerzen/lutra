use crate::SourceTree;
use crate::codespan;
use crate::error;

pub fn format(source_tree: &SourceTree) -> (Option<error::Error>, Vec<codespan::TextEdit>) {
    let mut edits = Vec::new();
    let mut diagnostics = Vec::new();

    for (id, path) in &source_tree.source_ids {
        let content = source_tree.sources.get(path).unwrap();

        let (parsed, diags, trivia) = crate::parser::parse_source(content, *id);

        if diagnostics.is_empty()
            && let Some(parsed) = parsed
        {
            let e = crate::printer::print_source(&parsed, Some(&trivia));
            edits.extend(codespan::minimize_text_edits(content, e));
        }

        diagnostics.extend(diags);
    }

    let err = if diagnostics.is_empty() {
        None
    } else {
        Some(error::Error::from_diagnostics(diagnostics, source_tree))
    };

    (err, edits)
}
