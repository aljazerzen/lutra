use crate::SourceTree;

pub fn format(source_tree: SourceTree) -> SourceTree {
    let mut result = SourceTree::empty();

    for (id, path) in source_tree.source_ids {
        let content = source_tree.sources.get(&path).unwrap();

        let (ast, _diagnostics, trivia) = crate::parser::parse_source(content, id);

        if let Some(ast) = ast {
            let formatted = crate::printer::print_source(&ast, Some(&trivia));

            result.insert(path, formatted);
        }
    }
    result
}
