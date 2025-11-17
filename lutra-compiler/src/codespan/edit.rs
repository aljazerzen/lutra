pub struct TextEdit {
    pub span: crate::Span,
    pub new_text: String,
}

/// Apply edits to the source code
pub fn apply_text_edits(text: &str, edits: &[TextEdit]) -> String {
    let mut out = String::new();
    let mut current_offset: usize = 0;

    for edit in edits {
        assert!(
            current_offset <= edit.span.start as usize,
            "[TextEdit]s are not ordered"
        );

        let start = (edit.span.start as usize).min(text.len());
        let end = (edit.span.end() as usize).min(text.len());

        out += &text[current_offset..start];
        out += &edit.new_text;
        current_offset = end;
    }
    out += &text[current_offset..];
    out
}

/// Drop text edits that do not change the source.
pub fn minimize_text_edits(text: &str, edits: Vec<TextEdit>) -> Vec<TextEdit> {
    edits
        .into_iter()
        .filter(|e| {
            let old_text = e.span.get_slice(text);
            old_text != e.new_text
        })
        .collect()
}

/// Apply an offset to span of each edit
pub fn offset_text_edits(edits: Vec<TextEdit>, offset: i32) -> Vec<TextEdit> {
    edits
        .into_iter()
        .map(|mut e| {
            e.span.start = if 0 <= offset {
                e.span.start.saturating_add(offset as u32)
            } else {
                e.span.start.saturating_sub((-offset) as u32)
            };
            e
        })
        .collect()
}
