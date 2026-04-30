//! Pure text-editing helpers for the program input line.
//!
//! All functions operate on `(text: &str, col: usize)` where `col` is a byte
//! offset into `text` that is always kept on a char boundary.

/// Returns `true` for characters that belong to a "word"
fn is_word(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

/// Move one character backward; returns new byte offset.
pub fn move_backward(text: &str, col: usize) -> usize {
    text[..col.min(text.len())]
        .char_indices()
        .next_back()
        .map_or(0, |(i, _)| i)
}

/// Move one character forward; returns new byte offset.
pub fn move_forward(text: &str, col: usize) -> usize {
    let col = col.min(text.len());
    text[col..]
        .chars()
        .next()
        .map_or(col, |c| col + c.len_utf8())
}

/// Move to the start of the previous word; returns new byte offset.
pub fn move_word_backward(text: &str, col: usize) -> usize {
    let before = &text[..col.min(text.len())];
    let trimmed = before.trim_end_matches(|c: char| !is_word(c));
    trimmed.trim_end_matches(is_word).len()
}

/// Move to the end of the next word; returns new byte offset.
pub fn move_word_forward(text: &str, col: usize) -> usize {
    let col = col.min(text.len());
    let after = &text[col..];
    let trimmed = after.trim_start_matches(|c: char| !is_word(c));
    let rest = trimmed.trim_start_matches(is_word);
    col + after.len() - rest.len()
}

#[derive(Debug)]
pub struct Edit {
    pub at: usize,
    pub delete: usize,
    pub insert: String,
    pub move_cursor: isize,
}

impl Edit {
    pub fn delete(text: &str, at: usize, f: impl Fn(&str, usize) -> usize + 'static) -> Edit {
        let after = f(text, at);
        Edit {
            at: at.min(after),
            delete: at.abs_diff(after),
            insert: String::new(),
            move_cursor: -(at.saturating_sub(after) as isize),
        }
    }

    pub fn insert(at: usize, insert: String) -> Edit {
        Edit {
            at,
            move_cursor: insert.len() as isize,
            delete: 0,
            insert,
        }
    }

    pub fn apply(self, to: &mut String, cursor: &mut usize) {
        to.replace_range(self.at..(self.at + self.delete), &self.insert);
        *cursor = cursor.saturating_add_signed(self.move_cursor);
        *cursor = (*cursor).min(to.len());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_move_backward() {
        assert_eq!(move_backward("hello", 0), 0);
        assert_eq!(move_backward("hello", 3), 2);
        assert_eq!(move_backward("hello", 5), 4);
    }

    #[test]
    fn test_move_forward() {
        assert_eq!(move_forward("hello", 5), 5);
        assert_eq!(move_forward("hello", 0), 1);
        assert_eq!(move_forward("hello", 4), 5);
    }

    #[test]
    fn test_move_word_backward() {
        assert_eq!(move_word_backward("foo bar", 7), 4);
        assert_eq!(move_word_backward("foo bar", 3), 0);
        assert_eq!(move_word_backward("foo  bar", 8), 5);
        assert_eq!(move_word_backward("std::sql", 8), 0);
        assert_eq!(move_word_backward("", 0), 0);
    }

    #[test]
    fn test_move_word_forward() {
        assert_eq!(move_word_forward("foo bar", 0), 3);
        assert_eq!(move_word_forward("foo bar", 3), 7);
        assert_eq!(move_word_forward("foo  bar", 3), 8);
        assert_eq!(move_word_forward("std::sql", 0), 8);
        assert_eq!(move_word_forward("", 0), 0);
    }

    #[test]
    fn test_delete_char_backward() {
        let mut s = "hello".to_string();
        let mut c = 3;
        Edit::delete(&s, c, move_backward).apply(&mut s, &mut c);
        assert_eq!(s, "helo");
        assert_eq!(c, 2);
    }

    #[test]
    fn test_delete_word_backward() {
        let mut s = "foo bar".to_string();
        let mut c = 6;
        Edit::delete(&s, c, move_word_backward).apply(&mut s, &mut c);
        assert_eq!(s, "foo r");
        assert_eq!(c, 4);
    }

    #[test]
    fn test_insert_char() {
        let mut s = "hello".to_string();
        let mut c = 2;
        Edit::insert(c, "X".into()).apply(&mut s, &mut c);
        assert_eq!(s, "heXllo");
        assert_eq!(c, 3);
    }
}
