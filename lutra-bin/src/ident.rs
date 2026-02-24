//! Common utils for Lutra identifiers

#[cfg(not(feature = "std"))]
use alloc::format;

pub fn display(s: &str) -> crate::borrow::Cow<'_, str> {
    fn forbidden_start(c: char) -> bool {
        !matches!(c, 'A'..='Z' | 'a'..='z' | '_')
    }
    fn forbidden_subsequent(c: char) -> bool {
        !matches!(c, 'A'..='Z' | 'a'..='z' | '0'..='9' | '_')
    }
    let needs_escape = s.is_empty()
        || s.starts_with(forbidden_start)
        || (s.len() > 1 && s.chars().skip(1).any(forbidden_subsequent));

    if needs_escape {
        format!("`{s}`").into()
    } else {
        s.into()
    }
}

#[test]
#[cfg(test)]
fn test_display_ident() {
    assert_eq!(display("Key"), "Key");
    assert_eq!(display("Key No"), "`Key No`");
    assert_eq!(display("#"), "`#`");
}
