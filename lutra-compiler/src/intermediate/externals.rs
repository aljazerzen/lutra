use std::collections::HashSet;

use lutra_bin::ir;

use crate::{error, intermediate::fold};

/// Validate that a program only uses allowed external
pub fn validate_externals(
    program: ir::Program,
    allowed: &[std::borrow::Cow<str>],
) -> Result<ir::Program, error::Error> {
    let mut ext = Validator {
        allowed: HashSet::from_iter(allowed.iter().map(|x| x.as_ref())),
        errors: Vec::new(),
    };

    // allow the whole std:: by default
    // TODO: this is not ok, because it also includes std::fs and std::sql
    ext.allowed.insert("std");

    let program = fold::IrFold::fold_program(&mut ext, program).unwrap();
    if !ext.errors.is_empty() {
        return Err(error::Error::UnsupportedExternal {
            message: ext.errors.join("\n"),
        });
    }

    Ok(program)
}

struct Validator<'a> {
    allowed: HashSet<&'a str>,
    errors: Vec<String>, // TODO: IR has no spans, so we cannot emit real diagnostics
}

impl<'a> fold::IrFold for Validator<'a> {
    fn fold_ptr(&mut self, ptr: ir::Pointer, ty: ir::Ty) -> Result<ir::Expr, ()> {
        if let ir::Pointer::External(ptr) = &ptr {
            let is_allowed = PathPrefixes::new(&ptr.id).any(|prefix| self.allowed.contains(prefix));
            if !is_allowed {
                self.errors
                    .push(format!("missing implementation for `{}`", ptr.id));
            }
        }
        fold::fold_ptr(ptr, ty)
    }
}

struct PathPrefixes<'a> {
    x: &'a str,
    searched: usize,
}

impl<'a> PathPrefixes<'a> {
    fn new(x: &'a str) -> Self {
        PathPrefixes { x, searched: 0 }
    }
}
impl<'a> Iterator for PathPrefixes<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        if self.searched >= self.x.len() {
            return None;
        }
        let Some(segment_len) = self.x[self.searched..].find("::") else {
            self.searched = self.x.len();
            return Some(self.x);
        };
        self.searched += segment_len + 2;
        Some(&self.x[0..(self.searched - 2)])
    }
}

#[cfg(test)]
mod tests {
    use super::PathPrefixes;

    #[test]
    fn empty() {
        let p: Vec<_> = PathPrefixes::new("").collect();
        assert_eq!(p, Vec::<&str>::new());
    }

    #[test]
    fn no_separator() {
        let p: Vec<_> = PathPrefixes::new("foo").collect();
        assert_eq!(p, vec!["foo"]);
    }

    #[test]
    fn single_separator() {
        let p: Vec<_> = PathPrefixes::new("a::b").collect();
        assert_eq!(p, vec!["a", "a::b"]);
    }

    #[test]
    fn multiple_separators() {
        let p: Vec<_> = PathPrefixes::new("a::b::c").collect();
        assert_eq!(p, vec!["a", "a::b", "a::b::c"]);
    }

    #[test]
    fn trailing_separator() {
        let p: Vec<_> = PathPrefixes::new("a::b::").collect();
        assert_eq!(p, vec!["a", "a::b"]);
    }

    #[test]
    fn leading_separator() {
        let p: Vec<_> = PathPrefixes::new("::a::b").collect();
        assert_eq!(p, vec!["", "::a", "::a::b"]);
    }

    #[test]
    fn consecutive_separators() {
        let p: Vec<_> = PathPrefixes::new("a::::b").collect();
        assert_eq!(p, vec!["a", "a::", "a::::b"]);
    }
}
