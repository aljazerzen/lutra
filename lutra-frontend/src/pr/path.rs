use std::fmt::Write;

/// A name referring to a statement within the module tree.
/// This is glorified way of writing a "vec with at least one element".
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct Path {
    path: Vec<String>,
}

impl Path {
    /// Creates a new ident from a non-empty path.
    pub fn new<S: ToString, I: IntoIterator<Item = S>>(path: I) -> Self {
        Path {
            path: path.into_iter().map(|x| x.to_string()).collect(),
        }
    }

    pub fn from_name<S: ToString>(name: S) -> Self {
        Path {
            path: vec![name.to_string()],
        }
    }

    pub fn name(&self) -> &str {
        self.path.last().unwrap()
    }

    pub fn path(&self) -> &[String] {
        &self.path[0..(self.len() - 1)]
    }

    pub fn first(&self) -> &str {
        self.path.first().unwrap()
    }

    pub fn full_path(&self) -> &[String] {
        &self.path
    }

    pub fn len(&self) -> usize {
        self.path.len()
    }

    pub fn is_empty(&self) -> bool {
        self.path.is_empty()
    }

    /// Remove last part of the ident.
    /// Result will generally refer to the parent of this ident.
    pub fn pop(&mut self) -> Option<String> {
        self.path.pop()
    }

    pub fn pop_front(&mut self) -> Option<String> {
        if self.path.is_empty() {
            None
        } else {
            Some(self.path.remove(0))
        }
    }

    pub fn prepend(self, mut parts: Vec<String>) -> Path {
        parts.extend(self);
        Path::new(parts)
    }

    pub fn push(&mut self, name: String) {
        self.path.push(name);
    }

    pub fn with_name<S: ToString>(mut self, name: S) -> Self {
        *self.path.last_mut().unwrap() = name.to_string();
        self
    }

    pub fn iter(&self) -> impl Iterator<Item = &String> {
        self.path.iter()
    }

    pub fn starts_with(&self, prefix: &Path) -> bool {
        if prefix.len() > self.len() {
            return false;
        }
        prefix
            .iter()
            .zip(self.iter())
            .all(|(prefix_component, self_component)| prefix_component == self_component)
    }

    pub fn starts_with_path<S: AsRef<str>>(&self, prefix: &[S]) -> bool {
        // self is an I
        if prefix.len() > self.len() {
            return false;
        }
        prefix
            .iter()
            .zip(self.iter())
            .all(|(prefix_component, self_component)| prefix_component.as_ref() == self_component)
    }

    pub fn starts_with_part(&self, prefix: &str) -> bool {
        self.starts_with_path(&[prefix])
    }
}

impl std::fmt::Debug for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(&self.path).finish()
    }
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        display_ident(f, self)
    }
}

impl IntoIterator for Path {
    type Item = String;
    type IntoIter = std::vec::IntoIter<std::string::String>;

    fn into_iter(self) -> Self::IntoIter {
        self.path.into_iter()
    }
}

impl std::ops::Add<Path> for Path {
    type Output = Path;

    fn add(mut self, rhs: Path) -> Self::Output {
        self.path.extend(rhs.path);
        self
    }
}

pub fn display_ident(f: &mut std::fmt::Formatter, ident: &Path) -> Result<(), std::fmt::Error> {
    let mut path = &ident.path[..];

    // HACK: don't display `_local` prefix
    // (this workaround is needed on feat-types branch)
    if path.first().map_or(false, |f| f == "_local") {
        path = &path[1..];
    }

    for (index, part) in path.iter().enumerate() {
        if index > 0 {
            f.write_char('.')?;
        }
        display_ident_part(f, part)?;
    }
    Ok(())
}

pub fn display_ident_part(f: &mut std::fmt::Formatter, s: &str) -> Result<(), std::fmt::Error> {
    fn forbidden_start(c: char) -> bool {
        !(c.is_ascii() || matches!(c, '_' | '$'))
    }
    fn forbidden_subsequent(c: char) -> bool {
        !(c.is_ascii() || c.is_ascii_digit() || matches!(c, '_'))
    }
    let needs_escape = s.is_empty()
        || s.starts_with(forbidden_start)
        || (s.len() > 1 && s.chars().skip(1).any(forbidden_subsequent));

    if needs_escape {
        write!(f, "`{s}`")
    } else {
        write!(f, "{s}")
    }
}
