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
    pub fn empty() -> Self {
        Path { path: vec![] }
    }

    pub fn from_name<S: ToString>(name: S) -> Self {
        Path {
            path: vec![name.to_string()],
        }
    }

    pub fn first(&self) -> &str {
        self.path.first().unwrap()
    }

    pub fn last(&self) -> &str {
        self.path.last().unwrap()
    }

    pub fn parent(&self) -> &[String] {
        &self.path[0..(self.len() - 1)]
    }

    pub fn as_steps(&self) -> &[String] {
        &self.path
    }

    pub fn len(&self) -> usize {
        self.path.len()
    }

    pub fn is_empty(&self) -> bool {
        self.path.is_empty()
    }

    pub fn prepend(self, mut prefix: Path) -> Path {
        prefix.path.extend(self.path);
        prefix
    }

    pub fn extend(&mut self, suffix: Path) {
        self.path.extend(suffix);
    }

    pub fn push(&mut self, name: String) {
        self.path.push(name);
    }

    pub fn pop(&mut self) -> Option<String> {
        self.path.pop()
    }

    pub fn pop_first(&mut self) -> Option<String> {
        if self.is_empty() {
            return None;
        }
        let remaining = self.path.split_off(1);
        let first = std::mem::replace(&mut self.path, remaining);
        Some(first.into_iter().next().unwrap())
    }

    pub fn with_name<S: ToString>(mut self, name: S) -> Self {
        *self.path.last_mut().unwrap() = name.to_string();
        self
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &String> {
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
        f.write_str(&display_path(self))
    }
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&display_path(self))
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

fn display_path(ident: &Path) -> String {
    let path = &ident.path[..];

    let mut r = String::new();
    for (index, part) in path.iter().enumerate() {
        if index > 0 {
            r += "::";
        }
        r += display_ident(part).as_ref();
    }
    r
}

pub fn display_ident(s: &str) -> std::borrow::Cow<'_, str> {
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
        format!("`{s}`").into()
    } else {
        s.into()
    }
}
