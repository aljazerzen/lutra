use std::collections::HashMap;
use std::fmt::{self, Write};
use std::path::{Path, PathBuf};

use crate::diagnostic::{Diagnostic, DiagnosticCode};

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("io error")]
    Io(#[from] std::io::Error),

    #[error("invalid path: {path}")]
    InvalidPath { path: PathBuf },

    #[error("invalid source structure: {problem}")]
    InvalidSourceStructure { problem: String },

    #[error("{}", DisplayMessages(.diagnostics))]
    Compile { diagnostics: Vec<DiagnosticMessage> },
}

impl Error {
    pub(crate) fn from_diagnostics(
        diagnostics: Vec<Diagnostic>,
        source: &crate::SourceTree,
    ) -> Self {
        let diagnostics = compose_diagnostic_messages(diagnostics, source);
        Error::Compile { diagnostics }
    }
}

#[derive(Debug)]
pub struct DiagnosticMessage {
    diagnostic: Diagnostic,

    display: String,

    location: SourceLocation,
}

impl DiagnosticMessage {
    pub fn code(&self) -> &'static str {
        self.diagnostic.code.get()
    }

    pub fn message(&self) -> &str {
        &self.diagnostic.message
    }

    pub fn span(&self) -> &Option<crate::Span> {
        &self.diagnostic.span
    }

    pub fn display(&self) -> &str {
        &self.display
    }

    pub fn location(&self) -> &SourceLocation {
        &self.location
    }
}

/// Location within the source file.
/// Tuples contain:
/// - line number (0-based),
/// - column number within that line (0-based),
#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub start: (usize, usize),

    pub end: (usize, usize),
}

fn compose_diagnostic_messages(
    diagnostics: Vec<Diagnostic>,
    sources: &crate::SourceTree,
) -> Vec<DiagnosticMessage> {
    use ariadne::Cache;

    let mut cache = FileTreeCache::new(sources);

    let mut messages = Vec::with_capacity(diagnostics.len());
    for diagnostic in diagnostics {
        if let Some(span) = diagnostic.span {
            let source_path = sources.source_ids.get(&span.source_id).unwrap().as_path();

            let source = cache.fetch(&source_path).unwrap();
            let Some(location) = compose_location(&diagnostic, source) else {
                panic!(
                    "span {:?} is out of bounds of the source (len = {})",
                    diagnostic.span,
                    source.len()
                );
            };

            let display = compose_display(&diagnostic, source_path, &mut cache);
            messages.push(DiagnosticMessage {
                diagnostic,
                display,
                location,
            });
        } else {
            panic!(
                "missing diagnostic span: [{:?}] {}, {:#?}",
                diagnostic.code, diagnostic.message, diagnostic.additional
            )
        }
    }
    messages
}

fn compose_display(
    diagnostic: &Diagnostic,
    source_path: &Path,
    cache: &mut FileTreeCache,
) -> String {
    use ariadne::{Config, Label, Report, ReportKind};

    let config = Config::default().with_color(false);

    let span = std::ops::Range::from(diagnostic.span.unwrap());

    let kind = match diagnostic.code.get_severity() {
        crate::diagnostic::Severity::Warning => ReportKind::Warning,
        crate::diagnostic::Severity::Error => ReportKind::Error,
    };

    let mut report = Report::build(kind, source_path, span.start)
        .with_config(config)
        .with_label(Label::new((source_path, span)).with_message(&diagnostic.message));

    if diagnostic.code != DiagnosticCode::CUSTOM {
        report = report.with_code(diagnostic.code.get());
    }

    let mut notes = String::new();
    for additional in &diagnostic.additional {
        if let Some(span) = additional.span {
            let span = std::ops::Range::from(span);
            report.add_label(Label::new((source_path, span)).with_message(&diagnostic.message))
        } else {
            notes += &additional.message;
            notes += "\n";
        }
    }
    if !notes.is_empty() {
        report.set_note(notes);
    }

    let mut out = Vec::new();
    report.finish().write(cache, &mut out).unwrap();
    String::from_utf8(out).unwrap()
}

fn compose_location(diagnostic: &Diagnostic, source: &ariadne::Source) -> Option<SourceLocation> {
    let span = diagnostic.span?;

    let start = source.get_offset_line(span.start)?;
    let end = source.get_offset_line(span.end)?;
    Some(SourceLocation {
        start: (start.1, start.2),
        end: (end.1, end.2),
    })
}

struct FileTreeCache<'a> {
    file_tree: &'a crate::SourceTree,
    cache: HashMap<PathBuf, ariadne::Source>,
}
impl<'a> FileTreeCache<'a> {
    fn new(file_tree: &'a crate::SourceTree) -> Self {
        FileTreeCache {
            file_tree,
            cache: HashMap::new(),
        }
    }
}

impl<'a> ariadne::Cache<&Path> for FileTreeCache<'a> {
    type Storage = String;
    fn fetch(&mut self, id: &&Path) -> Result<&ariadne::Source, Box<dyn fmt::Debug + '_>> {
        let file_contents = match self.file_tree.sources.get(*id) {
            Some(v) => v,
            None => return Err(Box::new(format!("Unknown file `{id:?}`"))),
        };

        Ok(self
            .cache
            .entry((*id).to_owned())
            .or_insert_with(|| ariadne::Source::from(file_contents.to_string())))
    }

    fn display<'b>(&self, id: &&'b Path) -> Option<Box<dyn fmt::Display + 'b>> {
        match id.as_os_str().to_str() {
            Some(s) => Some(Box::new(s)),
            None => None,
        }
    }
}

struct DisplayMessages<'a>(&'a Vec<DiagnosticMessage>);

impl<'a> std::fmt::Display for DisplayMessages<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for d in self.0 {
            f.write_str(&d.display)?;
            f.write_char('\n')?;
        }
        Ok(())
    }
}
