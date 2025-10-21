use std::collections::HashMap;
use std::fmt::{self, Write};
use std::path::{Path, PathBuf};

use itertools::Itertools;

use crate::codespan;
use crate::diagnostic::{Diagnostic, DiagnosticCode};

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("io error")]
    Io(#[from] std::io::Error),

    #[error("invalid path: {path}")]
    InvalidPath { path: PathBuf },

    #[error("cannot find project root")]
    CannotFindProjectRoot,

    #[error("cannot read source file {file}: {io:?}")]
    CannotReadSourceFile {
        file: std::path::PathBuf,
        io: std::io::Error,
    },

    #[error("invalid source structure: {problem}")]
    InvalidSourceStructure { problem: String },

    #[error("{}", DisplayMessages(.diagnostics))]
    Compile { diagnostics: Vec<DiagnosticMessage> },
}

impl Error {
    pub(crate) fn from_diagnostics(
        diagnostics: Vec<Diagnostic>,
        sources: &impl crate::project::SourceProvider,
    ) -> Self {
        let diagnostics = compose_diagnostic_messages(diagnostics, sources);
        Error::Compile { diagnostics }
    }
}

#[derive(Debug)]
pub struct DiagnosticMessage {
    diagnostic: Diagnostic,

    display: String,

    range: codespan::Range,
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

    pub fn range(&self) -> &codespan::Range {
        &self.range
    }
}

fn compose_diagnostic_messages(
    diagnostics: Vec<Diagnostic>,
    sources: &impl crate::project::SourceProvider,
) -> Vec<DiagnosticMessage> {
    use ariadne::Cache;

    let mut cache = FileTreeCache::new(sources);

    let mut messages = Vec::with_capacity(diagnostics.len());
    for diagnostic in diagnostics {
        if let Some(span) = diagnostic.span {
            let source_path = sources.get_path(span.source_id).unwrap();

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
                range: location,
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

fn compose_display<S>(
    diagnostic: &Diagnostic,
    source_path: &Path,
    cache: &mut FileTreeCache<S>,
) -> String
where
    S: crate::project::SourceProvider,
{
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
            notes += "\n   │ ";
            notes += &additional.message;
        }
    }
    if !notes.is_empty() {
        report.set_note(notes);
    }

    let mut out = Vec::new();
    report.finish().write(cache, &mut out).unwrap();
    let out = String::from_utf8(out).unwrap();
    out.lines().map(|l| l.trim_end()).join("\n")
}

fn compose_location(diagnostic: &Diagnostic, source: &ariadne::Source) -> Option<codespan::Range> {
    let span = diagnostic.span?;

    let start = source.get_byte_line(span.start as usize)?;
    let start = codespan::LineColumn {
        line: start.1 as u32,
        column: start.2 as u32,
    };

    let end = source.get_byte_line(span.start as usize + span.len as usize)?;
    let end = codespan::LineColumn {
        line: end.1 as u32,
        column: end.2 as u32,
    };
    Some(codespan::Range { start, end })
}

struct FileTreeCache<'a, S: crate::project::SourceProvider> {
    provider: &'a S,
    cache: HashMap<PathBuf, ariadne::Source>,
}
impl<'a, S: crate::project::SourceProvider> FileTreeCache<'a, S> {
    fn new(file_tree: &'a S) -> Self {
        FileTreeCache {
            provider: file_tree,
            cache: HashMap::new(),
        }
    }
}

impl<'a, S: crate::project::SourceProvider> ariadne::Cache<&Path> for FileTreeCache<'a, S> {
    type Storage = String;
    fn fetch(&mut self, path: &&Path) -> Result<&ariadne::Source, Box<dyn fmt::Debug + '_>> {
        let file_contents = match self.provider.get_source(path) {
            Some(v) => v,
            None => return Err(Box::new(format!("Unknown file `{path:?}`"))),
        };

        Ok(self
            .cache
            .entry((*path).to_owned())
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
