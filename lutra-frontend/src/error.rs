use crate::Span;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("io error")]
    Io(#[from] std::io::Error),

    #[error("invalid source structure: {problem}")]
    InvalidSourceStructure { problem: String },

    #[error("invalid source code")]
    InvalidSource { diagnostics: Vec<Diagnostic> },
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub code: DiagnosticCode,

    pub message: String,

    pub span: Option<Span>,

    pub additional: Vec<Additional>,
}

#[allow(dead_code)]
impl Diagnostic {
    pub(crate) fn new<S: ToString>(message: S, code: DiagnosticCode) -> Self {
        Diagnostic {
            code,
            message: message.to_string(),
            span: None,
            additional: vec![],
        }
    }

    pub(crate) fn new_custom<S: ToString>(message: S) -> Self {
        Diagnostic::new(message, DiagnosticCode::CUSTOM)
    }

    pub(crate) fn new_simple<S: ToString>(message: S) -> Self {
        Diagnostic::new(message, DiagnosticCode::CUSTOM)
    }

    /// Things that we know are not working correctly, but will eventually get fixed.
    pub(crate) fn new_bug(issue_no: u32) -> Self {
        Diagnostic::new("Internal bug", DiagnosticCode::BUG)
            .push_hint(format!("Tracked under number {issue_no}"))
    }

    /// Things that you *think* should never happen, but are not sure.
    pub(crate) fn new_assert<S: Into<String>>(message: S) -> Self {
        Diagnostic::new("Internal bug. Please file an issue.", DiagnosticCode::BUG)
            .push_hint(message)
    }

    pub(crate) fn with_span(mut self, span: Option<Span>) -> Self {
        self.span = span.or(self.span);
        self
    }

    pub(crate) fn with_span_fallback(mut self, span: Option<Span>) -> Self {
        self.span = self.span.or(span);
        self
    }

    pub(crate) fn with_additional(mut self, additional: Additional) -> Self {
        self.additional.push(additional);
        self
    }

    pub(crate) fn into_error(self) -> Error {
        Error::InvalidSource {
            diagnostics: vec![self],
        }
    }
}

#[derive(Debug, Clone)]
pub enum Severity {
    Warning,
    Error,
}

#[derive(Debug, Clone)]
pub struct DiagnosticCode(&'static str); // Maybe [std::ascii::Char; 5]?

impl DiagnosticCode {
    pub const CUSTOM: DiagnosticCode = DiagnosticCode("E0000");
    pub const BUG: DiagnosticCode = DiagnosticCode("E0001");
    pub const ASSERT: DiagnosticCode = DiagnosticCode("E0002");
    pub const PARSER: DiagnosticCode = DiagnosticCode("E0003");

    pub fn get(&self) -> &'static str {
        self.0
    }

    pub const fn get_severity(&self) -> Severity {
        match self.0.as_bytes()[0] {
            b'E' => Severity::Error,
            b'W' => Severity::Warning,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Additional {
    pub message: String,

    pub span: Option<Span>,
}

pub trait WithErrorInfo: Sized {
    fn push_hint<S: Into<String>>(self, hint: S) -> Self;

    fn with_span(self, span: Option<Span>) -> Self;

    fn with_span_fallback(self, span: Option<Span>) -> Self;
}

impl WithErrorInfo for Diagnostic {
    fn push_hint<S: Into<String>>(mut self, hint: S) -> Self {
        self.additional.push(Additional {
            message: hint.into(),
            span: None,
        });
        self
    }

    fn with_span(mut self, span: Option<Span>) -> Self {
        self.span = span;
        self
    }

    fn with_span_fallback(mut self, span: Option<Span>) -> Self {
        self.span = self.span.or(span);
        self
    }
}

impl<T, E: WithErrorInfo> WithErrorInfo for Result<T, E> {
    fn push_hint<S: Into<String>>(self, hint: S) -> Self {
        self.map_err(|e| e.push_hint(hint))
    }

    fn with_span(self, span: Option<Span>) -> Self {
        self.map_err(|e| e.with_span(span))
    }

    fn with_span_fallback(self, span: Option<Span>) -> Self {
        self.map_err(|e| e.with_span_fallback(span))
    }
}
