use lutra_parser::span::Span;

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

    pub(crate) fn custom<S: ToString>(message: S) -> Self {
        Diagnostic::new(message, DiagnosticCode::CUSTOM)
    }

    pub(crate) fn bug() -> Self {
        Diagnostic::new("Internal bug. Please file an issue.", DiagnosticCode::BUG)
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

    pub(crate) fn from_prql(e: lutra_parser::error::Error) -> Self {
        let mut d =
            Diagnostic::new(e.reason.to_string(), DiagnosticCode::PARSER_ERROR).with_span(e.span);
        for hint in e.hints {
            d = d.with_additional(Additional {
                message: hint,
                span: None,
            });
        }
        d
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
    pub const PARSER_ERROR: DiagnosticCode = DiagnosticCode("E0002");

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
