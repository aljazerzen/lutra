use crate::Span;

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub code: DiagnosticCode,

    pub message: String,

    pub span: Option<Span>,

    pub additional: Vec<Additional>,
}

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

    /// Things that we know are not working correctly, but will eventually get fixed.
    #[allow(dead_code)]
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
}

#[derive(Debug, Clone)]
pub enum Severity {
    Warning,
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticCode(&'static str); // Maybe [std::ascii::Char; 5]?

impl DiagnosticCode {
    pub const CUSTOM: DiagnosticCode = DiagnosticCode("E0000");
    pub const BUG: DiagnosticCode = DiagnosticCode("E0001");
    pub const ASSERT: DiagnosticCode = DiagnosticCode("E0002");
    pub const PARSER: DiagnosticCode = DiagnosticCode("E0003");

    pub const NAME: DiagnosticCode = DiagnosticCode("E0004");
    pub const NAME_KIND: DiagnosticCode = DiagnosticCode("E0005");
    pub const TYPE: DiagnosticCode = DiagnosticCode("E0006");
    pub const TYPE_DOMAIN: DiagnosticCode = DiagnosticCode("E0007");

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
