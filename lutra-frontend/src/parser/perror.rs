use core::fmt;
use std::collections::HashSet;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;

use crate::diagnostic::{Diagnostic, DiagnosticCode};
use crate::parser::lexer;

use crate::span::Span;

#[derive(Clone, Debug)]
pub struct ChumError<T: Hash + Eq + Debug> {
    span: Span,
    message: Option<String>,
    expected: HashSet<Option<T>>,
    found: Option<T>,
    label: SimpleLabel,
}

pub type PError = ChumError<lexer::TokenKind>;

impl<T: Hash + Eq + Debug> ChumError<T> {
    ///Create an error with a custom error message.
    pub fn custom<M: ToString>(span: Span, msg: M) -> Self {
        Self {
            span,
            message: Some(msg.to_string()),
            expected: HashSet::default(),
            found: None,
            label: SimpleLabel::None,
        }
    }

    /// Returns the span that the error occurred at.
    pub fn span(&self) -> Span {
        self.span
    }

    /// Returns an iterator over possible expected patterns.
    pub fn expected(&self) -> impl ExactSizeIterator<Item = &Option<T>> + '_ {
        self.expected.iter()
    }

    /// Returns the input, if any, that was found instead of an expected pattern.
    pub fn found(&self) -> Option<&T> {
        self.found.as_ref()
    }

    /// Returns the reason for the error.
    pub fn reason(&self) -> &Option<String> {
        &self.message
    }

    /// Returns the error's label, if any.
    pub fn label(&self) -> Option<&'static str> {
        self.label.into()
    }

    /// Map the error's inputs using the given function.
    ///
    /// This can be used to unify the errors between parsing stages that operate upon two forms of input (for example,
    /// the initial lexing stage and the parsing stage in most compilers).
    pub fn map<U: Hash + Eq + Debug, F: FnMut(T) -> U>(self, mut f: F) -> ChumError<U> {
        ChumError {
            span: self.span,
            message: self.message,
            expected: self.expected.into_iter().map(|e| e.map(&mut f)).collect(),
            found: self.found.map(f),
            label: self.label,
        }
    }
}

impl<T: Hash + Eq + Display + Debug> chumsky::Error<T> for ChumError<T> {
    type Span = Span;
    type Label = &'static str;

    fn expected_input_found<Iter: IntoIterator<Item = Option<T>>>(
        span: Span,
        expected: Iter,
        found: Option<T>,
    ) -> Self {
        let exp = expected.into_iter().collect();
        log::trace!("looking for {:?} but found {:?} at: {:?}", exp, found, span);
        Self {
            span,
            // reason: Some(String::from("unexpected")),
            message: None,
            expected: exp,
            found,
            label: SimpleLabel::None,
        }
    }

    fn unclosed_delimiter(
        unclosed_span: Self::Span,
        delimiter: T,
        span: Self::Span,
        expected: T,
        found: Option<T>,
    ) -> Self {
        Self {
            span,
            message: Some(format!(
                "unclosed delimiter: {delimiter} within span {}..{}",
                unclosed_span.start, unclosed_span.end
            )),
            expected: core::iter::once(Some(expected)).collect(),
            found,
            label: SimpleLabel::None,
        }
    }

    fn with_label(mut self, label: Self::Label) -> Self {
        match self.label {
            SimpleLabel::Some(_) => {}
            _ => {
                self.label = SimpleLabel::Some(label);
            }
        }
        self
    }

    ///from chumsky::error::Simple
    fn merge(mut self, other: Self) -> Self {
        // TODO: Assert that `self.span == other.span` here?
        // self.reason = match (&self.reason, &other.reason) {
        //     (Some(..), None) => self.reason,
        //     (None, Some(..) ) => other.reason,
        //     (Some(mut r1), Some(r2)) => {r1.push('s');Some(r1)},
        // };

        self.message = self.message.zip(other.message).map(|(mut r1, r2)| {
            r1.push_str(" | ");
            r1.push_str(r2.as_str());
            r1
        });

        self.label = self.label.merge(other.label);
        self.expected.extend(other.expected);
        self
    }
}

impl<T: Hash + Eq + Debug> PartialEq for ChumError<T> {
    fn eq(&self, other: &Self) -> bool {
        self.span == other.span
            && self.found == other.found
            && self.message == other.message
            && self.expected == other.expected
            && self.label == other.label
    }
}
impl<T: Hash + Eq + Debug> Eq for ChumError<T> {}

impl<T: fmt::Display + Hash + Eq + Debug> fmt::Display for ChumError<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO: Take `self.reason` into account

        if let Some(found) = &self.found {
            write!(f, "found {:?}", found.to_string())?;
        } else {
            write!(f, "found end of input")?;
        };

        match self.expected.len() {
            0 => {} //write!(f, " but end of input was expected")?,
            1 => write!(
                f,
                " but expected {}",
                match self.expected.iter().next().unwrap() {
                    Some(x) => format!("{:?}", x.to_string()),
                    None => "end of input".to_string(),
                },
            )?,
            _ => {
                write!(
                    f,
                    " but expected one of {}",
                    self.expected
                        .iter()
                        .map(|expected| match expected {
                            Some(x) => format!("{:?}", x.to_string()),
                            None => "end of input".to_string(),
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                )?;
            }
        }

        Ok(())
    }
}

impl From<PError> for Diagnostic {
    fn from(p: PError) -> Diagnostic {
        let span = p.span();

        fn construct_diagnostic(e: PError) -> Diagnostic {
            if let Some(message) = e.reason() {
                return Diagnostic::new(message, DiagnosticCode::PARSER);
            }

            fn token_to_string(t: Option<lexer::TokenKind>) -> String {
                t.as_ref()
                    .map(lexer::TokenKind::to_string)
                    .unwrap_or_else(|| "end of input".to_string())
            }

            let is_all_whitespace = e
                .expected()
                .all(|t| matches!(t, None | Some(lexer::TokenKind::NewLine)));
            let expected: Vec<String> = e
                .expected()
                // Only include whitespace if we're _only_ expecting whitespace,
                // otherwise we get "expected start of line or new line or }"
                // when there's no ending `}`, since a new line is allowed.
                .filter(|t| {
                    is_all_whitespace
                        || !matches!(
                            t,
                            None | Some(lexer::TokenKind::NewLine) | Some(lexer::TokenKind::Start)
                        )
                })
                .cloned()
                .map(token_to_string)
                .collect();

            let while_parsing = e
                .label()
                .map(|l| format!(" while parsing {l}"))
                .unwrap_or_default();

            if expected.is_empty() || expected.len() > 10 {
                let label = token_to_string(e.found().cloned());
                return Diagnostic::new(
                    format!("unexpected {label}{while_parsing}"),
                    DiagnosticCode::PARSER,
                );
            }

            let mut expected = expected;
            expected.sort();

            let expected = match expected.len() {
                1 => expected.remove(0),
                2 => expected.join(" or "),
                _ => {
                    let last = expected.pop().unwrap();
                    format!("one of {} or {last}", expected.join(", "))
                }
            };

            let who = e.label().map(|x| format!("{x} ")).unwrap_or_default();

            let message = match e.found() {
                Some(found) => format!("{who}expected {expected}, but found {found}"),
                None => format!("{who}expected {expected}, but encountered the end of the file."),
            };
            Diagnostic::new(message, DiagnosticCode::PARSER)
        }

        construct_diagnostic(p).with_span(Some(span))
    }
}

// Vendored from
// https://github.com/zesterer/chumsky/pull/238/files#diff-97e25e2a0e41c578875856e97b659be2719a65227c104b992e3144efa000c35eR184
// since it's private in chumsky

/// A type representing zero, one, or many labels applied to an error
#[derive(Clone, Copy, Debug, PartialEq)]
enum SimpleLabel {
    Some(&'static str),
    None,
    Multi,
}

impl SimpleLabel {
    fn merge(self, other: Self) -> Self {
        match (self, other) {
            (SimpleLabel::Some(a), SimpleLabel::Some(b)) if a == b => SimpleLabel::Some(a),
            (SimpleLabel::Some(_), SimpleLabel::Some(_)) => SimpleLabel::Multi,
            (SimpleLabel::Multi, _) => SimpleLabel::Multi,
            (_, SimpleLabel::Multi) => SimpleLabel::Multi,
            (SimpleLabel::None, x) => x,
            (x, SimpleLabel::None) => x,
        }
    }
}

impl From<SimpleLabel> for Option<&'static str> {
    fn from(label: SimpleLabel) -> Self {
        match label {
            SimpleLabel::Some(s) => Some(s),
            _ => None,
        }
    }
}
