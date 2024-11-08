mod compile;
mod discover;
mod error;
mod ir;
mod parser;
pub mod pr;
mod project;
mod semantic;
mod span;
mod utils;

pub use compile::{compile, CompileParams};
pub use discover::{discover, DiscoverParams};
pub use project::{Project, SourceTree};

pub use ir::decl;

pub use span::Span;

type Result<T, E = error::Diagnostic> = core::result::Result<T, E>;

pub mod _lexer {
    pub use crate::error::Diagnostic;
    pub use crate::parser::lexer::{Token, TokenKind};

    pub use crate::parser::lexer::lex_source_recovery as lex;
}
