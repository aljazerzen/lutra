mod compile;
mod discover;
mod error;
mod parser;
mod project;
mod semantic;
mod diagnostic;
mod span;
mod utils;

type Result<T, E = diagnostic::Diagnostic> = core::result::Result<T, E>;

pub mod decl;
pub mod pr;
pub use compile::{compile, CompileParams};
pub use discover::{discover, DiscoverParams};
pub use project::{Project, SourceTree};
pub use span::Span;

pub mod _lexer {
    pub use crate::diagnostic::Diagnostic;
    pub use crate::parser::lexer::{Token, TokenKind};

    pub use crate::parser::lexer::lex_source_recovery as lex;
}

#[track_caller]
pub fn _test_compile_ty(ty_source: &str) -> pr::Ty {
    let source = format!("type t = {ty_source}");

    let source = SourceTree::single("".into(), source);
    let project = compile(source, CompileParams {}).unwrap_or_else(|e| panic!("{e}"));

    let name = pr::Path::from_name("t");
    let type_def = project.root_module.module.get(&name);

    type_def.unwrap().kind.as_ty().unwrap().clone()
}
