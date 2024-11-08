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

#[track_caller]
pub fn _test_compile_ty(ty_source: &str) -> pr::Ty {
    let source = format!("type t = {ty_source}");

    let source = SourceTree::single("".into(), source);
    let project = compile(source, CompileParams {}).unwrap();

    let name = pr::Path::from_name("t");
    let type_def = project.root_module.module.get(&name);

    type_def.unwrap().kind.as_ty().unwrap().clone()
}
