mod bytecoding;
mod compile;
mod diagnostic;
mod discover;
mod lowering;
mod parser;
mod printer;
mod project;
mod resolver;
mod span;
mod sql;
mod utils;

type Result<T, E = diagnostic::Diagnostic> = core::result::Result<T, E>;

pub mod decl;
pub mod error;
pub mod pr;
pub use bytecoding::compile_program as bytecode_program;
pub use compile::{compile, CompileParams};
pub use discover::{discover, DiscoverParams};
pub use lowering::lower;
pub use project::{Project, SourceTree};
pub use span::Span;
pub use sql::compile as compile_to_sql;

use lutra_bin::ir;

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

    let mut ty = type_def.unwrap().kind.as_ty().unwrap().clone();
    ty.name = None;
    ty
}

pub fn _test_compile(source: &str) -> Result<ir::Program, error::Error> {
    let source = SourceTree::single("".into(), source.to_string());
    let project = compile(source, CompileParams {})?;

    let path = pr::Path::from_name("main");
    Ok(lowering::lower(&project.root_module, &path))
}
