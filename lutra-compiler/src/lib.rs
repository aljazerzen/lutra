mod bytecoding;
mod compile;
mod diagnostic;
mod discover;
mod intermediate;
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
pub use intermediate::{layouter, lower_expr, lower_type_defs, lower_var};
pub use project::{Project, SourceTree};
use resolver::resolve_post;
pub use span::Span;
pub use sql::compile as compile_to_sql;

use lutra_bin::ir;

pub mod _lexer {
    pub use crate::diagnostic::Diagnostic;
    pub use crate::parser::lexer::{Token, TokenKind};

    pub use crate::parser::lexer::lex_source_recovery as lex;
}

#[track_caller]
pub fn _test_compile_ty(ty_source: &str) -> ir::Ty {
    let source = format!("type t = {ty_source}");

    let source = SourceTree::single("".into(), source);
    let project = compile(source, CompileParams {}).unwrap_or_else(|e| panic!("{e}"));

    let name = pr::Path::from_name("t");
    let type_def = project.root_module.module.get(&name);

    let mut ty = type_def.unwrap().kind.as_ty().unwrap().clone();
    ty.name = None;

    layouter::on_ty(ir::Ty::from(ty))
}

pub fn _test_compile(source: &str) -> Result<ir::Program, error::Error> {
    let source = SourceTree::single("".into(), source.to_string());
    let project = compile(source, CompileParams {})?;

    let path = pr::Path::from_name("main");

    let program = lower_var(&project.root_module, &path);
    Ok(layouter::on_program(program))
}

pub fn _lower_expr(project: &Project, source: &str) -> Result<ir::Program, error::Error> {
    let (expr, _diagnostics) = parser::parse_expr(source, 0);
    let expr = expr.unwrap();

    let expr = resolve_post(&project.root_module, expr).unwrap();

    Ok(lower_expr(&project.root_module, &expr))
}
