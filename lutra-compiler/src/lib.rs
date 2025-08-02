mod bytecoding;
mod check;
mod diagnostic;
mod discover;
mod intermediate;
mod parser;
mod project;
mod resolver;
mod span;
mod sql;
mod utils;

type Result<T, E = diagnostic::Diagnostic> = core::result::Result<T, E>;

pub mod error;
pub mod pr;
pub mod printer;

pub use bytecoding::compile_program as bytecode_program;
pub use check::{CheckParams, check, check_overlay};
pub use discover::{DiscoverParams, discover};
pub use intermediate::{inline, layouter, lower_expr, lower_type_defs};
pub use lutra_bin::{ir, rr};
pub use project::{Project, SourceTree};
pub use span::Span;

#[derive(Debug, Clone)]
pub enum ProgramFormat {
    SqlPg,
    BytecodeLt,
}

pub fn compile(
    project: &Project,
    program: &str,
    name_hint: Option<&str>,
    format: ProgramFormat,
) -> Result<(rr::Program, rr::ProgramType), error::Error> {
    let program_pr = self::check::check_overlay(project, program, name_hint)?;
    let program_ir = crate::intermediate::lower_expr(project, &program_pr);

    tracing::debug!("ir:\n{}", lutra_bin::ir::print(&program_ir));

    let (program_ir, program) = match format {
        ProgramFormat::SqlPg => {
            let (program_ir, program_sr) = sql::compile_ir(program_ir);
            (program_ir, rr::Program::SqlPg(Box::new(program_sr)))
        }
        ProgramFormat::BytecodeLt => {
            let program_ir = intermediate::inline(program_ir);
            let program_ir = intermediate::layouter::on_program(program_ir);
            let inner = bytecoding::compile_program(program_ir.clone());
            (program_ir, rr::Program::BytecodeLt(inner))
        }
    };

    // construct the program ty
    let ir_func_ty = program_ir.main.ty.kind.into_function().unwrap();
    let ty = rr::ProgramType {
        input: ir_func_ty.params.into_iter().next().unwrap(),
        output: ir_func_ty.body,
        ty_defs: program_ir.types,
    };

    Ok((program, ty))
}

/// Internal implementation detail, do not use.
// Only exposed because I don't want to maintain a separate lexer for IR parser.
pub mod _lexer {
    pub use crate::diagnostic::Diagnostic;
    pub use crate::parser::lexer::{Token, TokenKind};

    pub use crate::parser::lexer::lex_source_recovery as lex;
}

#[track_caller]
pub fn _test_compile_ty(ty_source: &str) -> ir::Ty {
    let source = format!("type t: {ty_source}");

    let source = SourceTree::single("".into(), source);
    let project = check(source, CheckParams {}).unwrap_or_else(|e| panic!("{e}"));

    let name = pr::Path::from_name("t");
    let type_def = project.root_module.get(&name);

    let mut ty = type_def.unwrap().into_ty().unwrap().clone();
    ty.name = None;

    layouter::on_ty(ir::Ty::from(ty))
}

pub fn _test_compile_main(source: &str) -> Result<ir::Program, error::Error> {
    let source = SourceTree::single("".into(), source.to_string());
    let project = check(source, CheckParams {})?;

    let main = check_overlay(&project, "main", None)?;
    let program = lower_expr(&project, &main);
    Ok(layouter::on_program(program))
}
