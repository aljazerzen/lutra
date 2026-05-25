mod bytecoding;
mod check;
mod diagnostic;
mod discover;
mod format;
mod intermediate;
mod parser;
mod project;
mod resolver;
mod sql;
mod utils;

type Result<T, E = diagnostic::Diagnostic> = core::result::Result<T, E>;

pub mod codespan;
pub mod error;
pub mod pr;
pub mod printer;

pub use bytecoding::compile_program as bytecode_program;
pub use check::{CheckParams, check, check_overlay, std_source};
pub use codespan::Span;
pub use discover::{DiscoverParams, discover};
pub use format::format;
pub use parser::parse_path;
pub use project::{Project, SourceTree, SymbolInfo};

pub use lutra_bin::{ir, rr};

/// The representation kind of a compiled Lutra program.
#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "clap", derive(clap::ValueEnum))]
pub enum ProgramRepr {
    SqlPg,
    SqlDuckdb,
    BytecodeLt,
}

/// The representation kind of a compiled Lutra program.
#[derive(Clone)]
#[cfg_attr(feature = "clap", derive(clap::Parser))]
pub struct CompileParams {
    /// Program source code.
    ///
    /// Usually this is a path to an expression in the project, but it can be
    /// any lutra expression.
    #[cfg_attr(feature = "clap", clap(long, default_value = "main"))]
    program: String,

    /// The name of the program that should be displayed in diagnostics.
    /// For example, when called by cli, this would be `"--program"`.
    #[cfg_attr(feature = "clap", clap(skip))]
    program_name_hint: Option<String>,

    /// Target program representation.
    #[cfg_attr(feature = "clap", clap(long, default_value = "bytecode-lt"))]
    repr: ProgramRepr,

    /// Externals that are allowed to be used by the program.
    ///
    /// Contains fully-qualified paths to the external functions or modules.
    /// When a module is referenced, all contained external functions are allowed.
    #[cfg_attr(feature = "clap", clap(skip))]
    externals: Vec<std::borrow::Cow<'static, str>>,
}

pub fn compile(
    project: &Project,
    params: &CompileParams,
) -> Result<(rr::Program, rr::ProgramType), error::Error> {
    // resolve
    let program_pr = self::check::check_overlay(
        project,
        &params.program,
        params.program_name_hint.as_deref(),
    )?;

    // lower
    let program_ir = intermediate::lowerer::lower_expr(project, &program_pr);
    tracing::debug!("ir:\n{}\n", lutra_bin::ir::print_no_color(&program_ir));

    let program_ir = intermediate::validate_externals(program_ir, &params.externals)?;

    // intermediate optimizations
    let program_ir = intermediate::inline(program_ir);
    tracing::debug!(
        "ir (inlined):\n{}\n",
        lutra_bin::ir::print_no_color(&program_ir)
    );
    let program_ir = intermediate::layouter::on_program(program_ir);

    // backend (sql or bytecode)
    let program = match params.repr {
        ProgramRepr::SqlPg => rr::Program::SqlPostgres(Box::new(sql::compile_ir(
            &program_ir,
            sql::Dialect::Postgres,
        ))),
        ProgramRepr::SqlDuckdb => {
            rr::Program::SqlDuckDB(Box::new(sql::compile_ir(&program_ir, sql::Dialect::DuckDB)))
        }
        ProgramRepr::BytecodeLt => {
            rr::Program::BytecodeLt(bytecoding::compile_program(program_ir.clone()))
        }
    };

    // construct the program ty
    let ir_func_ty = program_ir.main.ty.kind.into_function().unwrap();
    let ty = rr::ProgramType {
        input: ir_func_ty.params.into_iter().next().unwrap(),
        output: ir_func_ty.body,
        defs: program_ir.defs,
    };

    Ok((program, ty))
}

pub fn project_to_types(project: &Project) -> ir::Module {
    let module = intermediate::lowerer::lower_type_defs(project);

    intermediate::layouter::on_root_module(module)
}

/// Internal implementation detail, do not use.
// Only exposed because I don't want to maintain a separate lexer for IR parser.
#[doc(hidden)]
pub mod _lexer {
    pub use crate::diagnostic::Diagnostic;
    pub use crate::parser::{Token, TokenKind, lex_source_recovery as lex};
}

/// Internal implementation detail, do not use.
// Exposed for benchmarks.
#[doc(hidden)]
pub mod _bench {
    pub use crate::diagnostic::Diagnostic;
    pub use crate::parser::{parse_expr, parse_source};
}

#[track_caller]
pub fn _test_compile_ty(ty_source: &str) -> (ir::Ty, Vec<ir::TyDef>) {
    let source = SourceTree::empty();
    let project = check(source, Default::default()).unwrap_or_else(|e| panic!("{e}"));

    let program = format!("func (x: {ty_source}) -> x");
    let program = check_overlay(&project, &program, None).unwrap();
    let program = intermediate::lowerer::lower_expr(&project, &program);
    let program = intermediate::layouter::on_program(program);
    (program.get_input_ty().clone(), program.defs)
}

#[doc(hidden)]
pub fn _test_compile_main(source: &str) -> Result<ir::Program, error::Error> {
    let source = SourceTree::single("".into(), source.to_string());
    let project = check(source, Default::default())?;
    _test_compile_main_in(&project)
}

#[doc(hidden)]
pub fn _test_compile_main_in(project: &Project) -> Result<ir::Program, error::Error> {
    let main = check_overlay(project, "main", None)?;
    let program = intermediate::lowerer::lower_expr(project, &main);
    Ok(intermediate::layouter::on_program(program))
}

#[doc(hidden)]
pub fn _test_inline(program: ir::Program) -> ir::Program {
    intermediate::inline(program)
}

#[doc(hidden)]
pub fn _test_layout(program: ir::Program) -> ir::Program {
    intermediate::layouter::on_program(program)
}

impl std::fmt::Display for ProgramRepr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SqlPg => write!(f, "sql-pg"),
            Self::SqlDuckdb => write!(f, "sql-duckdb"),
            Self::BytecodeLt => write!(f, "bytecode-lt"),
        }
    }
}

impl ProgramRepr {
    fn get_implicit_externals(&self) -> Vec<std::borrow::Cow<'static, str>> {
        match self {
            ProgramRepr::SqlPg => vec!["std::sql".into()],
            ProgramRepr::SqlDuckdb => vec!["std::sql".into(), "std::fs".into()],
            ProgramRepr::BytecodeLt => vec![],
        }
    }

    /// The tag string used in `get_externals()` to identify this repr.
    pub fn tag(&self) -> &'static str {
        match self {
            ProgramRepr::SqlPg => "repr:sql-pg",
            ProgramRepr::SqlDuckdb => "repr:sql-duckdb",
            ProgramRepr::BytecodeLt => "repr:bytecode-lt",
        }
    }

    /// Extract the program repr from a list of externals.
    ///
    /// Looks for a `repr:` prefixed tag. Returns `None` if no repr tag is found.
    pub fn from_externals<S: AsRef<str>>(externals: &[S]) -> Option<Self> {
        for ext in externals {
            match ext.as_ref() {
                "repr:sql-pg" | "repr:sql-postgres" => return Some(ProgramRepr::SqlPg),
                "repr:sql-duckdb" => return Some(ProgramRepr::SqlDuckdb),
                "repr:bytecode-lt" => return Some(ProgramRepr::BytecodeLt),
                _ => {}
            }
        }
        None
    }
}

impl CompileParams {
    pub fn new(program: impl Into<String>, repr: ProgramRepr) -> Self {
        Self {
            program: program.into(),
            program_name_hint: None,
            repr,
            externals: repr.get_implicit_externals(),
        }
    }

    /// Create compile params from a program name and runner-provided externals.
    ///
    /// The externals list should contain a `repr:` tag (e.g. `repr:sql-pg`)
    /// that determines the compilation target.
    pub fn from_externals<S: AsRef<str>>(
        program: impl Into<String>,
        externals: &[S],
    ) -> Result<Self, String> {
        let repr = ProgramRepr::from_externals(externals).ok_or_else(|| {
            "runner did not provide a program repr tag in get_externals()".to_string()
        })?;
        let externals: Vec<std::borrow::Cow<'static, str>> = externals
            .iter()
            .map(|s| std::borrow::Cow::Owned(s.as_ref().to_string()))
            .collect();
        Ok(Self {
            program: program.into(),
            program_name_hint: None,
            repr,
            externals,
        })
    }

    pub fn with_program_name_hint(mut self, hint: impl Into<String>) -> Self {
        self.program_name_hint = Some(hint.into());
        self
    }

    pub fn with_externals<I, T>(mut self, externals: I) -> Self
    where
        I: IntoIterator<Item = T>,
        T: Into<std::borrow::Cow<'static, str>>,
    {
        self.externals
            .extend(externals.into_iter().map(|x| x.into()));
        self
    }

    pub fn with_external(mut self, external: impl Into<std::borrow::Cow<'static, str>>) -> Self {
        self.externals.push(external.into());
        self
    }

    pub fn repr(&self) -> ProgramRepr {
        self.repr
    }
}
