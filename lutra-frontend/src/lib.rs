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

use error::{Diagnostic, WithErrorInfo};
use span::Span;

type Result<T, E = Diagnostic> = core::result::Result<T, E>;
