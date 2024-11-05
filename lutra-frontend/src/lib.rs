mod compile;
mod discover;
mod error;
mod ir;
mod project;
mod semantic;
mod utils;

pub use compile::{compile, CompileParams};
pub use discover::{discover, DiscoverParams};
pub use project::Project;

use lutra_parser::pr;

use lutra_parser::error::{Error, Reason, WithErrorInfo};
use lutra_parser::span::Span;

type Result<T, E = Error> = core::result::Result<T, E>;
