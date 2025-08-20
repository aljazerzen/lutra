mod fold;
mod inliner;
pub mod layouter;
mod lowerer;

pub(super) use inliner::IdCounter;
pub use inliner::inline;
pub use lowerer::{lower_expr, lower_type_defs};
