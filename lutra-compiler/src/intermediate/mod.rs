pub mod externals;
mod fold;
mod inliner;
mod ir_utils;
pub mod layouter;
pub mod lowerer;

pub use externals::validate_externals;
pub(super) use inliner::IdCounter;
pub use inliner::inline;
