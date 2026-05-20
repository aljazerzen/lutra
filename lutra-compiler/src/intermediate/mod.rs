pub mod externals;
mod fold;
mod inliner;
pub mod layouter;
pub mod lowerer;

pub use externals::validate_externals;
pub(super) use inliner::IdCounter;
pub use inliner::inline;
