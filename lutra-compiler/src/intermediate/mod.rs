mod fold;
mod inliner;
mod lowering;

pub use inliner::inline;
pub use lowering::lower;
