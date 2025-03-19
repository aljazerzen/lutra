mod fold;
mod inliner;
pub mod layouter;
mod lowering;

pub use inliner::inline;
pub use lowering::{lower_expr, lower_type_defs, lower_var};
