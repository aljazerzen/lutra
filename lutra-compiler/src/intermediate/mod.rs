mod fold;
mod inliner;
mod lowering;

pub use inliner::inline;
pub use lowering::{lower_expr, lower_type_defs, lower_var};
