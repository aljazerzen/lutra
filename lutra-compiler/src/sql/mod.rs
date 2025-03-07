mod clauses;
mod cr;
mod queries;
mod utils;

use lutra_bin::ir;

const COL_VALUE: &str = "value";
const COL_ARRAY_INDEX: &str = "index";

pub fn compile(program: ir::Program) -> String {
    let program = crate::intermediate::inline(program).unwrap();

    let clause = clauses::compile(&program);

    tracing::debug!("cr: {clause:#?}");

    let query = queries::compile(clause);

    query.to_string()
}
