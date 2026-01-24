mod clauses;
mod cr;
mod optimizer;
mod pg_repr;
mod queries;
mod serialization;
mod utils;

use lutra_bin::{ir, rr};

const COL_VALUE: &str = "value";
const COL_ARRAY_INDEX: &str = "index";

pub fn compile_ir(program: &ir::Program) -> rr::SqlProgram {
    // compile to clauses
    let (clauses, types) = clauses::compile(program);

    let clauses = optimizer::optimize(clauses);
    tracing::debug!("cr: {clauses:#?}");

    // compile to queries
    let query = queries::compile(clauses, types);
    tracing::trace!("sql ast: {query:?}");

    // serialize to SQL source
    let sql_source = query.to_string();
    tracing::debug!("sql:\n{query:#}");

    rr::SqlProgram {
        sql: sql_source,
        input_ty: program.get_input_ty().clone(),
        output_ty: program.get_output_ty().clone(),
        defs: program.defs.clone(),
    }
}
