mod clauses;
mod cr;
mod queries;
mod utils;

use lutra_bin::{ir, rr};

const COL_VALUE: &str = "value";
const COL_ARRAY_INDEX: &str = "index";

pub fn compile_ir(program: ir::Program) -> (ir::Program, rr::SqlProgram) {
    // intermediate optimizations
    let program = crate::intermediate::inline(program);
    tracing::debug!("ir (inlined): {}", lutra_bin::ir::print(&program));
    let program = crate::intermediate::layouter::on_program(program);

    // compile to clauses
    let (clause, types) = clauses::compile(&program);

    tracing::debug!("cr: {clause:#?}");

    // compile to queries
    let query = queries::compile(clause, types);

    tracing::trace!("sql ast: {query:?}");

    // serialize to SQL source
    let sql_source = query.to_string();

    if tracing::enabled!(tracing::Level::DEBUG) {
        // format sql
        #[cfg(feature = "debug")]
        {
            let options = sqlformat::FormatOptions::default();
            let formatted_sql =
                sqlformat::format(&sql_source, &sqlformat::QueryParams::None, &options);
            tracing::debug!("sql:\n{formatted_sql}");
        };
        #[cfg(not(feature = "debug"))]
        tracing::debug!("sql:\n{sql_source}");
    }

    let program_sr = rr::SqlProgram {
        sql: sql_source,
        input_ty: program.get_input_ty().clone(),
        output_ty: program.get_output_ty().clone(),
        types: program.types.clone(),
    };
    (program, program_sr)
}
