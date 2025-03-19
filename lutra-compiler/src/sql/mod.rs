mod clauses;
mod cr;
mod queries;
mod utils;

use crate::pr;
use lutra_bin::ir;

const COL_VALUE: &str = "value";
const COL_ARRAY_INDEX: &str = "index";

pub fn compile(project: &crate::Project, path: &pr::Path) -> (ir::Program, String) {
    // lower & inline
    let program = crate::intermediate::lower_var(&project.root_module, path);
    tracing::debug!("ir: {}", lutra_bin::ir::print(&program));
    let program = crate::intermediate::inline(program).unwrap();
    tracing::debug!("ir (inlined): {}", lutra_bin::ir::print(&program));
    let program = crate::intermediate::layouter::on_program(program);

    // compile to clauses
    let (clause, types) = clauses::compile(&program);

    tracing::debug!("cr: {clause:#?}");

    // compile to queries
    let query = queries::compile(clause, types);

    // serialize to SQL source
    let sql_source = query.to_string();

    (program, sql_source)
}
