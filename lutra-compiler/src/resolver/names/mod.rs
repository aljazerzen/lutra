mod expr;
mod module;
mod scope;

use std::collections::HashSet;

use itertools::Itertools;
use scope::Scope;

use crate::Result;
use crate::diagnostic::Diagnostic;
use crate::{pr, utils};

type TargetSpanMap = Vec<(crate::Span, crate::project::TargetSpan)>;

/// Resolves names
///
/// Returns a resolution order and the collected target map entries.
pub fn run(
    root: &mut pr::ModuleDef,
    unresolved: HashSet<pr::Path>,
    is_std: bool,
) -> Result<(Vec<Vec<pr::Path>>, TargetSpanMap)> {
    // resolve inter-definition references
    let (refs_tys, refs_vars, target_spans) = {
        let mut r = module::DefNameResolver {
            root,
            is_std,
            unresolved,
            refs_tys: Default::default(),
            refs_vars: Default::default(),
            current: pr::Path::empty(),
            scope_id_gen: Default::default(),
            target_spans: Vec::new(),
        };
        r.run()?;
        (r.refs_tys, r.refs_vars, r.target_spans)
    };

    // toposort tys
    let order_tys = utils::toposort::<pr::Path>(&refs_tys);

    // toposort vars
    let order_vars = utils::toposort::<pr::Path>(&refs_vars);
    let has_var_cycles = order_vars.iter().any(|scc| scc.len() != 1);

    if has_var_cycles {
        let scc = order_vars.iter().find(|scc| scc.len() != 1).unwrap();
        let (def, _) = root.try_get(scc[0].as_steps()).unwrap();
        return Err(
            Diagnostic::new_custom("unimplemented cyclic references between expressions")
                .with_span(def.span),
        );
    }

    let resolution_order = itertools::chain(order_tys, order_vars)
        .map(|tree| tree.iter().map(|p| (*p).clone()).collect_vec())
        .collect_vec();

    Ok((resolution_order, target_spans))
}
