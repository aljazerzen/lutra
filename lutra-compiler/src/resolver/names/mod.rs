mod expr;
mod module;
mod scope;

use itertools::Itertools;
use scope::Scope;

use crate::Result;
use crate::diagnostic::Diagnostic;
use crate::{pr, utils};

/// Runs name resolution for global names - names that refer to definitions.
///
/// Keeps track of all inter-definition references.
/// Returns a resolution order.
pub fn run(root: &mut pr::ModuleDef) -> Result<Vec<Vec<pr::Path>>> {
    // resolve inter-definition references
    let (refs_tys, refs_vars) = {
        let mut r = module::ModuleRefResolver {
            root,
            refs_tys: Default::default(),
            refs_vars: Default::default(),
            current_path: pr::Path::empty(),
            scope_id_gen: Default::default(),
        };
        r.run()?;
        (r.refs_tys, r.refs_vars)
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

    Ok(itertools::chain(order_tys, order_vars)
        .map(|tree| tree.iter().map(|p| (*p).clone()).collect_vec())
        .collect_vec())
}
