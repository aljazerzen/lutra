use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::pr;
use crate::utils::toposort;
use crate::{Project, Result};

/// Checks project for bad recursive type references
/// and computes [pr::Ty::variants_force_ptr].
// #[tracing::instrument(skip_all, name = "recursive_check")]
pub fn check(mut project: Project) -> Result<Project, Vec<Diagnostic>> {
    let mut diagnostics = Vec::new();

    for group in &project.ordering {
        // task 1: mark all potentially recursive enum variants
        for ident in group {
            let def = project.root_module.get_mut(ident).unwrap();
            let pr::DefKind::Ty(ty_def) = &mut def.kind else {
                continue;
            };
            mark_recursive(&mut ty_def.ty, group);
        }

        // task2: find bad recursive references

        // collect refs between group members
        // (these refs include only refs that affect type layout)
        let mut deps = Vec::with_capacity(group.len());
        for ident in group {
            let def = project.root_module.get(ident).unwrap();
            let pr::DefKind::Ty(ty_def) = &def.kind else {
                continue;
            };

            let mut refs = Vec::new();
            collect_refs(&ty_def.ty, &mut refs);

            if refs.contains(ident) {
                diagnostics.push(
                    Diagnostic::new_custom("type has infinite size")
                        .with_span(def.span)
                        .push_hint("self references are allowed only from within arrays or enums"),
                );
            }

            deps.push((ident.clone(), refs));
        }

        let order = toposort(&deps);

        for subgroup in order {
            if subgroup.len() == 1 {
                continue;
            }
            let def = project.root_module.get(subgroup[0]).unwrap();
            diagnostics.push(
                Diagnostic::new_custom("type has infinite size")
                    .with_span(def.span)
                    .push_hint("recursive references are allowed only from within arrays or enums"),
            );
        }
    }

    if !diagnostics.is_empty() {
        Err(diagnostics)
    } else {
        Ok(project)
    }
}

/// Returns true for types whose layout depends on layout of some type-def from current group.
fn mark_recursive(ty: &mut pr::Ty, group: &[pr::Path]) -> bool {
    match &mut ty.kind {
        pr::TyKind::Ident(_) => {
            let target = ty.target.as_ref().unwrap();
            let pr::Ref::Global(fq) = target else {
                return false;
            };

            group.contains(fq)
        }
        pr::TyKind::Primitive(_) => false,
        pr::TyKind::Tuple(fields) => {
            // tuples don't have pointers, so they propagate is_recursive status
            let mut is_recursive = false;
            for f in fields {
                if mark_recursive(&mut f.ty, group) {
                    is_recursive = true;
                }
            }
            is_recursive
        }
        pr::TyKind::Array(_) => {
            // arrays have an pointer, so they are never recursive
            false
        }
        pr::TyKind::Enum(variants) => {
            // enums: the interesting part

            for (i, variant) in variants.iter_mut().enumerate() {
                let is_recursive = mark_recursive(&mut variant.ty, group);

                if is_recursive {
                    ty.variants_force_ptr.push(i as u16);
                }
            }

            false
        }
        pr::TyKind::Func(_) => false,
        pr::TyKind::TupleComprehension(_) => false, // ???
    }
}

fn collect_refs(ty: &pr::Ty, refs: &mut Vec<pr::Path>) {
    match &ty.kind {
        pr::TyKind::Ident(_) => {
            let target = ty.target.as_ref().unwrap();
            let pr::Ref::Global(fq) = target else {
                return;
            };

            refs.push(fq.clone());
        }
        pr::TyKind::Primitive(_) => {}
        pr::TyKind::Tuple(fields) => {
            // tuples don't have pointers, so they always depend on layout of inner type
            for f in fields {
                collect_refs(&f.ty, refs);
            }
        }
        pr::TyKind::Array(_) => {
            // arrays have an pointer, so their layout never depends on the inner type
        }
        pr::TyKind::Enum(_) => {
            // enums have a pointer (if we will determine it is needed)
        }
        pr::TyKind::Func(_) => {}
        pr::TyKind::TupleComprehension(_) => {} // ???
    }
}
