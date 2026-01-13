use std::collections::HashMap;

use crate::Result;
use crate::diagnostic::Diagnostic;
use crate::intermediate::fold::{IrFold, fold_ty};
use crate::utils::toposort;
use indexmap::IndexMap;
use itertools::Itertools;
use lutra_bin::ir;

#[tracing::instrument(skip_all, name = "layouter")]
pub fn on_program(mut program: ir::Program) -> ir::Program {
    tracing::trace!(
        "types: {:?}",
        program.defs.iter().map(|x| &x.name).collect_vec()
    );

    let mut l = Layouter::default();

    // compute layout for each of program.defs
    program.defs = order_ty_defs(program.defs);
    program.defs = l.compute_ty_defs_layout(program.defs).unwrap();

    // compute layout for the main expr
    program.main = l.fold_expr(program.main).unwrap();

    program
}

#[tracing::instrument(skip_all, name = "layouter")]
pub fn on_root_module(root_module: ir::Module) -> ir::Module {
    // this function is a bit awkward, because ir::Module is hierarchial,
    // but we want to iterate over all types and then vars.

    let mut l = Layouter::default();

    // separate project out into var and type defs
    let mut vars = Vec::new();
    let mut types = Vec::new();
    for (path, def) in root_module.iter_defs_re() {
        match def {
            ir::Decl::Module(_) => unreachable!(),
            ir::Decl::Type(ty) => {
                let def = ir::TyDef {
                    name: path,
                    ty: ty.clone(),
                };
                types.push(def);
            }
            ir::Decl::Var(ty) => {
                vars.push((path, ty.clone()));
            }
        }
    }

    // compute layout for types
    let types = order_ty_defs(types);
    let types = l.compute_ty_defs_layout(types).unwrap();

    // compute layout for vars (and place back into result module)
    let mut result = ir::Module { decls: Vec::new() };
    for (path, var) in vars {
        let var = l.fold_ty(var).unwrap();

        result.insert(&path.0, ir::Decl::Var(var));
    }

    // put ty_defs back
    for def in types {
        result.insert(&def.name.0, ir::Decl::Type(def.ty));
    }

    result
}

#[derive(Default)]
struct Layouter {
    layouts: IndexMap<ir::Path, ir::TyLayout>,
    contains_missing_layout: bool,
}

impl Layouter {
    fn compute_ty_defs_layout(
        &mut self,
        defs: Vec<ir::TyDef>,
    ) -> Result<Vec<ir::TyDef>, Diagnostic> {
        // first pass
        let mut done_1 = Vec::with_capacity(defs.len());
        let mut redo = Vec::with_capacity(defs.len());
        for def in defs {
            self.contains_missing_layout = false;
            done_1.push(self.compute_ty_def_layout(def)?);
            redo.push(self.contains_missing_layout);
        }

        // there are still types without layout (but not top-level)
        // they are recursive references to top-level types, within arrays/enums
        // so we need to do a second pass, to fill those out
        let mut done_2 = Vec::with_capacity(done_1.len());
        for (def, redo) in std::iter::zip(done_1, redo) {
            self.contains_missing_layout = false;

            done_2.push(if redo {
                self.compute_ty_def_layout(def)?
            } else {
                def
            });

            // now, all inner types must have layout
            assert!(!self.contains_missing_layout);
        }

        Ok(done_2)
    }

    fn compute_ty_def_layout(&mut self, mut def: ir::TyDef) -> Result<ir::TyDef, Diagnostic> {
        // fold (and compute layout)
        tracing::debug!("computing layout of {:?}", &def.name);

        def.ty = self.fold_ty(def.ty).unwrap();

        let Some(layout) = def.ty.layout.clone() else {
            panic!("cannot compute layout of top-level type def?")
        };

        // save layout of current type for later references
        self.layouts.insert(def.name.clone(), layout);
        Ok(def)
    }
}

impl IrFold for Layouter {
    #[tracing::instrument(skip_all, name = "t")]
    fn fold_ty(&mut self, ty: ir::Ty) -> Result<ir::Ty, ()> {
        let mut ty = fold_ty(self, ty)?;
        self.compute_ty_layout(&mut ty);

        Ok(ty)
    }
}

impl Layouter {
    fn compute_ty_layout(&mut self, ty: &mut ir::Ty) {
        if ty.layout.is_some() {
            return;
        }

        if let ir::TyKind::Ident(path) = &ty.kind {
            // for ident, use layout of the target type
            let layout = self.layouts.get(path);
            ty.layout = layout.cloned();
        } else {
            // for others, use the simple layout algo
            ty.layout = lutra_bin::layout::compute(ty);
        }

        if ty.layout.is_none() && !ty.kind.is_function() {
            self.contains_missing_layout = true;
            tracing::debug!("missing layout: {}\n  {ty:?}", ir::print_ty(ty));
        }
    }
}

/// Types can refer to each other, which means that their layouts influence one another.
/// This function topo-sorts type defs, such that each type def only refers to previous types.
/// An exception for this rule are recursive types, which cannot be put in such an order.
/// Instead, recursive enum variants are skipped, and their enums are forced to use a pointer.
/// This makes them not depend on the layout of the inner type of the variant, which means
/// that they can refer to types that come later in the order.
///
/// Which enum variants are "recursive" is determined by `enum_hinter`.
///
/// For example, a simple case:
/// ```lt
/// type B: int32
/// type A: {B, C}
/// type C: {text, bool}
///
/// # order: A, B, C
/// ```
///
/// For example, a recursive case:
/// ```lt
/// type Tree: enum {
///   leaf: int32,
///   fork: Branches,
/// }
/// type Branches: {Tree, Tree}
///
/// # order: Tree, Fork
/// # here, Tree does refer to a later type def, but this is allowed because Tree.fork is
/// # a recursive enum variant, because it part of Tree-Branches cycle.
/// ```
fn order_ty_defs(defs: Vec<ir::TyDef>) -> Vec<ir::TyDef> {
    let mut deps: Vec<(ir::Path, Vec<ir::Path>)> = vec![];
    for def in &defs {
        let mut refs = Vec::new();
        collect_refs(&def.ty, &mut refs);
        deps.push((def.name.clone(), refs));
    }

    let order = toposort(&deps);

    let mut by_name: HashMap<_, _> = defs.into_iter().map(|d| (d.name, d.ty)).collect();
    let mut ordered = Vec::with_capacity(by_name.len());

    for group in order {
        assert!(group.len() == 1);
        let (name, ty) = by_name.remove_entry(group[0]).unwrap();
        ordered.push(ir::TyDef { name, ty });
    }
    ordered
}

pub fn collect_refs(ty: &ir::Ty, refs: &mut Vec<ir::Path>) {
    match &ty.kind {
        ir::TyKind::Ident(ident) => refs.push(ident.clone()),

        ir::TyKind::Primitive(_) => {}

        ir::TyKind::Tuple(fields) => {
            for f in fields {
                collect_refs(&f.ty, refs);
            }
        }
        ir::TyKind::Array(_) => {
            // don't recurse, because array has a pointer, always
        }

        ir::TyKind::Enum(variants) => {
            for (i, v) in variants.iter().enumerate() {
                if ty.variants_recursive.contains(&(i as u16)) {
                    continue;
                }
                collect_refs(&v.ty, refs);
            }
        }

        ir::TyKind::Function(_) => panic!("cannot layout a function"),
    }
}
