use indexmap::IndexMap;
use itertools::Itertools;

use super::fold::IrFold;
use crate::Result;
use crate::diagnostic::Diagnostic;
use lutra_bin::ir;

#[tracing::instrument(skip_all, name = "layouter")]
pub fn on_program(mut program: ir::Program) -> ir::Program {
    tracing::trace!(
        "types: {:?}",
        program.types.iter().map(|x| &x.name).collect_vec()
    );

    let mut l = Layouter {
        ty_defs: program.types.into_iter().map(|d| (d.name, d.ty)).collect(),
        contains_missing_layout: false,
        currently_resolving: None,
    };

    // compute layout for each of ty_defs
    l.compute_ty_defs().unwrap();

    // compute layout for the main expr
    program.main = l.fold_expr(program.main).unwrap();

    program.types = l
        .ty_defs
        .into_iter()
        .map(|(name, ty)| ir::TyDef { name, ty })
        .collect();
    program
}

#[tracing::instrument(skip_all, name = "layouter")]
pub fn on_root_module(root_module: ir::Module) -> ir::Module {
    let mut l = Layouter {
        ty_defs: IndexMap::new(),
        contains_missing_layout: false,
        currently_resolving: None,
    };
    let mut vars = Vec::new();
    for (path, def) in root_module.iter_defs_re() {
        match def {
            ir::Decl::Module(_) => unreachable!(),
            ir::Decl::Type(ty) => {
                l.ty_defs.insert(path, ty.clone());
            }
            ir::Decl::Var(ty) => {
                vars.push((path, ty.clone()));
            }
        }
    }

    // compute layout for each of ty_defs
    l.compute_ty_defs().unwrap();

    // compute layout for each of vars (and place into result module)
    let mut result = ir::Module { decls: Vec::new() };
    for (path, var) in vars {
        let var = l.fold_ty(var).unwrap();

        result.insert(&path.0, ir::Decl::Var(var));
    }

    // put ty_defs back
    for (path, ty) in l.ty_defs {
        result.insert(&path.0, ir::Decl::Type(ty));
    }

    result
}

#[tracing::instrument(skip_all, name = "layouter")]
pub fn on_ty(ty: ir::Ty) -> ir::Ty {
    let mut l = Layouter {
        ty_defs: IndexMap::new(),
        contains_missing_layout: false,
        currently_resolving: None,
    };
    l.fold_ty(ty).unwrap()
}

struct Layouter {
    ty_defs: IndexMap<ir::Path, ir::Ty>,

    currently_resolving: Option<ir::Path>,
    contains_missing_layout: bool,
}

impl Layouter {
    fn compute_ty_defs(&mut self) -> Result<(), Diagnostic> {
        let mut todo: Vec<_> = (0..self.ty_defs.len()).collect();
        let mut retry = Vec::new();

        for _ in 0..5 {
            for i in todo {
                // clone out
                let (name, ty) = self.ty_defs.get_index(i).unwrap();

                // fold (and compute layout)
                tracing::debug!("computing layout of {name:?}");
                self.contains_missing_layout = false;
                self.currently_resolving = Some(name.clone());
                let ty = self.fold_ty(ty.clone()).unwrap();
                tracing::debug!("computed: {:?} {:?}", ty.layout, ty.variants_recursive);

                // write back in
                *self.ty_defs.get_index_mut(i).unwrap().1 = ty;

                if self.contains_missing_layout {
                    retry.push(i);
                }
            }

            todo = retry;
            retry = Vec::new();
            if todo.is_empty() {
                break;
            }
        }

        if !todo.is_empty() {
            let paths = todo
                .iter()
                .map(|i| self.ty_defs.get_index(*i).unwrap().0.0.join("::"))
                .join(", ");
            panic!("Cannot layout: {paths}")
        }

        Ok(())
    }

    fn get_ty_mat<'t>(&'t self, ty: &'t ir::Ty) -> &'t ir::Ty {
        if let ir::TyKind::Ident(path) = &ty.kind {
            self.ty_defs
                .get(path)
                .unwrap_or_else(|| panic!("cannot find {path:?}"))
        } else {
            ty
        }
    }

    fn compute_ty_layout(&mut self, ty: &mut ir::Ty) -> Result<(), Diagnostic> {
        if ty.layout.is_some() {
            return Ok(());
        }

        let ty_mat = self.get_ty_mat(ty);

        if ty_mat.layout.is_some() {
            ty.layout = ty_mat.layout.clone();
            return Ok(());
        }

        if let ir::TyKind::Ident(ident) = &ty.kind {
            if Some(ident) == self.currently_resolving.as_ref() {
                return Ok(());
            }
        }

        ty.layout = match &ty_mat.kind {
            ir::TyKind::Primitive(_) | ir::TyKind::Array(_) | ir::TyKind::Tuple(_) => {
                lutra_bin::layout::get_layout_simple(ty_mat)
            }

            ir::TyKind::Enum(variants) => {
                let mut var_rec = Vec::new();
                for (index, variant) in variants.iter().enumerate() {
                    if variant.ty.layout.is_none() {
                        // unresolved - this type is (probably) recursive, save this info
                        // (I don't think this logic is 100% sound)
                        var_rec.push(index as u16);
                        tracing::debug!("found recursive variant: {variant:?}")
                    }
                }

                if var_rec.is_empty() {
                    lutra_bin::layout::get_layout_simple(ty_mat)
                } else {
                    ty.variants_recursive.extend(var_rec);
                    // TODO: is this ok? Does it work in all cases?
                    Some(ir::TyLayout {
                        head_size: 40,
                        body_ptrs: vec![1],
                    })
                }
            }

            ir::TyKind::Ident(ident) => {
                panic!("get_ty_mat returned an ident: {ident:?}")
            }

            // functions cannot be serialized - they don't need or have a layout
            ir::TyKind::Function(_) => return Ok(()),
        };
        Ok(())
    }
}

impl IrFold for Layouter {
    #[tracing::instrument(skip_all, name = "t")]
    fn fold_ty(&mut self, ty: ir::Ty) -> Result<ir::Ty, ()> {
        let mut ty = super::fold::fold_ty(self, ty)?;
        self.compute_ty_layout(&mut ty).unwrap();

        if ty.layout.is_none() && !ty.kind.is_function() {
            self.contains_missing_layout = true;
            tracing::debug!("missing layout: {}", lutra_bin::ir::print_ty(&ty));
        }

        Ok(ty)
    }
}
