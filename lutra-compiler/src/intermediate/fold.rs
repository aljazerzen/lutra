//! A trait to "fold" the IR, so we can transitively apply some logic to
//! a whole tree by just defining how we want to handle each type.
#![allow(dead_code)]

use itertools::Itertools;

use lutra_bin::ir::{self, *};

type Result<T> = std::result::Result<T, ()>;

pub trait IrFold {
    fn fold_program(&mut self, program: Program) -> Result<Program> {
        Ok(Program {
            main: self.fold_expr(program.main)?,
            defs: program
                .defs
                .into_iter()
                .map(|t| -> Result<TyDef> {
                    Ok(TyDef {
                        name: t.name,
                        ty: self.fold_ty(t.ty)?,
                    })
                })
                .collect::<Result<Vec<_>>>()?,
        })
    }
    fn fold_expr(&mut self, expr: Expr) -> Result<Expr> {
        let ty = self.fold_ty(expr.ty)?;
        fold_expr_kind(self, expr.kind, ty)
    }
    fn fold_ptr(&mut self, ptr: Pointer, ty: Ty) -> Result<Expr> {
        fold_ptr(ptr, ty)
    }
    fn fold_call(&mut self, call: Call, ty: Ty) -> Result<Expr> {
        fold_call(self, call, ty)
    }
    fn fold_func(&mut self, func: Function, ty: Ty) -> Result<Expr> {
        fold_func(self, func, ty)
    }
    fn fold_lookup(&mut self, lookup: TupleLookup, ty: Ty) -> Result<Expr> {
        fold_lookup(self, lookup, ty)
    }
    fn fold_binding(&mut self, binding: Binding, ty: Ty) -> Result<Expr> {
        fold_binding(self, binding, ty)
    }
    fn fold_ty(&mut self, ty: Ty) -> Result<Ty> {
        fold_ty(self, ty)
    }
}

pub fn fold_expr_kind<T: ?Sized + IrFold>(fold: &mut T, kind: ExprKind, ty: Ty) -> Result<Expr> {
    use ExprKind::*;
    let kind = match kind {
        Pointer(ptr) => return fold.fold_ptr(ptr, ty),
        Literal(lit) => Literal(lit),
        Call(call) => return fold.fold_call(*call, ty),
        Function(func) => return fold.fold_func(*func, ty),
        Tuple(fields) => Tuple(fold_exprs(fold, fields)?),
        Array(items) => Array(fold_exprs(fold, items)?),
        EnumVariant(variant) => EnumVariant(Box::new(fold_enum_variant(fold, *variant)?)),
        EnumEq(eq) => EnumEq(Box::new(fold_enum_eq(fold, *eq)?)),
        EnumUnwrap(unwrap) => EnumUnwrap(Box::new(fold_enum_unwrap(fold, *unwrap)?)),
        TupleLookup(lookup) => return fold.fold_lookup(*lookup, ty),
        Binding(binding) => return fold.fold_binding(*binding, ty),
        Switch(branches) => return fold_switch(fold, branches, ty),
    };
    Ok(Expr { kind, ty })
}

pub fn fold_exprs<F: ?Sized + IrFold>(fold: &mut F, exprs: Vec<Expr>) -> Result<Vec<Expr>> {
    exprs.into_iter().map(|node| fold.fold_expr(node)).collect()
}

pub fn fold_call<T: ?Sized + IrFold>(fold: &mut T, call: Call, ty: Ty) -> Result<Expr> {
    Ok(ir::Expr {
        kind: ExprKind::Call(Box::new(Call {
            function: fold.fold_expr(call.function)?,
            args: fold_exprs(fold, call.args)?,
        })),
        ty,
    })
}

pub fn fold_ptr(ptr: Pointer, ty: Ty) -> Result<Expr> {
    Ok(Expr {
        kind: ExprKind::Pointer(ptr),
        ty,
    })
}

pub fn fold_func<T: ?Sized + IrFold>(fold: &mut T, func: Function, ty: Ty) -> Result<Expr> {
    Ok(ir::Expr {
        kind: ExprKind::Function(Box::new(Function {
            id: func.id,
            body: fold.fold_expr(func.body)?,
        })),
        ty,
    })
}

pub fn fold_enum_variant<T: ?Sized + IrFold>(
    fold: &mut T,
    variant: EnumVariant,
) -> Result<EnumVariant> {
    Ok(EnumVariant {
        tag: variant.tag,
        inner: fold.fold_expr(variant.inner)?,
    })
}

pub fn fold_enum_eq<T: ?Sized + IrFold>(fold: &mut T, variant: EnumEq) -> Result<EnumEq> {
    Ok(EnumEq {
        tag: variant.tag,
        subject: fold.fold_expr(variant.subject)?,
    })
}

pub fn fold_enum_unwrap<T: ?Sized + IrFold>(
    fold: &mut T,
    unwrap: EnumUnwrap,
) -> Result<EnumUnwrap> {
    Ok(EnumUnwrap {
        tag: unwrap.tag,
        subject: fold.fold_expr(unwrap.subject)?,
    })
}

pub fn fold_lookup<T: ?Sized + IrFold>(fold: &mut T, lookup: TupleLookup, ty: Ty) -> Result<Expr> {
    Ok(ir::Expr {
        kind: ExprKind::TupleLookup(Box::new(ir::TupleLookup {
            base: fold.fold_expr(lookup.base)?,
            position: lookup.position,
        })),
        ty,
    })
}

pub fn fold_binding<T: ?Sized + IrFold>(fold: &mut T, binding: Binding, ty: Ty) -> Result<Expr> {
    Ok(ir::Expr {
        kind: ExprKind::Binding(Box::new(ir::Binding {
            id: binding.id,
            expr: fold.fold_expr(binding.expr)?,
            main: fold.fold_expr(binding.main)?,
        })),
        ty,
    })
}

pub fn fold_switch<T: ?Sized + IrFold>(
    fold: &mut T,
    branches: Vec<SwitchBranch>,
    ty: Ty,
) -> Result<Expr> {
    Ok(ir::Expr {
        kind: ExprKind::Switch(
            branches
                .into_iter()
                .map(|branch| {
                    Ok(SwitchBranch {
                        condition: fold.fold_expr(branch.condition)?,
                        value: fold.fold_expr(branch.value)?,
                    })
                })
                .collect::<Result<Vec<_>>>()?,
        ),
        ty,
    })
}

pub fn fold_ty<T: ?Sized + IrFold>(fold: &mut T, ty: Ty) -> Result<Ty> {
    Ok(Ty {
        kind: match ty.kind {
            TyKind::Tuple(fields) => TyKind::Tuple(fold_ty_tuple_fields(fold, fields)?),
            TyKind::Array(ty) => TyKind::Array(Box::new(fold.fold_ty(*ty)?)),
            TyKind::Function(func) => TyKind::Function(Box::new(fold_ty_func(fold, *func)?)),
            TyKind::Enum(variants) => TyKind::Enum(
                variants
                    .into_iter()
                    .map(|variant| -> Result<_> {
                        Ok(TyEnumVariant {
                            name: variant.name,
                            ty: fold.fold_ty(variant.ty)?,
                        })
                    })
                    .try_collect()?,
            ),
            TyKind::Ident(_) | TyKind::Primitive(_) => ty.kind,
        },
        name: ty.name,
        layout: ty.layout,
        variants_recursive: ty.variants_recursive,
    })
}

pub fn fold_ty_func<F: ?Sized + IrFold>(fold: &mut F, f: TyFunction) -> Result<TyFunction> {
    Ok(TyFunction {
        params: f
            .params
            .into_iter()
            .map(|p| fold.fold_ty(p))
            .try_collect()?,
        body: fold.fold_ty(f.body)?,
    })
}

pub fn fold_ty_tuple_fields<F: ?Sized + IrFold>(
    fold: &mut F,
    fields: Vec<TyTupleField>,
) -> Result<Vec<TyTupleField>> {
    fields
        .into_iter()
        .map(|field| -> Result<_> {
            Ok(TyTupleField {
                name: field.name,
                ty: fold.fold_ty(field.ty)?,
            })
        })
        .try_collect()
}
