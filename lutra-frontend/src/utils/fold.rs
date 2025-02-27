#![allow(dead_code)]

/// A trait to "fold" the PR AST, so we can transitively apply some logic to
/// a whole tree by just defining how we want to handle each type.
use itertools::Itertools;

use crate::pr;
use crate::pr::*;
use crate::Result;

// Fold pattern:
// - https://rust-unofficial.github.io/patterns/patterns/creational/fold.html
// Good discussions on the visitor / fold pattern:
// - https://github.com/rust-unofficial/patterns/discussions/236 (within this,
//   this comment looked interesting: https://github.com/rust-unofficial/patterns/discussions/236#discussioncomment-393517)
// - https://news.ycombinator.com/item?id=25620110

// For some functions, we want to call a default impl, because copying &
// pasting everything apart from a specific match is lots of repetition. So
// we define a function outside the trait, by default call it, and let
// implementors override the default while calling the function directly for
// some cases. Ref https://stackoverflow.com/a/66077767/3064736
pub trait PrFold {
    fn fold_stmt(&mut self, mut stmt: Stmt) -> Result<Stmt> {
        stmt.kind = fold_stmt_kind(self, stmt.kind)?;
        Ok(stmt)
    }
    fn fold_stmts(&mut self, stmts: Vec<Stmt>) -> Result<Vec<Stmt>> {
        stmts.into_iter().map(|stmt| self.fold_stmt(stmt)).collect()
    }
    fn fold_expr(&mut self, mut expr: Expr) -> Result<Expr> {
        expr.kind = self.fold_expr_kind(expr.kind)?;
        expr.ty = fold_type_opt(self, expr.ty)?;
        Ok(expr)
    }
    fn fold_expr_kind(&mut self, expr_kind: ExprKind) -> Result<ExprKind> {
        fold_expr_kind(self, expr_kind)
    }
    fn fold_exprs(&mut self, exprs: Vec<Expr>) -> Result<Vec<Expr>> {
        exprs.into_iter().map(|node| self.fold_expr(node)).collect()
    }
    fn fold_var_def(&mut self, var_def: VarDef) -> Result<VarDef> {
        fold_var_def(self, var_def)
    }
    fn fold_type_def(&mut self, ty_def: TypeDef) -> Result<TypeDef> {
        Ok(TypeDef {
            name: ty_def.name,
            ty: self.fold_type(ty_def.ty)?,
        })
    }
    fn fold_module_def(&mut self, module_def: ModuleDef) -> Result<ModuleDef> {
        fold_module_def(self, module_def)
    }
    fn fold_func_call(&mut self, func_call: FuncCall) -> Result<FuncCall> {
        fold_func_call(self, func_call)
    }
    fn fold_func(&mut self, func: Func) -> Result<Func> {
        fold_func(self, func)
    }
    fn fold_interpolate_item(&mut self, inter: InterpolateItem) -> Result<InterpolateItem> {
        fold_interpolate_item(self, inter)
    }
    fn fold_type(&mut self, t: Ty) -> Result<Ty> {
        fold_type(self, t)
    }
}

pub fn fold_expr_kind<T: ?Sized + PrFold>(fold: &mut T, expr_kind: ExprKind) -> Result<ExprKind> {
    use ExprKind::*;
    Ok(match expr_kind {
        Ident(ident) => Ident(ident),
        // All { within, except } => All {
        //     within: Box::new(fold.fold_expr(*within)?),
        //     except: Box::new(fold.fold_expr(*except)?),
        // },
        Indirection { base, field } => Indirection {
            base: Box::new(fold.fold_expr(*base)?),
            field,
        },
        Tuple(items) => Tuple(
            items
                .into_iter()
                .map(|field| -> Result<TupleField> {
                    Ok(TupleField {
                        name: field.name,
                        expr: fold.fold_expr(field.expr)?,
                    })
                })
                .try_collect()?,
        ),
        Array(items) => Array(fold.fold_exprs(items)?),
        FString(items) => FString(
            items
                .into_iter()
                .map(|x| fold.fold_interpolate_item(x))
                .try_collect()?,
        ),
        Case(cases) => Case(fold_cases(fold, cases)?),

        FuncCall(func_call) => FuncCall(fold.fold_func_call(func_call)?),
        Func(func) => Func(Box::new(fold.fold_func(*func)?)),
        // FuncApplication(func_app) => FuncApplication(super::expr::FuncApplication {
        //     func: Box::new(fold.fold_expr(*func_app.func)?),
        //     args: fold.fold_exprs(func_app.args)?,
        // }),
        Pipeline(pipeline) => Pipeline(pr::Pipeline {
            exprs: fold.fold_exprs(pipeline.exprs)?,
        }),
        Range(range) => Range(pr::Range {
            start: fold_optional_box(fold, range.start)?,
            end: fold_optional_box(fold, range.end)?,
        }),
        Binary(e) => Binary(BinaryExpr {
            left: Box::new(fold.fold_expr(*e.left)?),
            op: e.op,
            right: Box::new(fold.fold_expr(*e.right)?),
        }),
        Unary(e) => Unary(UnaryExpr {
            op: e.op,
            expr: Box::new(fold.fold_expr(*e.expr)?),
        }),

        // None of these capture variables, so we don't need to fold them.
        Internal | Literal(_) => expr_kind,
    })
}

pub fn fold_stmt_kind<T: ?Sized + PrFold>(fold: &mut T, stmt_kind: StmtKind) -> Result<StmtKind> {
    use StmtKind::*;
    Ok(match stmt_kind {
        VarDef(var_def) => VarDef(fold.fold_var_def(var_def)?),
        TypeDef(type_def) => TypeDef(fold.fold_type_def(type_def)?),
        ModuleDef(module_def) => ModuleDef(fold.fold_module_def(module_def)?),
        ImportDef(_) => stmt_kind,
    })
}

fn fold_module_def<F: ?Sized + PrFold>(fold: &mut F, module_def: ModuleDef) -> Result<ModuleDef> {
    Ok(ModuleDef {
        name: module_def.name,
        stmts: fold.fold_stmts(module_def.stmts)?,
    })
}

pub fn fold_var_def<F: ?Sized + PrFold>(fold: &mut F, var_def: VarDef) -> Result<VarDef> {
    Ok(pr::VarDef {
        name: var_def.name,
        value: fold_optional_box(fold, var_def.value)?,
        ty: var_def.ty.map(|x| fold.fold_type(x)).transpose()?,
    })
}

pub fn fold_range<F: ?Sized + PrFold>(fold: &mut F, Range { start, end }: Range) -> Result<Range> {
    Ok(Range {
        start: fold_optional_box(fold, start)?,
        end: fold_optional_box(fold, end)?,
    })
}

// This aren't strictly in the hierarchy, so we don't need to
// have an assoc. function for `fold_optional_box` — we just
// call out to the function in this module
pub fn fold_optional_box<F: ?Sized + PrFold>(
    fold: &mut F,
    opt: Option<Box<Expr>>,
) -> Result<Option<Box<Expr>>> {
    Ok(opt.map(|n| fold.fold_expr(*n)).transpose()?.map(Box::from))
}

pub fn fold_interpolate_item<F: ?Sized + PrFold>(
    fold: &mut F,
    interpolate_item: InterpolateItem,
) -> Result<InterpolateItem> {
    Ok(match interpolate_item {
        InterpolateItem::String(string) => InterpolateItem::String(string),
        InterpolateItem::Expr { expr, format } => InterpolateItem::Expr {
            expr: Box::new(fold.fold_expr(*expr)?),
            format,
        },
    })
}

fn fold_cases<F: ?Sized + PrFold>(fold: &mut F, cases: Vec<SwitchCase>) -> Result<Vec<SwitchCase>> {
    cases
        .into_iter()
        .map(|c| fold_switch_case(fold, c))
        .try_collect()
}

pub fn fold_switch_case<F: ?Sized + PrFold>(fold: &mut F, case: SwitchCase) -> Result<SwitchCase> {
    Ok(SwitchCase {
        condition: Box::new(fold.fold_expr(*case.condition)?),
        value: Box::new(fold.fold_expr(*case.value)?),
    })
}

pub fn fold_func_call<T: ?Sized + PrFold>(fold: &mut T, func_call: FuncCall) -> Result<FuncCall> {
    Ok(FuncCall {
        func: Box::new(fold.fold_expr(*func_call.func)?),
        args: fold.fold_exprs(func_call.args)?,
    })
}

pub fn fold_func<T: ?Sized + PrFold>(fold: &mut T, func: Func) -> Result<Func> {
    Ok(Func {
        body: Box::new(fold.fold_expr(*func.body)?),
        return_ty: fold_type_opt(fold, func.return_ty)?,
        params: fold_func_params(fold, func.params)?,
        ty_params: func.ty_params, // recurse into this too?
    })
}

pub fn fold_func_params<T: ?Sized + PrFold>(
    fold: &mut T,
    params: Vec<FuncParam>,
) -> Result<Vec<FuncParam>> {
    params
        .into_iter()
        .map(|param| {
            Ok(FuncParam {
                name: param.name,
                ty: fold_type_opt(fold, param.ty)?,
                default_value: fold_optional_box(fold, param.default_value)?,
                span: param.span,
            })
        })
        .try_collect()
}

#[inline]
pub fn fold_type_opt<T: ?Sized + PrFold>(fold: &mut T, ty: Option<Ty>) -> Result<Option<Ty>> {
    ty.map(|t| fold.fold_type(t)).transpose()
}

pub fn fold_type<T: ?Sized + PrFold>(fold: &mut T, ty: Ty) -> Result<Ty> {
    Ok(Ty {
        kind: match ty.kind {
            TyKind::Tuple(fields) => TyKind::Tuple(fold_ty_tuple_fields(fold, fields)?),
            TyKind::Array(ty) => TyKind::Array(Box::new(fold.fold_type(*ty)?)),
            TyKind::Function(func) => TyKind::Function(fold_ty_func(fold, func)?),
            TyKind::Enum(variants) => TyKind::Enum(
                variants
                    .into_iter()
                    .map(|variant| -> Result<_> {
                        Ok(TyEnumVariant {
                            name: variant.name,
                            ty: fold.fold_type(variant.ty)?,
                        })
                    })
                    .try_collect()?,
            ),
            // TyKind::Exclude { base, except } => TyKind::Exclude {
            //     base: Box::new(fold.fold_type(*base)?),
            //     except: Box::new(fold.fold_type(*except)?),
            // },
            TyKind::Ident(_) | TyKind::Primitive(_) => ty.kind,
        },
        span: ty.span,
        name: ty.name,
        layout: ty.layout,
    })
}

pub fn fold_ty_func<F: ?Sized + PrFold>(fold: &mut F, f: TyFunc) -> Result<TyFunc> {
    Ok(TyFunc {
        params: fold_ty_func_params(fold, f.params)?,
        body: f
            .body
            .map(|t| fold.fold_type(*t).map(Box::new))
            .transpose()?,
        ty_params: f.ty_params,
    })
}

pub fn fold_ty_func_params<F: ?Sized + PrFold>(
    fold: &mut F,
    params: Vec<Option<pr::Ty>>,
) -> Result<Vec<Option<Ty>>> {
    params
        .into_iter()
        .map(|a| fold_type_opt(fold, a))
        .try_collect()
}

pub fn fold_ty_tuple_fields<F: ?Sized + PrFold>(
    fold: &mut F,
    fields: Vec<TyTupleField>,
) -> Result<Vec<TyTupleField>> {
    fields
        .into_iter()
        .map(|field| -> Result<_> {
            Ok(TyTupleField {
                name: field.name,
                ty: fold.fold_type(field.ty)?,
            })
        })
        .try_collect()
}
