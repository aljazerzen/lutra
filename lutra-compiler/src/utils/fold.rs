#![allow(dead_code)]

use indexmap::IndexMap;
/// A trait to "fold" the PR AST, so we can transitively apply some logic to
/// a whole tree by just defining how we want to handle each type.
use itertools::Itertools;

use crate::Result;
use crate::pr;
use crate::pr::*;

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
    fn fold_def(&mut self, mut def: Def) -> Result<Def> {
        def.kind = fold_def_kind(self, def.kind)?;
        Ok(def)
    }
    fn fold_expr(&mut self, mut expr: Expr) -> Result<Expr> {
        expr.kind = self.fold_expr_kind(expr.kind)?;
        expr.ty = fold_type_opt(self, expr.ty)?;
        expr.ty_args = expr
            .ty_args
            .into_iter()
            .map(|t| self.fold_type(t))
            .try_collect()?;
        Ok(expr)
    }
    fn fold_expr_kind(&mut self, expr_kind: ExprKind) -> Result<ExprKind> {
        fold_expr_kind(self, expr_kind)
    }
    fn fold_exprs(&mut self, exprs: Vec<Expr>) -> Result<Vec<Expr>> {
        exprs.into_iter().map(|node| self.fold_expr(node)).collect()
    }
    fn fold_expr_def(&mut self, var_def: ExprDef) -> Result<ExprDef> {
        fold_expr_def(self, var_def)
    }
    fn fold_type_def(&mut self, ty_def: TyDef) -> Result<TyDef> {
        Ok(TyDef {
            ty: self.fold_type(ty_def.ty)?,
            is_framed: ty_def.is_framed,
        })
    }
    fn fold_module_def(&mut self, module_def: ModuleDef) -> Result<ModuleDef> {
        fold_module_def(self, module_def)
    }
    fn fold_func_call(&mut self, func_call: Call) -> Result<Call> {
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
    fn fold_type_annotation(&mut self, a: TypeAnnotation) -> Result<TypeAnnotation> {
        Ok(TypeAnnotation {
            ty: Box::new(self.fold_type(*a.ty)?),
            expr: Box::new(self.fold_expr(*a.expr)?),
        })
    }
    fn fold_pattern(&mut self, pattern: Pattern) -> Result<Pattern> {
        fold_pattern(self, pattern)
    }
}

pub fn fold_expr_kind<T: ?Sized + PrFold>(fold: &mut T, expr_kind: ExprKind) -> Result<ExprKind> {
    use ExprKind::*;
    Ok(match expr_kind {
        Ident(ident) => Ident(ident),
        Lookup { base, lookup } => Lookup {
            base: Box::new(fold.fold_expr(*base)?),
            lookup,
        },
        Tuple(items) => Tuple(
            items
                .into_iter()
                .map(|field| -> Result<TupleField> {
                    Ok(TupleField {
                        name: field.name,
                        unpack: field.unpack,
                        expr: fold.fold_expr(field.expr)?,
                    })
                })
                .try_collect()?,
        ),
        Array(items) => Array(fold.fold_exprs(items)?),
        Variant(variant) => Variant(pr::Variant {
            name: variant.name,
            inner: fold_optional_box(fold, variant.inner)?,
        }),
        FString(items) => FString(
            items
                .into_iter()
                .map(|x| fold.fold_interpolate_item(x))
                .try_collect()?,
        ),
        Match(match_) => Match(fold_match(fold, match_)?),
        If(if_else) => If(fold_if(fold, if_else)?),
        VarBinding(binding) => VarBinding(pr::VarBinding {
            name: binding.name,
            bound: Box::new(fold.fold_expr(*binding.bound)?),
            main: Box::new(fold.fold_expr(*binding.main)?),
        }),

        Call(func_call) => Call(fold.fold_func_call(func_call)?),
        Func(func) => Func(Box::new(fold.fold_func(*func)?)),
        FuncShort(func) => FuncShort(Box::new(pr::FuncShort {
            param: fold_func_param(fold, func.param)?,
            body: Box::new(fold.fold_expr(*func.body)?),
        })),
        Nested(inner) => Nested(Box::new(fold.fold_expr(*inner)?)),
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

        TypeAnnotation(annotation) => TypeAnnotation(fold.fold_type_annotation(annotation)?),

        Literal(_) => expr_kind,
    })
}

pub fn fold_def_kind<T: ?Sized + PrFold>(fold: &mut T, def_kind: DefKind) -> Result<DefKind> {
    use DefKind::*;
    Ok(match def_kind {
        Expr(var_def) => Expr(fold.fold_expr_def(var_def)?),
        Ty(type_def) => Ty(fold.fold_type_def(type_def)?),
        Module(module_def) => Module(fold.fold_module_def(module_def)?),
        Import(_) => def_kind,
        Unresolved(inner) => Unresolved(
            inner
                .map(|x| fold_def_kind(fold, *x))
                .transpose()?
                .map(Box::new),
        ),
    })
}

fn fold_module_def<F: ?Sized + PrFold>(fold: &mut F, module_def: ModuleDef) -> Result<ModuleDef> {
    let mut defs = IndexMap::with_capacity(module_def.defs.len());
    for (name, def) in module_def.defs {
        defs.insert(name, fold.fold_def(def)?);
    }
    Ok(ModuleDef { defs })
}

pub fn fold_expr_def<F: ?Sized + PrFold>(fold: &mut F, expr_def: ExprDef) -> Result<ExprDef> {
    Ok(pr::ExprDef {
        value: Box::new(fold.fold_expr(*expr_def.value)?),
        ty: expr_def.ty.map(|x| fold.fold_type(x)).transpose()?,
        constant: expr_def.constant,
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

fn fold_match<F: ?Sized + PrFold>(fold: &mut F, match_: Match) -> Result<Match> {
    Ok(Match {
        subject: Box::new(fold.fold_expr(*match_.subject)?),
        branches: match_
            .branches
            .into_iter()
            .map(|c| fold_match_branch(fold, c))
            .try_collect()?,
    })
}

pub fn fold_match_branch<F: ?Sized + PrFold>(
    fold: &mut F,
    case: MatchBranch,
) -> Result<MatchBranch> {
    Ok(MatchBranch {
        pattern: fold.fold_pattern(case.pattern)?,
        value: Box::new(fold.fold_expr(*case.value)?),
    })
}

fn fold_if<F: ?Sized + PrFold>(fold: &mut F, if_else: If) -> Result<If> {
    Ok(If {
        condition: Box::new(fold.fold_expr(*if_else.condition)?),
        then: Box::new(fold.fold_expr(*if_else.then)?),
        els: Box::new(fold.fold_expr(*if_else.els)?),
    })
}

pub fn fold_func_call<T: ?Sized + PrFold>(fold: &mut T, call: Call) -> Result<Call> {
    Ok(Call {
        subject: Box::new(fold.fold_expr(*call.subject)?),
        args: call
            .args
            .into_iter()
            .map(|a| -> Result<_> {
                Ok(CallArg {
                    label: a.label,
                    expr: fold.fold_expr(a.expr)?,
                    span: a.span,
                })
            })
            .try_collect()?,
    })
}

pub fn fold_func<T: ?Sized + PrFold>(fold: &mut T, func: Func) -> Result<Func> {
    Ok(Func {
        body: fold_optional_box(fold, func.body)?,
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
        .map(|p| fold_func_param(fold, p))
        .collect()
}

pub fn fold_func_param<T: ?Sized + PrFold>(fold: &mut T, param: FuncParam) -> Result<FuncParam> {
    Ok(FuncParam {
        constant: param.constant,
        label: param.label,
        name: param.name,
        ty: fold_type_opt(fold, param.ty)?,
        span: param.span,
    })
}

pub fn fold_pattern<T: ?Sized + PrFold>(fold: &mut T, pattern: Pattern) -> Result<Pattern> {
    Ok(Pattern {
        kind: match pattern.kind {
            PatternKind::Enum(name, inner) => PatternKind::Enum(
                name,
                inner
                    .map(|p| fold.fold_pattern(*p).map(Box::new))
                    .transpose()?,
            ),
            PatternKind::Literal(lit) => PatternKind::Literal(lit),
            PatternKind::AnyOf(pats) => PatternKind::AnyOf(
                pats.into_iter()
                    .map(|p| fold.fold_pattern(p))
                    .collect::<Result<_, _>>()?,
            ),
            PatternKind::Bind(name) => PatternKind::Bind(name),
        },
        span: pattern.span,
        variant_tag: pattern.variant_tag,
    })
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
            TyKind::Func(func) => TyKind::Func(fold_ty_func(fold, func)?),
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
            TyKind::TupleComprehension(comp) => TyKind::TupleComprehension(TyTupleComprehension {
                tuple: Box::new(fold.fold_type(*comp.tuple)?),
                variable_name: comp.variable_name,
                variable_ty: comp.variable_ty,
                body_name: comp.body_name,
                body_ty: Box::new(fold.fold_type(*comp.body_ty)?),
            }),
        },
        span: ty.span,
        name: ty.name,
        scope_id: ty.scope_id,
        target: ty.target,
    })
}

pub fn fold_ty_func<F: ?Sized + PrFold>(fold: &mut F, f: TyFunc) -> Result<TyFunc> {
    Ok(TyFunc {
        params: f
            .params
            .into_iter()
            .map(|p| fold_ty_func_param(fold, p))
            .try_collect()?,
        body: f
            .body
            .map(|t| fold.fold_type(*t).map(Box::new))
            .transpose()?,
        ty_params: f.ty_params,
    })
}

pub fn fold_ty_func_param<F: ?Sized + PrFold>(
    fold: &mut F,
    p: pr::TyFuncParam,
) -> Result<pr::TyFuncParam> {
    Ok(pr::TyFuncParam {
        constant: p.constant,
        label: p.label,
        ty: fold_type_opt(fold, p.ty)?,
    })
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
                unpack: field.unpack,
                ty: fold.fold_type(field.ty)?,
            })
        })
        .try_collect()
}
