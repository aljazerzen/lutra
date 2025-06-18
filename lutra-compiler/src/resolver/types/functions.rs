use itertools::Itertools;

use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::pr::{self, *};
use crate::utils;
use crate::utils::fold::{self, PrFold};
use crate::{Result, Span};

use super::scope::Scope;
use super::TypeResolver;

impl TypeResolver<'_> {
    /// Folds function types, so they are resolved to material types, ready for type checking.
    /// Requires id of the function call node, so it can be used to generic type arguments.
    #[tracing::instrument(name = "func", skip_all, fields(f = func.params.iter().map(|p| &p.name).join(",")))]
    pub fn resolve_func(&mut self, scope_id: usize, mut func: Box<Func>) -> Result<Box<Func>> {
        tracing::debug!(
            "resolving func with params: ({})",
            func.params.iter().map(|p| &p.name).join(", ")
        );
        let mut scope = Scope::new(scope_id);

        // prepare generic arguments
        scope.insert_type_params(&func.ty_params);
        self.scopes.push(scope);

        // fold types
        func.params = fold::fold_func_params(self, func.params)?;
        func.return_ty = fold::fold_type_opt(self, func.return_ty)?;

        // put params into scope
        self.scopes.last_mut().unwrap().insert_params(&func)?;

        func.body = Box::new(self.fold_expr(*func.body)?);

        // validate that the body has correct type
        if let Some(return_ty) = &func.return_ty {
            self.validate_expr_type(&mut func.body, return_ty, &|| None)?;
        }

        tracing::debug!("func done, popping scope");

        // finalize generic type args
        let mapping = self
            .finalize_type_vars()
            .with_span_fallback(func.body.span)?;
        let func = utils::TypeReplacer::on_func(*func, mapping);

        // pop the scope
        self.scopes.pop().unwrap();

        tracing::debug!("scope finalized");

        Ok(Box::new(func))
    }

    pub fn resolve_func_call(
        &mut self,
        func: Box<Expr>,
        args: Vec<Expr>,
        span: Option<Span>,
    ) -> Result<Expr> {
        let metadata = self.gather_func_metadata(&func);
        let tspan = tracing::span!(tracing::Level::TRACE, "call", f = metadata.as_debug_name());
        let _enter = tspan.enter();

        let fn_ty = func.ty.as_ref().unwrap();
        let fn_ty = fn_ty.kind.as_func().unwrap().clone();

        let args_match_params = args.len() == fn_ty.params.len();
        if !args_match_params {
            return Err(Diagnostic::new_custom(format!(
                "{}expected {} arguments, but got {}",
                metadata
                    .name_hint
                    .map(|x| format!("{x} "))
                    .unwrap_or_default(),
                fn_ty.params.len(),
                args.len(),
            ))
            .with_span(span));
        }

        let mut args_resolved = Vec::with_capacity(args.len());
        for (param, arg) in itertools::zip_eq(&fn_ty.params, args) {
            // fold
            let mut arg = self.fold_expr(arg)?;

            // validate type
            let who = || {
                metadata
                    .name_hint
                    .as_ref()
                    .map(|n| format!("function {n}, one of the params")) // TODO: param name
            };
            if let Some(param_ty) = param {
                self.validate_expr_type(&mut arg, param_ty, &who)?;
            }

            args_resolved.push(arg);
        }

        Ok(Expr {
            ty: fn_ty.body.clone().map(|x| *x),
            span,
            ..Expr::new(ExprKind::FuncCall(FuncCall {
                func,
                args: args_resolved,
            }))
        })
    }

    /// In PRQL, func is just an expression and does not have a name (the same way
    /// as literals don't have a name). Regardless, we want to provide name hints for functions
    /// in error messages (i.e. `std.count requires 2 arguments, found 1`), so here we infer name
    /// and annotations for functions from its declaration.
    fn gather_func_metadata(&self, func: &Expr) -> FuncMetadata {
        let mut res = FuncMetadata::default();

        let ExprKind::Ident(fq_ident) = &func.kind else {
            return res;
        };
        res.name_hint = Some(fq_ident.clone());

        res
    }

    /// Wraps non-tuple Exprs into a singleton Tuple.
    pub(super) fn coerce_into_tuple(&mut self, expr: Expr) -> Result<Expr> {
        let is_tuple_ty = expr.ty.as_ref().unwrap().kind.is_tuple(); // && !expr.kind.is_all();
        Ok(if is_tuple_ty {
            expr
        } else {
            let span = expr.span;
            let mut expr = pr::Expr::new(pr::ExprKind::Tuple(vec![pr::TupleField {
                name: None,
                expr,
            }]));
            expr.span = span;

            self.fold_expr(expr)?
        })
    }
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct FuncMetadata {
    /// Name of the function. Used for user-facing messages only.
    pub name_hint: Option<Path>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ImplicitClosureConfig {
    pub param: u8,
    pub this: Option<u8>,
    pub that: Option<u8>,
}

impl FuncMetadata {
    pub(crate) fn as_debug_name(&self) -> &str {
        let ident = self.name_hint.as_ref();

        ident.map(|n| n.name()).unwrap_or("<anonymous>")
    }
}
