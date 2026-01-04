use itertools::Itertools;

use crate::diagnostic::Diagnostic;
use crate::pr::{self, *};
use crate::utils::fold::{self, PrFold};
use crate::{Result, Span, utils};

use super::TypeResolver;
use super::scope::{Scope, ScopeKind};

impl TypeResolver<'_> {
    /// Folds function types, so they are resolved to material types, ready for type checking.
    #[tracing::instrument(name = "func", skip_all, fields(f = func.params.iter().map(|p| &p.name).join(",")))]
    pub fn resolve_func(&mut self, scope_id: usize, mut func: Box<Func>) -> Result<Box<Func>> {
        tracing::debug!(
            "resolving func with params: ({})",
            func.params.iter().map(|p| &p.name).join(", ")
        );
        let allow_native = self.allow_native_functions;
        self.allow_native_functions = false;

        let mut scope = Scope::new(
            scope_id,
            if func.ty_params.is_empty() {
                ScopeKind::Nested
            } else {
                ScopeKind::Isolated
            },
        );

        // prepare generic arguments
        scope.insert_type_params(&func.ty_params);
        self.scopes.push(scope);

        // fold types
        func.params = fold::fold_func_params(self, func.params)?;
        func.return_ty = fold::fold_type_opt(self, func.return_ty)?;
        if func.ty_params.is_empty() {
            // only allow ty param inference for functions without type params

            for param in func.params.iter_mut() {
                if param.ty.is_none() {
                    param.ty = Some(self.introduce_ty_var(pr::TyDomain::Open, param.span));
                }
            }
        }

        // put params into scope
        let res = self.scopes.last_mut().unwrap().insert_params(&func);
        res.map_err(|mut d| d.remove(0))?;

        // fold body
        if let Some(body) = func.body {
            let mut body = Box::new(self.fold_expr(*body)?);

            // validate that the body has correct type
            if let Some(return_ty) = &func.return_ty {
                self.validate_expr_type(&mut body, return_ty, &|| None)?;
            }
            func.body = Some(body);
        } else {
            // there is no body: this is a native function definition
            if !allow_native {
                return Err(Diagnostic::new_custom("missing function body"));
            }

            // ... which require return type to be set
            if func.return_ty.is_none() {
                return Err(Diagnostic::new_custom("missing return type"));
            }
        }

        // pop the scope
        tracing::debug!("func done, popping scope");
        let mapping = self.finalize_type_vars()?;
        let func = utils::TypeReplacer::on_func(*func, mapping);
        self.scopes.pop().unwrap();

        Ok(Box::new(func))
    }

    pub fn resolve_func_call(
        &mut self,
        func: Box<Expr>,
        args: Vec<CallArg>,
        span: Option<Span>,
    ) -> Result<Expr> {
        let metadata = self.gather_func_metadata(&func);
        let tspan = tracing::span!(tracing::Level::TRACE, "call", f = metadata.as_debug_name());
        let _enter = tspan.enter();

        let fn_ty = func.ty.as_ref().unwrap();
        let fn_ty = fn_ty.kind.as_func().unwrap().clone();

        let args = self.match_args_to_params(args, &fn_ty.params, &metadata, span);

        let mut args_resolved = Vec::with_capacity(fn_ty.params.len());
        for (param, arg) in std::iter::zip(&fn_ty.params, args) {
            let Some(mut arg) = arg else {
                continue;
            };

            // fold
            arg.expr = self.fold_expr(arg.expr)?;

            // validate type
            let who = || metadata.as_who();
            if let Some(param_ty) = &param.ty {
                self.validate_expr_type(&mut arg.expr, param_ty, &who)
                    .unwrap_or_else(self.push_diagnostic());
            }

            if param.constant {
                self.const_validator
                    .validate_is_const(&arg.expr)
                    .map_err(|span| {
                        Diagnostic::new_custom("non-constant expression")
                            .with_span(span.or(arg.span))
                    })
                    .unwrap_or_else(self.push_diagnostic())
            }

            args_resolved.push(arg);
        }

        Ok(Expr {
            ty: fn_ty.body.clone().map(|x| *x),
            span,
            ..Expr::new(ExprKind::Call(Call {
                subject: func,
                args: args_resolved,
            }))
        })
    }

    /// For each given arg, finds the func param that it should pass the value to.
    /// Takes care of labelled args. Returns args in the same order as func params.
    /// An arg might be None, which means that it was not supplied and we have pushed
    /// an error onto [Self::diagnostics].
    fn match_args_to_params(
        &self,
        mut args: Vec<pr::CallArg>,
        params: &[pr::TyFuncParam],
        metadata: &FuncMetadata,
        span: Option<Span>,
    ) -> Vec<Option<pr::CallArg>> {
        if args.len() != params.len() {
            let who = metadata
                .as_who()
                .map(|n| format!("{n} "))
                .unwrap_or_default();
            let message = format!(
                "{who}expected {} arguments, but got {}",
                params.len(),
                args.len()
            );
            self.diagnostics
                .push(Diagnostic::new_custom(message).with_span(span));
        }

        let mut args_reordered = vec![None; args.len()];

        // find first labelled arg
        let first_labelled = args
            .iter()
            .find_position(|p| p.label.is_some())
            .map(|(p, _)| p)
            .unwrap_or(args.len());

        // match labelled args
        let labelled = args.split_off(first_labelled);
        for arg in labelled {
            if arg.label.is_none() {
                self.diagnostics.push(
                    Diagnostic::new_custom("positional arg must come before labelled arg")
                        .with_span(arg.span),
                );
                continue;
            }

            let Some((param_pos, _)) = params.iter().find_position(|p| p.label == arg.label) else {
                self.diagnostics.push(
                    Diagnostic::new_custom(format!(
                        "unknown parameter `{}`",
                        arg.label.as_ref().unwrap()
                    ))
                    .with_span(arg.span),
                );
                continue;
            };
            args_reordered[param_pos] = Some(arg);
        }

        // now fill-in non-labelled args
        for arg in args {
            assert!(arg.label.is_none());

            // find first non-populated
            let Some((pos, _)) = args_reordered.iter().find_position(|a| a.is_none()) else {
                // this happens when there is too much args
                // (we have pushed an error into self.diagnostics already)
                continue;
            };
            args_reordered[pos] = Some(arg);
        }
        args_reordered
    }

    /// In Lutra, func is just an expression and does not have a name (the same way
    /// as literals don't have a name). Regardless, we want to provide name hints for functions
    /// in error messages (i.e. `std.count requires 2 arguments, found 1`), so here we infer name
    /// and annotations for functions from its definition.
    fn gather_func_metadata(&self, func: &Expr) -> FuncMetadata {
        let mut res = FuncMetadata::default();

        let ExprKind::Ident(fq_ident) = &func.kind else {
            return res;
        };
        res.name_hint = Some(fq_ident.clone());

        res
    }
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct FuncMetadata {
    /// Name of the function. Used for user-facing messages only.
    pub name_hint: Option<Path>,
}

impl FuncMetadata {
    fn as_debug_name(&self) -> &str {
        let ident = self.name_hint.as_ref();

        ident.map(|n| n.last()).unwrap_or("<anonymous>")
    }

    fn as_who(&self) -> Option<String> {
        self.name_hint.as_ref().map(|n| format!("func {n}"))
    }
}
