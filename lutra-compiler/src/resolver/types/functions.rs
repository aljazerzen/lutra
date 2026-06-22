use itertools::Itertools;

use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::pr::{self, *};
use crate::utils::fold::{self, PrFold};
use crate::{Result, Span, utils};

use super::TypeResolver;
use super::scope::{Scope, ScopeKind};

impl TypeResolver<'_> {
    #[tracing::instrument(name = "func", skip_all, fields(f = func.params.iter().map(|p| &p.name).join(",")))]
    pub fn resolve_func(&mut self, scope_id: usize, mut func: Box<Func>) -> Result<Box<Func>> {
        tracing::debug!(
            "resolving func with params: ({})",
            func.params.iter().map(|p| &p.name).join(", ")
        );
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
        self.resolve_func_tys(&mut func)?;

        // put params into scope
        let s = self.scopes.last_mut().unwrap();
        s.insert_params(&func).map_err(|e| self.push_errors(e))?;

        // resolve param defaults (requires scope to be complete)
        func.params = self.resolve_func_param_defaults(func.params)?;

        // fold body
        let mut body = Box::new(self.fold_expr(*func.body)?);
        // validate that the body has correct type
        if let Some(return_ty) = &func.return_ty {
            self.validate_expr_type(&mut body, return_ty, &|| None)?;
        }
        func.body = body;

        // pop the scope
        tracing::debug!("func done, popping scope");
        let mapping = self.finalize_type_vars()?;
        let func = utils::TypeReplacer::on_func(*func, mapping);
        self.scopes.pop().unwrap();

        Ok(Box::new(func))
    }

    fn resolve_func_tys(&mut self, func: &mut pr::Func) -> Result<()> {
        let mut err_slot = None;

        // params
        for p in &mut func.params {
            let Some(ty) = p.ty.take() else { continue };
            match self.fold_type(ty) {
                Ok(ty) => p.ty = Some(ty),
                Err(d) => self.collect_err(&mut err_slot, d),
            }
        }
        // return ty
        func.return_ty = fold::fold_type_opt(self, func.return_ty.take())?;

        // param ty inference
        if func.ty_params.is_empty() {
            // only allow ty param inference for functions without type params

            for param in func.params.iter_mut() {
                if param.ty.is_none() {
                    param.ty = Some(self.introduce_ty_var(pr::TyDomain::Open, param.span));
                }
            }
        }

        err_slot.map_or(Ok(()), Err)
    }

    fn resolve_func_param_defaults(
        &mut self,
        params: Vec<pr::FuncParam>,
    ) -> Result<Vec<pr::FuncParam>, Diagnostic> {
        let mut resolved = Vec::with_capacity(params.len());
        let mut err_slot = None;
        let mut last_default = None;
        for mut p in params {
            // validate no positionals after defaults
            if p.label.is_none() && last_default.is_some() {
                let d = err_unsuable_default(last_default, Some(p.span));
                self.collect_err(&mut err_slot, d);
            }
            let Some(default) = p.default.take() else {
                resolved.push(p);
                continue;
            };
            last_default = Some(default.span.unwrap_or(p.span));

            // resolve default
            match self.resolve_func_param_default(*default, &p.ty) {
                Ok(default) => p.default = Some(Box::new(default)),
                Err(e) => self.collect_err(&mut err_slot, e),
            }
            resolved.push(p);
        }
        err_slot.map_or(Ok(resolved), Err)
    }

    pub fn resolve_ty_func(
        &mut self,
        mut ty_func: pr::TyFunc,
        span: Option<Span>,
    ) -> Result<pr::TyFunc, Diagnostic> {
        let mut err_slot = None;
        let mut last_default = None;
        for p in &mut ty_func.params {
            // introduce new ty vars for missing type annotations
            // (this is needed to find non-inferable params)
            if p.ty.is_none() {
                p.ty = Some(self.introduce_ty_var(pr::TyDomain::Open, span.unwrap()));
            }
            // fold ty
            p.ty = p.ty.take().map(|t| self.fold_type(t)).transpose()?;

            // validate no positionals after defaults
            if p.label.is_none() && last_default.is_some() {
                self.collect_err(&mut err_slot, err_unsuable_default(last_default, p.span));
            }

            // fold defaults
            if let Some(default) = p.default.take() {
                last_default = Some(default.span.or(p.span).unwrap());

                match self.resolve_func_param_default(*default, &p.ty) {
                    Ok(default) => p.default = Some(Box::new(default)),
                    Err(e) => self.collect_err(&mut err_slot, e),
                }
            }
        }
        if ty_func.body.is_none() {
            // introduce new ty vars for missing type annotations
            // (this is needed to find non-inferable params)
            ty_func.body = Some(Box::new(
                self.introduce_ty_var(pr::TyDomain::Open, span.unwrap()),
            ));
        }
        err_slot.map_or(Ok(ty_func), Err)
    }

    fn resolve_func_param_default(
        &mut self,
        default: pr::Expr,
        ty: &Option<pr::Ty>,
    ) -> Result<pr::Expr, Diagnostic> {
        // validate const
        let r = self.const_validator.validate_is_const(&default);
        if let Err(span) = r {
            return Err(Diagnostic::new_custom("param defaults must be const").with_span(span));
        }

        // fold
        let mut default = self.fold_expr(default)?;

        // validate type
        if let Some(expected_ty) = ty {
            self.validate_expr_type(&mut default, expected_ty, &|| None)
                .unwrap_or_else(self.push_diagnostic());
        }
        Ok(default)
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

        let args = self.resolve_call_args(args, &fn_ty.params, metadata, span);
        Ok(Expr {
            ty: fn_ty.body.clone(),
            span,
            ..Expr::new(ExprKind::Call(Call {
                subject: func,
                args,
            }))
        })
    }

    pub fn resolve_call_args(
        &mut self,
        args: Vec<CallArg>,
        params: &[pr::TyFuncParam],
        metadata: FuncMetadata,
        span: Option<Span>,
    ) -> Vec<CallArg> {
        let args = self.match_args_to_params(args, params, &metadata, span);
        let mut args_resolved = Vec::with_capacity(params.len());
        for (param, arg) in std::iter::zip(params, args) {
            let Some(mut arg) = arg else {
                if let Some(default) = &param.default {
                    args_resolved.push(pr::CallArg {
                        expr: *default.clone(),
                        label: None,
                        span: None,
                    });
                }
                continue;
            };

            // fold
            match self.fold_expr(arg.expr) {
                Ok(e) => arg.expr = e,
                Err(e) => {
                    self.diagnostics.push(e);
                    continue;
                }
            }

            // validate type
            let who = || metadata.as_who();
            if let Some(param_ty) = &param.ty {
                self.validate_expr_type(&mut arg.expr, param_ty, &who)
                    .unwrap_or_else(self.push_diagnostic());
            }

            // validate const marker
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
        args_resolved
    }

    /// For each given arg, finds the func param that it should pass the value to.
    /// Takes care of labelled args. Returns args in the same order as func params.
    /// An arg might be None, which means that it was not provided and is either:
    /// - missing (and we pushed to self.diagnostics), or
    /// - has a default.
    fn match_args_to_params(
        &self,
        mut args: Vec<pr::CallArg>,
        params: &[pr::TyFuncParam],
        metadata: &FuncMetadata,
        span: Option<Span>,
    ) -> Vec<Option<pr::CallArg>> {
        let mut args_reordered = vec![None; params.len()];

        // validate too many args
        if args.len() > params.len() {
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

        // report missing args
        for (a, p) in args_reordered.iter().zip(params) {
            if a.is_some() || p.default.is_some() {
                continue;
            }
            let label = p.label.as_deref().unwrap_or("an");
            self.diagnostics
                .push(Diagnostic::new_custom(format!("missing {label} argument")).with_span(span));
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

fn err_unsuable_default(default: Option<Span>, positional: Option<Span>) -> Diagnostic {
    Diagnostic::new_custom("this default value can never be used")
        .with_span(default)
        .push_additional("because this param is not labelled", positional)
        .push_hint("Either swap param order or add param label")
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
