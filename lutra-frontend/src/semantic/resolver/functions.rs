use itertools::Itertools;

use crate::ir::decl::{Decl, DeclKind};
use crate::ir::fold::{self, PrFold};
use crate::pr::*;
use crate::{Error, Result, Span, WithErrorInfo};

use super::scope::Scope;
use super::Resolver;

impl Resolver<'_> {
    /// Folds function types, so they are resolved to material types, ready for type checking.
    /// Requires id of the function call node, so it can be used to generic type arguments.
    pub fn resolve_func(&mut self, mut func: Box<Func>) -> Result<Box<Func>> {
        let scope = Scope::new();

        // prepare generic arguments
        for generic_param in &func.generic_type_params {
            let _domain: Vec<Ty> = generic_param
                .domain
                .iter()
                .map(|b| self.fold_type(b.clone()))
                .try_collect()?;

            // register the generic type param in the resolver
            todo!();
        }
        self.scopes.push(scope);

        // fold types
        func.params = func
            .params
            .into_iter()
            .map(|p| -> Result<_> {
                Ok(FuncParam {
                    ty: fold::fold_type_opt(self, p.ty)?,
                    ..p
                })
            })
            .try_collect()?;
        func.return_ty = fold::fold_type_opt(self, func.return_ty)?;

        // put params into scope
        prepare_scope_of_func(self.scopes.last_mut().unwrap(), &func);

        func.body = Box::new(self.fold_expr(*func.body)?);

        // validate that the body has correct type
        self.validate_expr_type(&mut func.body, func.return_ty.as_ref(), &|| None)?;

        // pop the scope
        let _scope = self.scopes.pop().unwrap();

        // pop generic types
        if !func.generic_type_params.is_empty() {
            todo!()
        }

        Ok(func)
    }

    pub fn resolve_func_application(
        &mut self,
        func: Box<Expr>,
        args: Vec<Expr>,
        span: Option<Span>,
    ) -> Result<Expr> {
        let metadata = self.gather_func_metadata(&func);

        let fn_ty = func.ty.as_ref().unwrap();
        let fn_ty = fn_ty.kind.as_function().unwrap();
        let fn_ty = fn_ty.as_ref().unwrap().clone();

        log::debug!(
            "func {} {}/{} params",
            metadata.as_debug_name(),
            args.len(),
            fn_ty.params.len()
        );

        if args.len() > fn_ty.params.len() {
            return Err(Error::new_simple(format!(
                "Too many arguments to function `{}`",
                metadata.as_debug_name()
            ))
            .with_span(span));
        }

        let enough_args = args.len() == fn_ty.params.len();
        if !enough_args {
            todo!()
        }

        self.init_func_app_generic_args(&fn_ty, func.id.unwrap());

        log::debug!("resolving args of function {}", metadata.as_debug_name());
        let args = self.resolve_func_app_args(&func, args, &metadata)?;

        self.finalize_func_app_generic_args(&fn_ty, func.id.unwrap())
            .with_span_fallback(span)?;

        // run fold again, so idents that used to point to generics get inlined
        let return_ty = fn_ty
            .return_ty
            .clone()
            .map(|ty| self.fold_type(*ty))
            .transpose()?;

        Ok(expr_of_func_application(*func, args, return_ty, span))
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
        // let fq_ident = loop {
        //     match &func.kind {
        //         ExprKind::Ident(i) => break i,
        //         ExprKind::FuncApplication(FuncApplication { func: f, .. }) => {
        //             func = f.as_ref();
        //         }
        //         _ => return res,
        //     }
        // };

        // populate name hint
        res.name_hint = Some(fq_ident.clone());

        let decl = self.root_mod.module.get(fq_ident).unwrap();

        fn literal_as_u8(expr: Option<&Expr>) -> Option<u8> {
            Some(*expr?.kind.as_literal()?.as_integer()? as u8)
        }

        // populate implicit_closure config
        if let Some(im_clos) = decl
            .annotations
            .iter()
            .find_map(|a| annotation_as_func_call(a, "implicit_closure"))
        {
            res.implicit_closure = Some(Box::new(ImplicitClosureConfig {
                param: literal_as_u8(im_clos.args.first()).unwrap(),
                this: literal_as_u8(im_clos.named_args.get("this")),
                that: literal_as_u8(im_clos.named_args.get("that")),
            }));
        }

        // populate coerce_tuple config
        if let Some(coerce_tuple) = decl
            .annotations
            .iter()
            .find_map(|a| annotation_as_func_call(a, "coerce_tuple"))
        {
            res.coerce_tuple = Some(literal_as_u8(coerce_tuple.args.first()).unwrap());
        }

        res
    }

    fn init_func_app_generic_args(&mut self, _fn_ty: &TyFunc, _func_id: usize) {
        /*
        for generic_param in &fn_ty.generic_type_params {
            // register the generic type param in the resolver
            let generic_ident = Ident::from_path(vec![
                NS_GENERIC.to_string(),
                func_id.to_string(),
                generic_param.name.clone(),
            ]);

            let generic = Decl::from(DeclKind::GenericArg(Vec::new()));
            self.root_mod.module.insert(generic_ident, generic).unwrap();
        }
         */
    }

    fn finalize_func_app_generic_args(&mut self, _fn_ty: &TyFunc, _func_id: usize) -> Result<()> {
        /*
        for generic_param in &fn_ty.generic_type_params {
            let ident = Ident::from_path(vec![
                NS_GENERIC.to_string(),
                func_id.to_string(),
                generic_param.name.clone(),
            ]);

            let decl = self.root_mod.module.get_mut(&ident).unwrap();

            let DeclKind::GenericArg(bounds) = &decl.kind else {
                // this case means that we have already finalized this generic arg and should never happen
                // hack: this case does happen, because our resolution order is all over the place,
                //    so I had to add "finalize_function_generic_args" into "resolve_function_arg".
                //    This only sorta makes sense, so I want to mark this case as "will remove in the future".
                panic!()
            };

            let res: Vec<&Ty> = if generic_param.domain.is_empty() {
                // deduplicate bounds
                let mut passed = Vec::new();
                for domain_type in &generic_param.domain {
                    if !passed.contains(&domain_type) {
                        passed.push(domain_type);
                    }
                }
                passed
            } else {
                // type check each of bounds against all domain candidates
                let mut passed = Vec::new();
                for domain_type in &generic_param.domain {
                    let mut pass = true;
                    for bound in bounds {
                        if bound != domain_type {
                            pass = false;
                            break;
                        }
                    }
                    if pass {
                        passed.push(domain_type);
                    }
                }
                passed
            };

            if res.len() != 1 {
                return Err(Error::new_simple(format!(
                    "cannot determine the type {}",
                    generic_param.name
                )));
            };
            let ty = res.into_iter().next().unwrap();
            log::debug!("finalizing {ident} into {}", write_ty(ty));
            decl.kind = DeclKind::Ty(ty.clone());
        }
        */
        Ok(())
    }

    /// Resolves function arguments. Will return `Err(func)` is partial application is required.
    fn resolve_func_app_args(
        &mut self,
        func: &Expr,
        args_to_resolve: Vec<Expr>,
        metadata: &FuncMetadata,
    ) -> Result<Vec<Expr>> {
        let mut args = vec![Expr::new(Literal::Boolean(false)); args_to_resolve.len()];

        let func_name = &metadata.name_hint;

        let func_ty = func.ty.as_ref().unwrap();
        let func_ty = func_ty.kind.as_function().unwrap();
        let func_ty = func_ty.as_ref().unwrap();
        let mut param_args = itertools::zip_eq(&func_ty.params, args_to_resolve)
            .map(Box::new)
            .map(Some)
            .collect_vec();

        // pull out this and that
        let this_pos = metadata.implicit_closure.as_ref().and_then(|i| i.this);
        let that_pos = metadata.implicit_closure.as_ref().and_then(|i| i.that);

        // prepare order
        let order = this_pos
            .into_iter()
            .chain(that_pos)
            .map(|x| x as usize)
            .chain(0..param_args.len())
            .unique()
            .collect_vec();

        for index in order {
            let (param, mut arg) = *param_args[index].take().unwrap();
            let should_coerce_tuple = metadata.coerce_tuple.map_or(false, |i| i as usize == index);

            arg = self.resolve_func_app_arg(arg, param, func_name, should_coerce_tuple)?;

            args[index] = arg;
        }

        Ok(args)
    }

    fn resolve_func_app_arg(
        &mut self,
        arg: Expr,
        param: &Option<Ty>,
        func_name: &Option<Path>,
        coerce_tuple: bool,
    ) -> Result<Expr> {
        // fold
        let mut arg = self.fold_expr(arg)?;

        if coerce_tuple {
            arg = self.coerce_into_tuple(arg)?;
        }

        // validate type
        let who = || {
            func_name
                .as_ref()
                .map(|n| format!("function {n}, one of the params")) // TODO: param name
        };
        self.validate_expr_type(&mut arg, param.as_ref(), &who)?;

        Ok(arg)
    }

    /// Wraps non-tuple Exprs into a singleton Tuple.
    pub(super) fn coerce_into_tuple(&mut self, expr: Expr) -> Result<Expr> {
        let is_tuple_ty = expr.ty.as_ref().unwrap().kind.is_tuple(); // && !expr.kind.is_all();
        Ok(if is_tuple_ty {
            // a helpful check for a common anti-pattern
            if let Some(alias) = expr.alias {
                return Err(Error::new_simple(format!("unexpected assign to `{alias}`"))
                    .push_hint(format!("move assign into the tuple: `{{{alias} = ...}}`"))
                    .with_span(expr.span));
            }

            expr
        } else {
            let span = expr.span;
            let mut expr = Expr::new(ExprKind::Tuple(vec![expr]));
            expr.span = span;

            self.fold_expr(expr)?
        })
    }
}

fn prepare_scope_of_func(scope: &mut Scope, func: &Func) {
    for param in &func.params {
        let v = Decl {
            kind: DeclKind::Variable(param.ty.clone()),
            ..Default::default()
        };
        let param_name = param.name.split('.').last().unwrap();
        scope.values.insert(param_name.to_string(), v);
    }
}

/// Utility to match function calls by name and unpack its arguments.
pub fn annotation_as_func_call<'a>(a: &'a Annotation, name: &str) -> Option<&'a FuncCall> {
    let call = a.expr.kind.as_func_call()?;

    let func_name = call.name.kind.as_ident()?;
    if func_name.len() != 1 || func_name.name() != name {
        return None;
    }
    Some(call)
}
#[derive(Debug, PartialEq, Clone, Default)]
pub struct FuncMetadata {
    /// Name of the function. Used for user-facing messages only.
    pub name_hint: Option<Path>,

    pub implicit_closure: Option<Box<ImplicitClosureConfig>>,
    pub coerce_tuple: Option<u8>,
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

pub fn expr_of_func_application(
    func: Expr,
    args: Vec<Expr>,
    body_ty: Option<Ty>,
    span: Option<Span>,
) -> Expr {
    let fn_ty = func.ty.as_ref().unwrap();
    let fn_ty = fn_ty.kind.as_function().unwrap();
    let fn_ty = fn_ty.as_ref().unwrap();

    let ty_func_params: Vec<_> = fn_ty.params[args.len()..].to_vec();

    let ty = if ty_func_params.is_empty() {
        body_ty
    } else {
        Some(Ty::new(TyFunc {
            params: ty_func_params,
            return_ty: body_ty.map(Box::new),
        }))
    };

    Expr {
        ty,
        span,
        ..Expr::new(ExprKind::FuncCall(FuncCall {
            name: Box::new(func),
            args,
            named_args: Default::default(),
        }))
    }
}
