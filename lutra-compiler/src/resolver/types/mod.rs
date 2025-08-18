mod def;
mod expr;
mod functions;
mod inference;
mod pattern;
mod scope;
mod tuple;
mod validation;

use crate::diagnostic::Diagnostic;
use crate::pr;
use crate::utils::fold::PrFold;

pub fn run(
    root_module: &mut pr::ModuleDef,
    resolution_order: &[Vec<pr::Path>],
) -> Result<(), Vec<crate::diagnostic::Diagnostic>> {
    let mut resolver = TypeResolver::new(root_module);

    let diagnostic = resolver.resolve_defs(resolution_order).err();
    resolver.diagnostics.extend(diagnostic);

    if resolver.diagnostics.len() == 0 {
        Ok(())
    } else {
        Err(resolver.diagnostics.into_vec())
    }
}

/// Can fold (walk) over AST and for each function call or variable find what they are referencing.
struct TypeResolver<'a> {
    root_mod: &'a mut pr::ModuleDef,

    debug_current_def: crate::pr::Path,

    scopes: Vec<scope::Scope>,

    const_validator: super::const_eval::ConstantValidator,

    diagnostics: append_only_vec::AppendOnlyVec<Diagnostic>,
}

impl TypeResolver<'_> {
    fn new(root_mod: &mut pr::ModuleDef) -> TypeResolver {
        TypeResolver {
            root_mod,
            debug_current_def: crate::pr::Path::from_name("?"),
            scopes: Vec::new(),

            const_validator: super::const_eval::ConstantValidator::new(),
            diagnostics: Default::default(),
        }
    }

    fn push_diagnostic(&self) -> impl FnOnce(Diagnostic) {
        |d| {
            assert!(d.span.is_some());
            self.diagnostics.push(d);
        }
    }

    /// If diagnostic has span, push it to buffer.
    /// If it does not, return it back so it can be propagated up the stack where is should find the span.
    fn try_push_diagnostic(&self) -> impl FnOnce(Diagnostic) -> Result<(), Diagnostic> {
        |d| {
            if d.span.is_some() {
                self.diagnostics.push(d);
                Ok(())
            } else {
                Err(d)
            }
        }
    }

    /// Resolve expr and try to recover errors.
    /// On error, construct a "fallback placeholder" with provided type and
    /// store the diagnostic so resolving will be aborted later.
    fn fold_expr_or_recover(&mut self, expr: pr::Expr, fallback_ty: &pr::Ty) -> pr::Expr {
        match self.fold_expr(expr) {
            Ok(e) => e,
            Err(d) => {
                self.diagnostics.push(d);

                let mut placeholder = pr::Expr::new(pr::ExprKind::Tuple(vec![]));
                placeholder.ty = Some(fallback_ty.clone());
                placeholder
            }
        }
    }
}
