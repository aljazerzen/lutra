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
    is_std: bool,
) -> Result<(), Vec<crate::diagnostic::Diagnostic>> {
    let mut resolver = TypeResolver::new(root_module, is_std);

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
    is_std: bool,

    current_def_fq: Option<pr::Path>,
    scopes: Vec<scope::Scope>,

    const_validator: super::const_eval::ConstantValidator,

    diagnostics: append_only_vec::AppendOnlyVec<Diagnostic>,
}

impl TypeResolver<'_> {
    fn new(root_mod: &mut pr::ModuleDef, is_std: bool) -> TypeResolver<'_> {
        TypeResolver {
            root_mod,
            is_std,

            current_def_fq: None,
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

    /// Store diagnostic in a slot. Existing diagnostic is pushed into [Self::diagnostics].
    fn collect_err(&mut self, slot: &mut Option<Diagnostic>, d: Diagnostic) {
        if let Some(existing) = slot.take() {
            self.diagnostics.push(existing);
        }
        *slot = Some(d);
    }

    /// Given a Vec of errors, push all but the last into [Self::diagnostics] and return the last.
    fn push_errors(&mut self, mut errors: Vec<Diagnostic>) -> Diagnostic {
        let r = errors.pop().unwrap();
        self.diagnostics.extend(errors);
        r
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
                placeholder.ty = Some(Box::new(fallback_ty.clone()));
                placeholder
            }
        }
    }
}
