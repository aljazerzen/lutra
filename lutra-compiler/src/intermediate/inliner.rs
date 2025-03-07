use std::collections::{HashMap, HashSet};

use lutra_bin::ir;

use super::fold::{self, IrFold};

pub fn inline(mut program: ir::Program) -> Result<ir::Program, ()> {
    let mut inliner = Inliner {
        bindings: Default::default(),
        inlining: Default::default(),
    };

    program.main = inliner.fold_expr(program.main)?;
    Ok(program)
}

struct Inliner {
    bindings: HashMap<u32, ir::Function>,

    inlining: HashSet<u32>,
}

impl fold::IrFold for Inliner {
    fn fold_binding(&mut self, binding: ir::Binding, ty: ir::Ty) -> Result<ir::Expr, ()> {
        // bindings of type function
        if binding.expr.ty.kind.is_function() {
            match binding.expr.kind {
                // that are function definitions
                ir::ExprKind::Function(func) => {
                    // store in self.bindings
                    self.bindings.insert(binding.id, *func);

                    // return just the main expr
                    return self.fold_expr(binding.main);
                }

                ir::ExprKind::Pointer(_) => todo!(),

                _ => panic!(),
            }
        }
        fold::fold_binding(self, binding, ty)
    }

    fn fold_call(&mut self, call: ir::Call, ty: ir::Ty) -> Result<ir::Expr, ()> {
        let args = fold::fold_exprs(self, call.args)?;

        let function = match call.function.kind {
            // calls to bound functions
            ir::ExprKind::Pointer(ir::Pointer::Binding(ref binding_id)) => {
                if self.inlining.contains(binding_id) {
                    panic!("recursive function cannot be inlined");
                }

                if let Some(func) = self.bindings.get(binding_id) {
                    // substitute
                    let expr = Substituter::run(func.body.clone(), func.id, args);

                    self.inlining.insert(*binding_id);
                    let expr = self.fold_expr(expr);
                    self.inlining.remove(binding_id);
                    return expr;
                }

                call.function
            }

            // calls of explicit functions
            ir::ExprKind::Function(func) => {
                // substitute
                let expr = Substituter::run(func.body, func.id, args);
                return self.fold_expr(expr);
            }

            ir::ExprKind::Pointer(ir::Pointer::Parameter(_)) => todo!(),

            ir::ExprKind::Pointer(ir::Pointer::External(_)) => call.function,

            _ => unreachable!(),
        };

        let kind = ir::ExprKind::Call(Box::new(ir::Call { function, args }));
        Ok(ir::Expr { kind, ty })
    }
}

struct Substituter {
    function_id: u32,
    args: Vec<ir::Expr>,
}

impl Substituter {
    fn run(expr: ir::Expr, function_id: u32, args: Vec<ir::Expr>) -> ir::Expr {
        let mut s = Substituter { function_id, args };
        s.fold_expr(expr).unwrap()
    }
}

impl fold::IrFold for Substituter {
    fn fold_ptr(&mut self, ptr: ir::Pointer, ty: ir::Ty) -> Result<ir::Expr, ()> {
        match &ptr {
            ir::Pointer::Parameter(ptr) if ptr.function_id == self.function_id => {
                Ok(self.args[ptr.param_position as usize].clone())
            }
            _ => {
                let kind = ir::ExprKind::Pointer(ptr);
                Ok(ir::Expr { kind, ty })
            }
        }
    }
}
