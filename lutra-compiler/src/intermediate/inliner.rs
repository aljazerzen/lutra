use std::collections::{HashMap, HashSet};

use lutra_bin::ir;

use crate::utils::IdGenerator;

use super::fold::{self, IrFold};

pub fn inline(program: ir::Program) -> ir::Program {
    let (mut program, id_counts) = IdCounter::run(program);

    // inline functions
    let mut inliner = FuncInliner {
        bindings: Default::default(),
        binding_usage: Default::default(),

        currently_inlining: Default::default(),

        generator_var_binding: IdGenerator::new_at(id_counts.max_var_id as usize),
    };
    program.main = inliner.fold_expr(program.main).unwrap();

    log::debug!("binding_usage = {:?}", inliner.binding_usage);

    // inline vars
    let mut inliner = VarInliner::new(inliner.binding_usage);
    program.main = inliner.fold_expr(program.main).unwrap();

    program
}

struct FuncInliner {
    // bindings of functions
    bindings: HashMap<u32, ir::Function>,

    // used later, for VarInliner
    binding_usage: HashMap<u32, usize>,

    currently_inlining: HashSet<u32>,

    generator_var_binding: IdGenerator,
}

impl fold::IrFold for FuncInliner {
    fn fold_binding(&mut self, binding: ir::Binding, ty: ir::Ty) -> Result<ir::Expr, ()> {
        self.binding_usage.insert(binding.id, 0);

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
                if self.currently_inlining.contains(binding_id) {
                    panic!("recursive function cannot be inlined");
                }

                // count usage of bindings
                *self.binding_usage.entry(*binding_id).or_default() += 1;

                if let Some(func) = self.bindings.get(binding_id) {
                    // generate args bound to vars
                    let mut arg_var_ids = Vec::with_capacity(args.len());
                    let mut arg_pointers = Vec::with_capacity(args.len());
                    for arg in &args {
                        let id = self.generator_var_binding.next() as u32;
                        arg_var_ids.push(id);
                        arg_pointers.push(ir::Expr {
                            kind: ir::ExprKind::Pointer(ir::Pointer::Binding(id)),
                            ty: arg.ty.clone(),
                        });
                    }

                    // substitute
                    tracing::debug!("inlining call to function {} with {arg_var_ids:?}", func.id);
                    let mut expr = Substituter::run(func.body.clone(), func.id, arg_pointers);

                    // wrap in Bindings
                    for (id, arg) in std::iter::zip(arg_var_ids, args) {
                        expr = ir::Expr {
                            ty: expr.ty.clone(),
                            kind: ir::ExprKind::Binding(Box::new(ir::Binding {
                                id,
                                expr: arg,
                                main: expr,
                            })),
                        }
                    }

                    self.currently_inlining.insert(*binding_id);
                    let expr = self.fold_expr(expr);
                    self.currently_inlining.remove(binding_id);
                    return expr;
                }

                call.function
            }

            // calls of lambda functions
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

    fn fold_ptr(&mut self, ptr: ir::Pointer, ty: ir::Ty) -> Result<ir::Expr, ()> {
        if let ir::Pointer::Binding(binding_id) = &ptr {
            // count usage of bindings
            *self.binding_usage.entry(*binding_id).or_default() += 1;
        }
        fold::fold_ptr(ptr, ty)
    }
}

struct VarInliner {
    bindings: HashMap<u32, ir::Expr>,

    bindings_to_inline: HashSet<u32>,
}

impl VarInliner {
    fn new(bindings_usage: HashMap<u32, usize>) -> Self {
        let bindings_to_inline = bindings_usage
            .into_iter()
            // inline vars that are used 1 or 0 times
            .filter(|(_, usage_count)| *usage_count <= 1)
            .map(|(id, _)| id)
            .collect();

        VarInliner {
            bindings: Default::default(),
            bindings_to_inline,
        }
    }
}

impl IrFold for VarInliner {
    fn fold_binding(&mut self, binding: ir::Binding, ty: ir::Ty) -> Result<ir::Expr, ()> {
        if self.bindings_to_inline.contains(&binding.id) {
            // store in self.bindings
            self.bindings.insert(binding.id, binding.expr);

            // return just the main expr
            return self.fold_expr(binding.main);
        }
        fold::fold_binding(self, binding, ty)
    }
    fn fold_ptr(&mut self, ptr: ir::Pointer, ty: ir::Ty) -> Result<ir::Expr, ()> {
        if let ir::Pointer::Binding(binding_id) = &ptr {
            // replace ptr with bound value

            // to avoid a clone, we remove from bindings
            // this is ok, because each ptr will appear at most once
            if let Some(value) = self.bindings.remove(binding_id) {
                return Ok(value);
            }
        }
        fold::fold_ptr(ptr, ty)
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

#[derive(Default)]
struct IdCounter {
    pub max_var_id: u32,
    pub max_func_id: u32,
}

impl IdCounter {
    fn run(mut program: ir::Program) -> (ir::Program, IdCounter) {
        let mut c = Self::default();
        program.main = c.fold_expr(program.main).unwrap();
        (program, c)
    }
}

impl fold::IrFold for IdCounter {
    fn fold_func(&mut self, func: ir::Function, ty: ir::Ty) -> Result<ir::Expr, ()> {
        self.max_func_id = u32::max(self.max_func_id, func.id);
        fold::fold_func(self, func, ty)
    }

    fn fold_binding(&mut self, binding: ir::Binding, ty: ir::Ty) -> Result<ir::Expr, ()> {
        self.max_var_id = u32::max(self.max_var_id, binding.id);
        fold::fold_binding(self, binding, ty)
    }

    fn fold_ty(&mut self, ty: ir::Ty) -> Result<ir::Ty, ()> {
        Ok(ty)
    }
}
