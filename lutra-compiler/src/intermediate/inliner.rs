use std::collections::{HashMap, HashSet};

use lutra_bin::ir;

use super::fold::{self, IrFold};
use crate::utils::IdGenerator;

pub fn inline(program: ir::Program) -> ir::Program {
    let (mut program, id_counts) = IdCounter::run(program);

    // inline functions
    let mut inliner = FuncInliner {
        bindings: Default::default(),

        currently_inlining: Default::default(),

        generator_var_binding: IdGenerator::new_at(id_counts.max_var_id as usize),
    };
    program.main = inliner.fold_expr(program.main).unwrap();

    tracing::debug!("ir (funcs inlined):\n{}", ir::print_no_color(&program));

    // count bindings usage
    let mut counter = BindingUsageCounter {
        usage: Default::default(),
        simple: Default::default(),
    };
    program.main = counter.fold_expr(program.main).unwrap();
    tracing::debug!("binding_usage = {:?}", counter.usage);
    tracing::debug!("simple_bindings = {:?}", counter.simple);

    // inline bindings
    let mut inliner = BindingInliner::new(counter.usage, counter.simple);
    program.main = inliner.fold_expr(program.main).unwrap();

    program
}

struct FuncInliner {
    // bindings of functions
    bindings: HashMap<u32, ir::Function>,

    currently_inlining: HashSet<u32>,

    generator_var_binding: IdGenerator,
}

impl fold::IrFold for FuncInliner {
    fn fold_binding(&mut self, binding: ir::Binding, ty: ir::Ty) -> Result<ir::Expr, ()> {
        // bindings of the function type
        if binding.expr.ty.kind.is_function() {
            match binding.expr.kind {
                // that are function definitions
                ir::ExprKind::Function(func) => {
                    // fold the function
                    let func = self.fold_func(*func, binding.expr.ty)?;
                    let func = func.kind.into_function().unwrap();

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
                if let Some(func) = self.bindings.get(binding_id) {
                    let expr = self.substitute_function(func.clone(), args);

                    self.currently_inlining.insert(*binding_id);
                    let expr = self.fold_expr(expr);
                    self.currently_inlining.remove(binding_id);
                    return expr;
                } else {
                    // panic!("binding not found: {binding_id}")
                    call.function
                }
            }

            // calls of lambda functions
            ir::ExprKind::Function(func) => {
                // substitute

                let expr = self.substitute_function(*func, args);
                return self.fold_expr(expr);
            }

            ir::ExprKind::Pointer(ir::Pointer::Parameter(_)) => call.function,

            ir::ExprKind::Pointer(ir::Pointer::External(_)) => call.function,

            _ => unreachable!(),
        };

        let kind = ir::ExprKind::Call(Box::new(ir::Call { function, args }));
        Ok(ir::Expr { kind, ty })
    }

    fn fold_ptr(&mut self, ptr: ir::Pointer, ty: ir::Ty) -> Result<ir::Expr, ()> {
        if let ir::Pointer::Binding(binding_id) = &ptr {
            // special case: when there is a function ptr that is not called directly
            // we have to inline it
            if let Some(func) = self.bindings.get(binding_id) {
                return Ok(ir::Expr {
                    kind: ir::ExprKind::Function(Box::new(func.clone())),
                    ty,
                });
            }
        }
        fold::fold_ptr(ptr, ty)
    }

    // optimization: inline unneeded switch cases
    fn fold_switch(&mut self, branches: Vec<ir::SwitchBranch>, ty: ir::Ty) -> Result<ir::Expr, ()> {
        // detect cases:
        // (switch
        //    (...cond..., bool_then)
        //    (.anything., bool_else)
        // )
        fn as_prim8(expr: &ir::Expr) -> Option<bool> {
            expr.kind
                .as_literal()
                .and_then(|l| l.as_prim8())
                .map(|i| *i != 0)
        }
        if ty.kind.as_ident().is_some_and(|i| i.is(&["std", "Bool"]))
            && branches.len() == 2
            && let Some(value_then) = as_prim8(&branches[0].value)
            && let Some(value_else) = as_prim8(&branches[1].value)
        {
            let cond = branches.into_iter().next().unwrap().condition;

            match (value_then, value_else) {
                (true, true) | (false, false) => {
                    // pathological case, we don't need to compare
                    return Ok(ir::Expr::new_lit_bool(value_then));
                }
                (true, false) => {
                    // this just executes the cond
                    return self.fold_expr(cond);
                }
                (false, true) => {
                    // this just inverts the cond
                    let cond = self.fold_expr(cond)?;

                    let std_not = ir::Expr::new(
                        ir::ExternalPtr {
                            id: "std::ops::not".into(),
                        },
                        ir::Ty::new(ir::TyFunction {
                            params: vec![ir::Ty::bool()],
                            body: ir::Ty::bool(),
                        }),
                    );

                    return Ok(ir::Expr::new(
                        ir::Call {
                            function: std_not,
                            args: vec![cond],
                        },
                        ir::Ty::bool(),
                    ));
                }
            }
        }

        // detect cases:
        // (switch
        //    (.anything., x)
        // )
        if branches.len() == 1 {
            let value = branches.into_iter().next().unwrap().value;
            return self.fold_expr(value);
        }

        fold::fold_switch(self, branches, ty)
    }
}

impl FuncInliner {
    fn substitute_function(&mut self, func: ir::Function, args: Vec<ir::Expr>) -> ir::Expr {
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
        let mut expr = Substituter::run(func.body, func.id, arg_pointers);

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
        expr
    }
}

struct BindingUsageCounter {
    usage: HashMap<u32, usize>,
    simple: HashSet<u32>,
}

impl BindingUsageCounter {
    fn is_simple_expr(&self, expr: &ir::Expr) -> bool {
        match &expr.kind {
            ir::ExprKind::Literal(ir::Literal::Text(_)) => false,
            ir::ExprKind::Literal(_) => true,
            // A binding alias is only simple if the binding it points to is
            // simple. Otherwise inlining the alias into multiple sites would
            // duplicate a non-simple binding (see .plans/fix-inlining.md).
            ir::ExprKind::Pointer(ir::Pointer::Binding(id)) => self.simple.contains(id),
            ir::ExprKind::Pointer(_) => true,
            ir::ExprKind::TupleLookup(lookup) => self.is_simple_expr(&lookup.base),
            ir::ExprKind::Tuple(fields) => fields.iter().all(|f| self.is_simple_expr(&f.expr)),
            _ => false,
        }
    }
}

impl fold::IrFold for BindingUsageCounter {
    fn fold_binding(&mut self, binding: ir::Binding, ty: ir::Ty) -> Result<ir::Expr, ()> {
        self.usage.insert(binding.id, 0);

        // Check if this binding is simple
        if self.is_simple_expr(&binding.expr) {
            self.simple.insert(binding.id);
        }

        fold::fold_binding(self, binding, ty)
    }

    fn fold_ptr(&mut self, ptr: ir::Pointer, ty: ir::Ty) -> Result<ir::Expr, ()> {
        if let ir::Pointer::Binding(binding_id) = &ptr {
            // count usage of bindings
            *self.usage.entry(*binding_id).or_default() += 1;
        }
        fold::fold_ptr(ptr, ty)
    }
}

struct BindingInliner {
    bindings: HashMap<u32, ir::Expr>,

    to_inline: HashSet<u32>,
}

impl BindingInliner {
    fn new(bindings_usage: HashMap<u32, usize>, simple: HashSet<u32>) -> Self {
        // inline vars that are used 1 or 0 times, or are simple
        let to_inline: HashSet<u32> = bindings_usage
            .into_iter()
            .filter(|(id, usage_count)| *usage_count <= 1 || simple.contains(id))
            .map(|(id, _)| id)
            .collect();

        tracing::debug!("inlining vars: {:?}", to_inline);

        BindingInliner {
            bindings: Default::default(),
            to_inline,
        }
    }
}

impl IrFold for BindingInliner {
    fn fold_binding(&mut self, binding: ir::Binding, ty: ir::Ty) -> Result<ir::Expr, ()> {
        if self.to_inline.contains(&binding.id) {
            // store in self.bindings
            let expr = self.fold_expr(binding.expr)?;
            self.bindings.insert(binding.id, expr);

            // return just the main expr
            return self.fold_expr(binding.main);
        }
        fold::fold_binding(self, binding, ty)
    }
    fn fold_ptr(&mut self, ptr: ir::Pointer, ty: ir::Ty) -> Result<ir::Expr, ()> {
        if let ir::Pointer::Binding(binding_id) = &ptr {
            // replace ptr with bound value

            if let Some(value) = self.bindings.get(binding_id) {
                return Ok(value.clone());
            }
        }
        fold::fold_ptr(ptr, ty)
    }

    // optimization: simplify call chains
    fn fold_call(&mut self, call: ir::Call, ty: ir::Ty) -> Result<ir::Expr, ()> {
        // normal fold
        let expr = fold::fold_call(self, call, ty)?;

        // optimizations
        let expr = rewrite_cmp_swap_greater(expr);
        let expr = rewrite_cmp_tag_to_op(expr);
        let expr = rewrite_not_chain(expr);
        Ok(expr)
    }
}

/// Matches `eq(enum_tag(cmp(a, b)), 2)` and returns `eq(enum_tag(cmp(b, a)), 0)`
fn rewrite_cmp_swap_greater(expr: ir::Expr) -> ir::Expr {
    let Some((_, 2)) = as_cmp_tag_test(&expr) else {
        return expr;
    };

    let mut expr = expr;
    let eq = expr.kind.as_call_mut().unwrap();

    let enum_tag = eq.args[0].kind.as_enum_tag_mut().unwrap();
    let cmp = enum_tag.subject.kind.as_call_mut().unwrap();
    cmp.args.reverse();

    let tag = eq.args[1].kind.as_literal_mut().unwrap();
    *tag = ir::Literal::Prim8(0);

    expr
}

/// Matches `eq(enum_tag(cmp(a, b)), tag)` and returns `lt(a, b)` or `eq(a, b)`.
fn rewrite_cmp_tag_to_op(expr: ir::Expr) -> ir::Expr {
    let Some((_, tag)) = as_cmp_tag_test(&expr) else {
        return expr;
    };
    let op = match tag {
        0 => "std::ops::lt",
        1 => "std::ops::eq",
        _ => return expr,
    };

    // unwrap the outer two ops
    let [enum_tag, _] = unpack(expr.kind.into_call().unwrap().args);
    let mut expr = enum_tag.kind.into_enum_tag().unwrap().subject;

    // overwrite op
    let call = expr.kind.as_call_mut().unwrap();
    call.function.kind = ir::ExternalPtr { id: op.to_string() }.into();

    // overwrite types
    let func_ty = call.function.ty.kind.as_function_mut().unwrap();
    func_ty.body = ir::Ty::bool();
    expr.ty = ir::Ty::bool();

    expr
}

/// Simplifies `not` applied to another negatable call:
///   not(not(x))    ->  x
///   not(lt(a, b))  ->  lte(b, a)
///   not(lte(a, b)) ->  lt(b, a)
fn rewrite_not_chain(expr: ir::Expr) -> ir::Expr {
    let Some([inner]) = as_call_to(&expr, "std::ops::not") else {
        return expr;
    };

    // not(not(x)) --> x
    if as_call_to(inner, "std::ops::not").is_some() {
        let [inner] = unpack(expr.kind.into_call().unwrap().args);
        let [x] = unpack(inner.kind.into_call().unwrap().args);
        return x;
    }

    // not(lt(a, b)) --> lte(b, a) ; not(lte(a, b)) --> lt(b, a)
    let swapped_op = if as_call_to(inner, "std::ops::lt").is_some() {
        "std::ops::lte"
    } else if as_call_to(inner, "std::ops::lte").is_some() {
        "std::ops::lt"
    } else {
        return expr;
    };

    let [mut inner] = unpack(expr.kind.into_call().unwrap().args);

    let inner_call = inner.kind.as_call_mut().unwrap();
    inner_call.args.reverse();
    let inner_id = as_external_mut(&mut inner_call.function).unwrap();
    *inner_id = swapped_op.to_string();

    inner
}

/// Matches `eq(enum_tag(cmp(a, b)), tag)` and returns `([a, b], tag)`.
///
/// The `Ordering` enum has three variants, so its tag occupies a single byte
/// and the compared literal is a `Prim8`.
fn as_cmp_tag_test(expr: &ir::Expr) -> Option<(&[ir::Expr], u8)> {
    let Some([enum_expr, tag]) = as_call_to(expr, "std::ops::eq") else {
        return None;
    };

    let enum_tag = enum_expr.kind.as_enum_tag()?;
    let args = as_call_to(&enum_tag.subject, "std::ops::cmp")?;
    let tag = tag.kind.as_literal()?.as_prim8()?;
    Some((args, *tag))
}

/// Matches `(call (pointer (external id)), args)` and returns `args`.
fn as_call_to<'e>(expr: &'e ir::Expr, id: &str) -> Option<&'e [ir::Expr]> {
    let call = expr.kind.as_call()?;
    let ptr = call.function.kind.as_pointer()?.as_external()?;
    if ptr.id != id {
        return None;
    }
    Some(&call.args)
}

/// Returns `true` if `expr` is a pointer to the external function `id`.
fn as_external_mut(expr: &mut ir::Expr) -> Option<&mut String> {
    Some(&mut expr.kind.as_pointer_mut()?.as_external_mut()?.id)
}

fn unpack<const N: usize, T: std::fmt::Debug>(x: Vec<T>) -> [T; N] {
    x.try_into().unwrap()
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
pub(crate) struct IdCounter {
    pub max_var_id: u32,
    pub max_func_id: u32,
}

impl IdCounter {
    pub(crate) fn run(mut program: ir::Program) -> (ir::Program, IdCounter) {
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
