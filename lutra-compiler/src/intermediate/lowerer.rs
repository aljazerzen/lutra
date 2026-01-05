use std::borrow::Cow;
use std::collections::{HashMap, VecDeque};

use indexmap::IndexMap;
use itertools::Itertools;
use lutra_bin::ir;

use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::pr;
use crate::resolver::NS_STD;
use crate::utils::{self, IdGenerator};
use crate::{Project, Result};

pub fn lower_expr(project: &Project, main_pr: &pr::Expr) -> ir::Program {
    let mut lowerer = Lowerer::new(&project.root_module);

    let (input_ty, packed) = get_entry_point_input(main_pr);
    lowerer.program_input_ty = Some((lowerer.lower_ty(input_ty), packed));
    tracing::debug!(
        "program_input_ty = {}",
        ir::print_ty(&lowerer.program_input_ty.as_ref().unwrap().0)
    );
    lowerer.is_main_a_func = main_pr.ty.as_ref().unwrap().kind.is_func();

    let main = if let Some(pr::Ref::Global(target_fq)) = &main_pr.target {
        // optimization: inline direct idents, don't bind them to vars
        lowerer
            .lower_expr_def(target_fq, main_pr.ty_args.clone())
            .unwrap()
    } else {
        lowerer.lower_expr(main_pr).unwrap()
    };
    let main = lowerer.prepare_entry_point(main);

    let main = lowerer.lower_def_dependencies(main, project).unwrap();

    lowerer.lower_ty_defs_queue();
    let defs = order_ty_defs(lowerer.type_defs, project);

    ir::Program { main, defs }
}

struct Lowerer<'a> {
    root_module: &'a pr::ModuleDef,

    scopes: Vec<Scope>,

    is_main_a_func: bool,

    /// Type of the program's input. Flag for when input is packed from multiple params.
    program_input_ty: Option<(ir::Ty, bool)>,

    /// Expr defs that are referenced from generated IR.
    /// Contain FQ path and list of type arguments, pointing to id the should
    /// be accessible at.
    def_dependencies: IndexMap<(pr::Path, Vec<pr::Ty>), u32>,

    type_defs_queue: VecDeque<pr::Path>,
    type_defs: HashMap<pr::Path, ir::Ty>,

    generator_function_scope: IdGenerator<usize>,
    generator_var_binding: IdGenerator<usize>,
}

struct Scope {
    id: usize,
    kind: ScopeKind,
}

enum ScopeKind {
    Function {
        start_of_params: usize,
        function_id: u32,
        is_main: bool,
    },
    Local {
        /// Pre-compiled nodes that can substitute references into scope
        values: Vec<ir::Expr>,
    },
    TyLocal {
        types: Vec<ir::Ty>,
    },
}

impl<'a> Lowerer<'a> {
    fn new(root_module: &'a pr::ModuleDef) -> Self {
        Self {
            root_module,

            is_main_a_func: false,
            program_input_ty: None,

            scopes: vec![],
            def_dependencies: Default::default(),
            type_defs: Default::default(),
            type_defs_queue: Default::default(),

            generator_function_scope: Default::default(),
            generator_var_binding: Default::default(),
        }
    }

    /// Lowers a reference to native functions. If target function is not native, it returns None.
    #[tracing::instrument(name = "leed", skip_all)]
    fn lower_ref_to_native(&mut self, path: &pr::Path) -> Result<Option<ir::ExprKind>> {
        if path.as_steps() == [NS_STD, "default"] {
            // special case: evaluate std::default in lowerer
            return Ok(None);
        }

        // validate that it is an external definition
        let def = self.root_module.get(path);
        let def = def.unwrap_or_else(|| panic!("{path} does not exist"));
        let expr = def.kind.as_expr().unwrap();
        let expr = &expr.value;
        if !is_native_func(expr) {
            return Ok(None);
        }

        // return ptr to a native func
        let external_symbol_id = path.iter().join("::");
        Ok(Some(ir::ExprKind::Pointer(ir::Pointer::External(
            ir::ExternalPtr {
                id: external_symbol_id,
            },
        ))))
    }

    #[tracing::instrument(name = "led", skip_all, fields(p = path.to_string()))]
    fn lower_expr_def(&mut self, path: &pr::Path, ty_args: Vec<pr::Ty>) -> Result<ir::Expr> {
        let def = self.root_module.get(path);
        let def = def.unwrap_or_else(|| panic!("{path} does not exist"));
        let expr = def.kind.as_expr().unwrap();
        let expr = *expr.value.clone();

        if path.as_steps() == [NS_STD, "default"] {
            // special case: evaluate std::default in lowerer
            let ty_arg = ty_args.into_iter().next().unwrap();
            return Ok(self.impl_std_default(ty_arg));
        }

        if is_native_func(&expr) {
            // Usually, this is lowered earlier, when lowering pointers.
            // But for top-level external symbols, we do it here.

            let external_symbol_id = path.iter().join("::");
            let kind = ir::ExprKind::Pointer(ir::Pointer::External(ir::ExternalPtr {
                id: external_symbol_id,
            }));
            let ty = self.lower_ty(expr.ty.clone().unwrap());
            return Ok(ir::Expr { kind, ty });
        }

        let mut expr = expr;

        if let pr::TyKind::Func(ty_func) = &expr.ty.as_ref().unwrap().kind
            && !ty_func.ty_params.is_empty()
        {
            // replace refs to type params with inferred type args
            // For example:
            // - `func identity<T>(x: T) -> x` contains type param T,
            // - `identity(false)` instantiates param into arg that is inferred to be bool,
            // - when identify is lowered, we finalize the function into `func (x: bool): bool -> x`.
            let mut mapping = HashMap::<pr::Ref, pr::Ty>::new();
            for (gtp_index, arg) in ty_args.into_iter().enumerate() {
                mapping.insert(
                    pr::Ref::Local {
                        scope: expr.scope_id.unwrap(),
                        offset: gtp_index,
                    },
                    arg,
                );
            }
            expr = utils::TypeReplacer::on_expr(expr, mapping);
        }

        let res = self.lower_expr(&expr)?;

        Ok(res)
    }

    #[tracing::instrument(name = "le", skip_all)]
    fn lower_expr(&mut self, expr: &pr::Expr) -> Result<ir::Expr> {
        tracing::trace!("lower_expr: {expr:?}");
        let ty = expr.ty.as_ref().unwrap();

        let kind = match &expr.kind {
            pr::ExprKind::Literal(lit) => {
                ir::ExprKind::Literal(self.lower_literal(lit, ty).with_span(expr.span)?)
            }

            pr::ExprKind::Tuple(fields) => ir::ExprKind::Tuple(
                fields
                    .iter()
                    .map(|f| -> Result<ir::TupleField> {
                        Ok(ir::TupleField {
                            expr: self.lower_expr(&f.expr)?,
                            unpack: f.unpack,
                        })
                    })
                    .try_collect()?,
            ),
            pr::ExprKind::Array(items) => ir::ExprKind::Array(self.lower_exprs(items)?),
            pr::ExprKind::Variant(variant) => {
                let inner = if let Some(inner) = &variant.inner {
                    self.lower_expr(inner)?
                } else {
                    ir::Expr {
                        kind: ir::ExprKind::Tuple(vec![]),
                        ty: ir::Ty::new(ir::TyKind::Tuple(vec![])),
                    }
                };
                let (ty, _) = self.get_ty_mat_pr(ty);
                let variants = ty.kind.as_enum().unwrap();
                let (tag, _) = variants
                    .iter()
                    .find_position(|v| v.name == variant.name)
                    .unwrap();

                ir::ExprKind::EnumVariant(Box::new(ir::EnumVariant {
                    tag: tag as u64,
                    inner,
                }))
            }
            pr::ExprKind::Lookup { base, lookup } => {
                // At this stage, ty vars and ty params should have all been compiled away
                // and we can expect the base to be a concrete tuple.
                // This means we can iterate over the fields and find the correct tuple offset that way.
                let position = match lookup {
                    pr::Lookup::Position(position) => *position as u16,

                    pr::Lookup::Name(name) => {
                        let base_ty = base.ty.as_ref().unwrap();
                        let mut fields = self.tuple_iter_fields(base_ty).enumerate();

                        let res = fields.find(|(_, f)| f.matches_name(name));
                        let (position, _) = res.unwrap();
                        position as u16
                    }
                };

                let base = self.lower_expr(base)?;
                ir::ExprKind::TupleLookup(Box::new(ir::TupleLookup { base, position }))
            }

            pr::ExprKind::Call(call) => ir::ExprKind::Call(Box::new(ir::Call {
                function: self.lower_expr(&call.subject)?,
                args: (call.args.iter())
                    .map(|a| self.lower_expr(&a.expr))
                    .try_collect()?,
            })),
            pr::ExprKind::Func(func) => {
                let function_id = self.generator_function_scope.next() as u32;
                let scope = Scope {
                    id: expr.scope_id.unwrap(),
                    kind: ScopeKind::Function {
                        function_id,
                        start_of_params: func.ty_params.len(),
                        is_main: self.is_main_a_func,
                    },
                };

                self.scopes.push(scope);
                self.is_main_a_func = false;
                let body = self.lower_expr(func.body.as_ref().unwrap())?;
                self.scopes.pop();

                let func = ir::Function {
                    id: function_id,
                    body,
                };

                ir::ExprKind::Function(Box::new(func))
            }

            pr::ExprKind::Ident(_) => match expr.target.as_ref().unwrap() {
                pr::Ref::Local { scope, offset } => {
                    let scope = self.scopes.iter().find(|s| s.id == *scope).unwrap();

                    match &scope.kind {
                        ScopeKind::Function {
                            start_of_params,
                            function_id,
                            is_main,
                        } => {
                            let param_position = (*offset - start_of_params) as u8;

                            if !*is_main {
                                ir::ExprKind::Pointer(ir::Pointer::Parameter(ir::ParameterPtr {
                                    function_id: *function_id,
                                    param_position,
                                }))
                            } else {
                                // special case: this is reference to param of the main func,
                                // which is translated into func with exactly one param.
                                let param_ref = ir::ExprKind::Pointer(ir::Pointer::Parameter(
                                    ir::ParameterPtr {
                                        function_id: *function_id,
                                        param_position: 0,
                                    },
                                ));
                                let input_ty = self.program_input_ty.as_ref();
                                if input_ty.is_some_and(|(_, packed)| *packed) {
                                    // if original function params have been packed into a tuple,
                                    // we also need to also inject tuple lookup.
                                    ir::ExprKind::TupleLookup(Box::new(ir::TupleLookup {
                                        base: ir::Expr {
                                            kind: param_ref,
                                            ty: self.program_input_ty.clone().unwrap().0,
                                        },
                                        position: param_position as u16,
                                    }))
                                } else {
                                    param_ref
                                }
                            }
                        }
                        ScopeKind::Local { values } => {
                            values.get(*offset).map(|e| e.kind.clone()).unwrap()
                        }
                        ScopeKind::TyLocal { .. } => {
                            unreachable!()
                        }
                    }
                }

                pr::Ref::Global(ref_) => self.lower_ref_global(ref_, &expr.ty_args)?,
            },

            pr::ExprKind::Match(match_) => {
                let subject = self.lower_expr(&match_.subject)?;
                let subject_id = self.generator_var_binding.next() as u32;
                let subject_ref = ir::Expr {
                    kind: ir::ExprKind::Pointer(ir::Pointer::Binding(subject_id)),
                    ty: subject.ty.clone(),
                };

                let mut switch_branches = Vec::new();

                for branch in &match_.branches {
                    // compile the condition
                    let condition =
                        self.lower_pattern_to_condition(&subject_ref, &branch.pattern)?;
                    let condition = condition.unwrap_or_else(|| ir::Expr {
                        kind: ir::ExprKind::Literal(ir::Literal::bool(true)),
                        ty: ir::Ty::new(ir::TyPrimitive::bool),
                    });

                    // collect pattern scope
                    let mut values = Vec::new();
                    self.collect_pattern_binds(&branch.pattern, subject_ref.clone(), &mut values);
                    self.scopes.push(Scope {
                        id: branch.value.scope_id.unwrap(),
                        kind: ScopeKind::Local { values },
                    });

                    // compile value
                    let value = self.lower_expr(&branch.value)?;

                    self.scopes.pop();

                    switch_branches.push(ir::SwitchBranch { condition, value })
                }

                let ty = self.lower_ty(ty.clone());
                let switch = ir::Expr {
                    kind: ir::ExprKind::Switch(switch_branches),
                    ty: ty.clone(),
                };

                let kind = ir::ExprKind::Binding(Box::new(ir::Binding {
                    id: subject_id,
                    expr: subject,
                    main: switch,
                }));
                return Ok(ir::Expr { kind, ty });
            }

            pr::ExprKind::If(if_else) => {
                let first = ir::SwitchBranch {
                    condition: self.lower_expr(&if_else.condition)?,
                    value: self.lower_expr(&if_else.then)?,
                };
                let second = ir::SwitchBranch {
                    condition: ir::Expr {
                        kind: ir::ExprKind::Literal(ir::Literal::bool(true)),
                        ty: ir::Ty::new(ir::TyPrimitive::bool),
                    },
                    value: self.lower_expr(&if_else.els)?,
                };
                ir::ExprKind::Switch(vec![first, second])
            }

            pr::ExprKind::VarBinding(binding) => {
                let bound = self.lower_expr(&binding.bound)?;
                let bound_id = self.generator_var_binding.next() as u32;
                let bound_ref = ir::Expr {
                    kind: ir::ExprKind::Pointer(ir::Pointer::Binding(bound_id)),
                    ty: bound.ty.clone(),
                };

                // prepare scope
                self.scopes.push(Scope {
                    id: expr.scope_id.unwrap(),
                    kind: ScopeKind::Local {
                        values: vec![bound_ref],
                    },
                });

                // compile value
                let main = self.lower_expr(&binding.main)?;

                self.scopes.pop();

                ir::ExprKind::Binding(Box::new(ir::Binding {
                    id: bound_id,
                    expr: bound,
                    main,
                }))
            }

            // consumed by type resolver
            pr::ExprKind::TypeAnnotation(_) => unreachable!(),

            // desugared away
            pr::ExprKind::Nested(_)
            | pr::ExprKind::Binary(_)
            | pr::ExprKind::Unary(_)
            | pr::ExprKind::Range(_)
            | pr::ExprKind::FString(_)
            | pr::ExprKind::FuncShort(_) => unreachable!(),
        };
        Ok(ir::Expr {
            kind,
            ty: self.lower_ty(expr.ty.clone().unwrap()),
        })
    }

    fn lower_ref_global(&mut self, ref_: &pr::Path, ty_args: &[pr::Ty]) -> Result<ir::ExprKind> {
        // if native ref, return ir::Ptr::Native
        if let Some(ptr) = self.lower_ref_to_native(ref_)? {
            return Ok(ptr);
        }

        // for other refs, return ir::Ptr::Binding and remember to lower them later
        let reference = (ref_.clone(), ty_args.to_vec());
        let entry = self.def_dependencies.entry(reference);
        let binding_id = match entry {
            indexmap::map::Entry::Occupied(e) => *e.get(),
            indexmap::map::Entry::Vacant(e) => {
                let id = self.generator_var_binding.next() as u32;
                e.insert(id);
                id
            }
        };
        Ok(ir::ExprKind::Pointer(ir::Pointer::Binding(binding_id)))
    }

    fn lower_literal(&mut self, lit: &pr::Literal, ty: &pr::Ty) -> Result<ir::Literal> {
        Ok(match lit {
            pr::Literal::Number(_) => {
                let (ty_mat, frame_name) = self.get_ty_mat_pr(ty);

                if frame_name.is_some_and(|n| n.as_steps() == [NS_STD, "Decimal"]) {
                    return Ok(ir::Literal::int64(lit.as_decimal().unwrap()));
                }

                let prim = ty_mat.kind.as_primitive().ok_or_else(|| {
                    Diagnostic::new_assert("expected literal to be of primitive type")
                })?;

                match prim {
                    pr::TyPrimitive::int8 => ir::Literal::int8(lit.as_integer().unwrap() as i8),
                    pr::TyPrimitive::int16 => ir::Literal::int16(lit.as_integer().unwrap() as i16),
                    pr::TyPrimitive::int32 => ir::Literal::int32(lit.as_integer().unwrap() as i32),
                    pr::TyPrimitive::int64 => ir::Literal::int64(lit.as_integer().unwrap() as i64),
                    pr::TyPrimitive::uint8 => ir::Literal::uint8(lit.as_integer().unwrap() as u8),
                    pr::TyPrimitive::uint16 => {
                        ir::Literal::uint16(lit.as_integer().unwrap() as u16)
                    }
                    pr::TyPrimitive::uint32 => {
                        ir::Literal::uint32(lit.as_integer().unwrap() as u32)
                    }
                    pr::TyPrimitive::uint64 => ir::Literal::uint64(lit.as_integer().unwrap()),
                    pr::TyPrimitive::float32 => {
                        ir::Literal::float32(lit.as_float().unwrap() as f32)
                    }
                    pr::TyPrimitive::float64 => ir::Literal::float64(lit.as_float().unwrap()),
                    _ => {
                        return Err(Diagnostic::new_assert(
                            "number literal is not of a number type?",
                        ));
                    }
                }
            }
            pr::Literal::Boolean(v) => ir::Literal::bool(*v),
            pr::Literal::Text(v) => ir::Literal::text(v.clone()),

            pr::Literal::Date(date) => {
                let Some(epoch_days) = date.to_epoch_days() else {
                    // TODO: this should have been validated earlier (probably resolver)
                    return Err(Diagnostic::new_custom("invalid date"));
                };

                ir::Literal::int32(epoch_days)
            }
            pr::Literal::Time(time) => ir::Literal::int64(time.to_microseconds()),

            pr::Literal::DateTime(date, time) => {
                let Some(epoch_days) = date.to_epoch_days() else {
                    // TODO: this should have been validated earlier (probably resolver)
                    return Err(Diagnostic::new_custom("invalid date"));
                };

                // convert to timestamp (at UTC, excluding leap seconds)
                let date_micros = epoch_days as i64 * 24 * 60 * 60 * 1000 * 1000;
                ir::Literal::int64(date_micros + time.to_microseconds())
            }
        })
    }

    fn lower_pattern_to_condition(
        &mut self,
        subject: &ir::Expr,
        pattern: &pr::Pattern,
    ) -> Result<Option<ir::Expr>> {
        match &pattern.kind {
            // match a enum variant
            pr::PatternKind::Enum(variant_name, inner) => {
                let tag = get_pattern_enum_eq_tag(subject, pattern, variant_name);

                let mut expr = ir::Expr {
                    kind: ir::ExprKind::EnumEq(Box::new(ir::EnumEq {
                        subject: subject.clone(),
                        tag: tag as u64,
                    })),
                    ty: ir::Ty::new(ir::TyPrimitive::bool),
                };

                if let Some(inner) = inner {
                    let subject_ty = self.get_ty_mat(subject.ty.clone());

                    let subject_variants = subject_ty.kind.into_enum().unwrap();
                    let inner_ty = subject_variants.into_iter().nth(tag).unwrap().ty;
                    let inner_ref = ir::Expr {
                        kind: ir::ExprKind::EnumUnwrap(Box::new(ir::EnumUnwrap {
                            subject: subject.clone(),
                            tag: tag as u64,
                        })),
                        ty: inner_ty,
                    };

                    let inner_cond = self.lower_pattern_to_condition(&inner_ref, inner)?;

                    if let Some(inner_cond) = inner_cond {
                        expr = new_bool_bin_func("std::and", expr, inner_cond);
                    }
                }
                Ok(Some(expr))
            }

            // match a literal value
            pr::PatternKind::Literal(lit) => {
                let subject_ty = self.get_ty_mat(subject.ty.clone());
                let subject_ty_pr = pr::Ty::from(subject_ty.clone());
                let lit = ir::Expr {
                    kind: ir::ExprKind::Literal(
                        self.lower_literal(lit, &subject_ty_pr)
                            .with_span(Some(pattern.span))?,
                    ),
                    ty: subject_ty.clone(),
                };

                Ok(Some(new_bool_bin_func("std::eq", subject.clone(), lit)))
            }

            // AnyOf matches if any of branches match
            pr::PatternKind::AnyOf(branches) => {
                let mut conditions = Vec::with_capacity(branches.len());
                for br in branches {
                    conditions.extend(self.lower_pattern_to_condition(subject, br)?);
                }

                let mut conditions = conditions.into_iter().rev();
                let Some(mut res) = conditions.next() else {
                    return Ok(None);
                };
                for c in conditions {
                    res = new_bool_bin_func("std::or", c, res);
                }
                Ok(Some(res))
            }

            // bind always matches
            pr::PatternKind::Bind(_) => Ok(None),
        }
    }

    fn lower_exprs(&mut self, exprs: &[pr::Expr]) -> Result<Vec<ir::Expr>> {
        exprs.iter().map(|e| self.lower_expr(e)).collect()
    }

    fn lower_def_dependencies(&mut self, main: ir::Expr, project: &Project) -> Result<ir::Expr> {
        if self.def_dependencies.is_empty() {
            return Ok(main);
        }

        let main_ty = main.ty.clone();
        let main_func_ty = main.ty.kind.as_function().unwrap().clone();

        // construct a new main function
        let f_id = self.generator_function_scope.next() as u32;
        let mut main = ir::Expr {
            kind: ir::ExprKind::Call(Box::new(ir::Call {
                function: main,
                args: vec![ir::Expr {
                    kind: ir::ExprKind::Pointer(ir::Pointer::Parameter(ir::ParameterPtr {
                        function_id: f_id,
                        param_position: 0_u8,
                    })),
                    ty: main_func_ty.params[0].clone(),
                }],
            })),
            ty: main_func_ty.body.clone(),
        };

        // fold each of the bindings
        let mut bindings = HashMap::new();
        let mut i = 0;
        loop {
            let Some((reference, id)) = self.def_dependencies.get_index(i) else {
                break;
            };
            i += 1;
            let reference = reference.clone();
            let id = *id;

            // lower
            let expr = self.lower_expr_def(&reference.0, reference.1)?;

            let entry: &mut Vec<_> = bindings.entry(reference.0).or_insert_with(Vec::new);
            entry.push((id, expr));
        }

        // wrap the call into bindings, in resolution order
        for o_group in project.ordering.iter().rev() {
            assert_eq!(o_group.len(), 1);
            let path = &o_group[0];

            for (id, expr) in bindings.remove(path).unwrap_or_default() {
                let ty = main.ty.clone();
                main = ir::Expr::new(ir::Binding { id, expr, main }, ty);
            }
        }
        // same for remaining bindings that come from project dependencies
        for (id, expr) in bindings.into_values().flatten() {
            let ty = main.ty.clone();
            main = ir::Expr::new(ir::Binding { id, expr, main }, ty);
        }

        // place bindings into function body
        Ok(ir::Expr {
            kind: ir::ExprKind::Function(Box::new(ir::Function {
                id: f_id,
                body: main,
            })),
            ty: main_ty,
        })
    }

    #[tracing::instrument(name = "lt", skip_all)]
    fn lower_ty(&mut self, ty: pr::Ty) -> ir::Ty {
        tracing::trace!("lower ty: {}", crate::printer::print_ty(&ty));

        if let Some(target) = ty.target {
            match target {
                pr::Ref::Global(fq) => {
                    self.type_defs_queue.push_back(fq.clone());
                    tracing::debug!("lower ty ident: {}", fq);
                    return ir::Ty {
                        kind: ir::TyKind::Ident(ir::Path(fq.into_iter().collect_vec())),
                        layout: None,
                        name: ty.name,
                        variants_recursive: vec![],
                    };
                }
                pr::Ref::Local { scope, offset } => {
                    let scope = self.scopes.iter().find(|s| s.id == scope).unwrap();

                    match &scope.kind {
                        ScopeKind::TyLocal { types } => {
                            return types[offset].clone();
                        }

                        ScopeKind::Function { .. } | ScopeKind::Local { .. } => unreachable!(),
                    }
                }
            }
        }

        // this part is direct no-op mapping
        let kind = match ty.kind {
            pr::TyKind::Primitive(primitive) => {
                let primitive = match primitive {
                    pr::TyPrimitive::int8 => ir::TyPrimitive::int8,
                    pr::TyPrimitive::int16 => ir::TyPrimitive::int16,
                    pr::TyPrimitive::int32 => ir::TyPrimitive::int32,
                    pr::TyPrimitive::int64 => ir::TyPrimitive::int64,
                    pr::TyPrimitive::uint8 => ir::TyPrimitive::uint8,
                    pr::TyPrimitive::uint16 => ir::TyPrimitive::uint16,
                    pr::TyPrimitive::uint32 => ir::TyPrimitive::uint32,
                    pr::TyPrimitive::uint64 => ir::TyPrimitive::uint64,
                    pr::TyPrimitive::float32 => ir::TyPrimitive::float32,
                    pr::TyPrimitive::float64 => ir::TyPrimitive::float64,
                    pr::TyPrimitive::bool => ir::TyPrimitive::bool,
                    pr::TyPrimitive::text => ir::TyPrimitive::text,
                };
                ir::TyKind::Primitive(primitive)
            }
            pr::TyKind::Tuple(fields) => {
                let mut r = Vec::with_capacity(fields.len());

                for f in fields {
                    let name = f.name.clone();
                    let ty = self.lower_ty(f.ty);
                    if f.unpack {
                        let ir::TyKind::Tuple(fields) = self.get_ty_mat(ty).kind else {
                            panic!("expected a tuple type in unpack");
                        };
                        r.extend(fields);
                    } else {
                        r.push(ir::TyTupleField { name, ty });
                    }
                }

                ir::TyKind::Tuple(r)
            }
            pr::TyKind::Array(items_ty) => ir::TyKind::Array(Box::new(self.lower_ty(*items_ty))),
            pr::TyKind::Enum(variants) => ir::TyKind::Enum(
                variants
                    .into_iter()
                    .map(|v| ir::TyEnumVariant {
                        name: v.name,
                        ty: self.lower_ty(v.ty),
                    })
                    .collect(),
            ),
            pr::TyKind::Ident(_) => unreachable!(),
            pr::TyKind::Func(func) => ir::TyKind::Function(Box::new(ir::TyFunction {
                params: func
                    .params
                    .into_iter()
                    .map(|p| self.lower_ty(p.ty.unwrap()))
                    .collect(),
                body: self.lower_ty(*func.body.clone().unwrap()),
            })),
            pr::TyKind::TupleComprehension(comp) => {
                let tuple = self.lower_ty(*comp.tuple);
                let ir::TyKind::Tuple(fields) = self.get_ty_mat(tuple).kind else {
                    panic!("expected a tuple type in unpack");
                };

                let scope_id = ty.scope_id.unwrap();
                self.scopes.push(Scope {
                    id: scope_id,
                    kind: ScopeKind::TyLocal { types: vec![] },
                });

                let mut r = Vec::with_capacity(fields.len());
                for field in fields {
                    // setup the scope with field.ty,
                    // so comp.variable_ty references can be replaced with field.ty
                    let ScopeKind::TyLocal { types } = &mut self.scopes.last_mut().unwrap().kind
                    else {
                        panic!()
                    };
                    *types = vec![field.ty];

                    // fold (and replace references)
                    let f_ty = self.lower_ty(*comp.body_ty.clone());

                    r.push(ir::TyTupleField {
                        name: if comp.body_name.is_some() {
                            field.name
                        } else {
                            None
                        },
                        ty: f_ty,
                    });
                }

                self.scopes.pop();

                ir::TyKind::Tuple(r)
            }
        };

        ir::Ty {
            kind,
            name: ty.name,
            layout: None,
            variants_recursive: vec![],
        }
    }

    fn get_ty_mat(&mut self, ty: ir::Ty) -> ir::Ty {
        if let ir::TyKind::Ident(fq_path) = &ty.kind {
            let path = pr::Path::new(fq_path.0.clone());

            // ensure it is lowered
            self.lower_ty_def(path.clone());

            // fetch it
            let ty = self.type_defs.get(&path).unwrap().clone();

            // re-run get_ty_mat
            self.get_ty_mat(ty)
        } else {
            ty
        }
    }

    /// Returns material type and the named of it's inner-most frame.
    fn get_ty_mat_pr(&self, ty: &'a pr::Ty) -> (&'a pr::Ty, Option<&'a pr::Path>) {
        let pr::TyKind::Ident(_) = &ty.kind else {
            return (ty, None);
        };
        let Some(pr::Ref::Global(target_fq)) = &ty.target else {
            panic!();
        };
        let def = self.root_module.get(target_fq).unwrap();
        let def = def.kind.as_ty().unwrap();

        let (ty, frame_name) = self.get_ty_mat_pr(&def.ty);

        let frame_name = frame_name.or(if def.is_framed { Some(target_fq) } else { None });

        (ty, frame_name)
    }

    fn tuple_iter_fields(
        &'a self,
        ty: &'a pr::Ty,
    ) -> Box<dyn Iterator<Item = Cow<'a, pr::TyTupleField>> + 'a> {
        let (base_ty, _) = self.get_ty_mat_pr(ty);
        match &base_ty.kind {
            pr::TyKind::Tuple(ty_fields) => Box::new(ty_fields.iter().flat_map(|f| {
                if f.unpack {
                    self.tuple_iter_fields(&f.ty)
                } else {
                    Box::new(Some(Cow::Borrowed(f)).into_iter())
                }
            })),
            pr::TyKind::TupleComprehension(comp) => {
                // this is very inefficient: we are basically materializing the type on-the-fly,
                // and then pick out just one field.

                Box::new(self.tuple_iter_fields(&comp.tuple).map(|var_input| {
                    let var_input = var_input.into_owned();

                    let var_ref = pr::Ref::Local {
                        scope: base_ty.scope_id.unwrap(),
                        offset: 0,
                    };
                    let mapping = HashMap::from_iter(Some((var_ref, var_input.ty)));
                    let ty = utils::TypeReplacer::on_ty(*comp.body_ty.clone(), mapping);

                    Cow::Owned(pr::TyTupleField {
                        name: if comp.body_name.is_some() {
                            var_input.name
                        } else {
                            None
                        },
                        ty,
                        unpack: false,
                    })
                }))
            }
            _ => panic!("expected a tuple: {ty:?}"),
        }
    }

    /// Lowers a type definition
    fn lower_ty_def(&mut self, path: pr::Path) {
        if self.type_defs.contains_key(&path) {
            return;
        }

        let def = self.root_module.get(&path).unwrap();
        let def = def.kind.as_ty().unwrap().clone();

        let ty = self.lower_ty(def.ty);

        self.type_defs.insert(path, ty);
    }

    fn lower_ty_defs_queue(&mut self) {
        while let Some(path) = self.type_defs_queue.pop_front() {
            self.lower_ty_def(path)
        }
    }

    /// For a given pattern, this function collects all scope entries (variable bindings)
    /// and returns ir nodes that can be used as a reference to this entry.
    fn collect_pattern_binds(
        &mut self,
        pattern: &pr::Pattern,
        subject_ref: ir::Expr,
        entries: &mut Vec<ir::Expr>,
    ) {
        match &pattern.kind {
            pr::PatternKind::Bind(_) => {
                entries.push(subject_ref);
            }

            pr::PatternKind::AnyOf(branches) => {
                assert!(branches.len() >= 2);

                // collect variables for each branch
                let mut branches_vars = Vec::new();
                let mut var_types: Option<Vec<ir::Ty>> = None;
                for br in branches {
                    let mut br_vars = Vec::new();
                    self.collect_pattern_binds(br, subject_ref.clone(), &mut br_vars);

                    if var_types.is_none() {
                        var_types = Some(br_vars.iter().map(|e| e.ty.clone()).collect());
                    }

                    branches_vars.push(br_vars.into_iter());
                }

                // each branch has the same number of vars (enforced at name checking)
                // TODO: ... which are of the same type (enforced as type checking)

                // for each bound variable
                for var_ty in var_types.unwrap() {
                    // construct a switch that will pick correct variable
                    let mut cases = Vec::new();
                    for (branch, vars) in std::iter::zip(branches, branches_vars.iter_mut()) {
                        let var = vars.next().unwrap();

                        let condition = self
                            .lower_pattern_to_condition(&subject_ref, branch)
                            .unwrap();
                        let condition = condition.unwrap_or_else(|| ir::Expr {
                            kind: ir::ExprKind::Literal(ir::Literal::bool(true)),
                            ty: ir::Ty::new(ir::TyPrimitive::bool),
                        });
                        cases.push(ir::SwitchBranch {
                            condition,
                            value: var,
                        });
                    }
                    let var = ir::Expr {
                        kind: ir::ExprKind::Switch(cases),
                        ty: var_ty,
                    };
                    entries.push(var);
                }
            }

            pr::PatternKind::Enum(variant_name, inner) => {
                let tag = get_pattern_enum_eq_tag(&subject_ref, pattern, variant_name);

                if let Some(inner) = inner {
                    let subject_ty = self.get_ty_mat(subject_ref.ty.clone());

                    let subject_variants = subject_ty.kind.into_enum().unwrap();
                    let inner_ty = subject_variants.into_iter().nth(tag).unwrap().ty;
                    let inner_ref = ir::Expr {
                        kind: ir::ExprKind::EnumUnwrap(Box::new(ir::EnumUnwrap {
                            subject: subject_ref,
                            tag: tag as u64,
                        })),
                        ty: inner_ty,
                    };

                    self.collect_pattern_binds(inner, inner_ref, entries)
                }
            }

            pr::PatternKind::Literal(_) => {}
        }
    }

    /// Massages an expression for being the entry-point of a program.
    /// In particular, this makes sure that it is:
    /// - a function,
    /// - with exactly one param (which is named input).
    fn prepare_entry_point(&mut self, mut main: ir::Expr) -> ir::Expr {
        if let ir::TyKind::Function(func) = &mut main.ty.kind {
            // change type from `func (a, b, c): d` into `func ({a, b, c}): d`
            if func.params.len() != 1 {
                let fields = func
                    .params
                    .drain(..)
                    .map(|ty| ir::TyTupleField { ty, name: None })
                    .collect();
                func.params = vec![ir::Ty::new(ir::TyKind::Tuple(fields))];
            }
            main
        } else {
            self.wrap_into_main_func(main)
        }
    }

    fn wrap_into_main_func(&mut self, body: ir::Expr) -> ir::Expr {
        ir::Expr {
            ty: ir::Ty::new(ir::TyFunction {
                body: body.ty.clone(),
                params: vec![ir::Ty::new_unit()],
            }),
            kind: ir::ExprKind::Function(Box::new(ir::Function {
                id: self.generator_function_scope.next() as u32,
                body,
            })),
        }
    }

    fn impl_std_default(&mut self, ty: pr::Ty) -> ir::Expr {
        let ty = self.lower_ty(ty);
        let body = self.construct_default_for_ty(ty.clone());

        ir::Expr {
            kind: ir::ExprKind::Function(Box::new(ir::Function {
                id: self.generator_function_scope.next() as u32,
                body,
            })),
            ty: ir::Ty::new(ir::TyFunction {
                body: ty,
                params: vec![],
            }),
        }
    }

    fn construct_default_for_ty(&mut self, ty: ir::Ty) -> ir::Expr {
        let kind = match self.get_ty_mat(ty.clone()).kind {
            ir::TyKind::Primitive(prim) => ir::ExprKind::Literal(match prim {
                ir::TyPrimitive::bool => ir::Literal::bool(false),
                ir::TyPrimitive::int8 => ir::Literal::int8(0),
                ir::TyPrimitive::int16 => ir::Literal::int16(0),
                ir::TyPrimitive::int32 => ir::Literal::int32(0),
                ir::TyPrimitive::int64 => ir::Literal::int64(0),
                ir::TyPrimitive::uint8 => ir::Literal::uint8(0),
                ir::TyPrimitive::uint16 => ir::Literal::uint16(0),
                ir::TyPrimitive::uint32 => ir::Literal::uint32(0),
                ir::TyPrimitive::uint64 => ir::Literal::uint64(0),
                ir::TyPrimitive::float32 => ir::Literal::float32(0.0),
                ir::TyPrimitive::float64 => ir::Literal::float64(0.0),
                ir::TyPrimitive::text => ir::Literal::text("".into()),
            }),
            ir::TyKind::Array(_) => ir::ExprKind::Array(vec![]),
            ir::TyKind::Tuple(ty_fields) => ir::ExprKind::Tuple(
                ty_fields
                    .into_iter()
                    .map(|f| ir::TupleField {
                        expr: self.construct_default_for_ty(f.ty),
                        unpack: false,
                    })
                    .collect(),
            ),
            ir::TyKind::Enum(ty_enum_variants) => {
                // TODO: ensure that enums have at least one variant
                // TODO: ensure that enums are not recursive in the first field

                let variant = ty_enum_variants.into_iter().next().unwrap();
                ir::ExprKind::EnumVariant(Box::new(ir::EnumVariant {
                    tag: 0,
                    inner: self.construct_default_for_ty(variant.ty),
                }))
            }

            ir::TyKind::Function(_) => panic!(),
            ir::TyKind::Ident(_) => unreachable!(),
        };

        ir::Expr { kind, ty }
    }
}

fn new_bool_bin_func(func_id: &str, left: ir::Expr, right: ir::Expr) -> ir::Expr {
    let bool_ty = ir::Ty::new(ir::TyPrimitive::bool);
    ir::Expr {
        kind: ir::ExprKind::Call(Box::new(ir::Call {
            function: ir::Expr {
                kind: ir::ExprKind::Pointer(ir::Pointer::External(ir::ExternalPtr {
                    id: func_id.to_string(),
                })),
                ty: ir::Ty::new(ir::TyFunction {
                    params: vec![left.ty.clone(), right.ty.clone()],
                    body: bool_ty.clone(),
                }),
            },
            args: vec![left, right],
        })),
        ty: bool_ty,
    }
}

fn get_pattern_enum_eq_tag(subject: &ir::Expr, pattern: &pr::Pattern, variant_name: &str) -> usize {
    if let Some(tag) = pattern.variant_tag {
        tag
    } else {
        // this happens when subject of the pattern is a type var
        // and we cannot determine the position of the variant until the
        // concrete type is known. Which is now, after instantiating all type params.
        let variants = subject.ty.kind.as_enum().unwrap();
        let (tag, _) = variants
            .iter()
            .enumerate()
            .find(|(_, v)| v.name == variant_name)
            .unwrap();
        tag
    }
}

pub fn lower_type_defs(project: &Project) -> ir::Module {
    let mut lowerer = Lowerer::new(&project.root_module);

    let mut module = ir::Module { decls: Vec::new() };

    for (name, def) in project.root_module.iter_defs_re() {
        if name.starts_with_part(NS_STD) {
            continue;
        }

        match &def.kind {
            pr::DefKind::Module(_) => {
                unreachable!("iter_def_re does not return modules");
            }

            pr::DefKind::Expr(expr) => {
                let expr = &expr.value;
                let ty = lowerer.lower_ty(expr.ty.clone().unwrap());

                module.insert(name.as_steps(), ir::Decl::Var(ty));
            }

            pr::DefKind::Ty(_) => {
                lowerer.type_defs_queue.push_back(name);
            }

            pr::DefKind::Import(_) => {}

            pr::DefKind::Unresolved(_) => panic!(),
        }
    }

    lowerer.lower_ty_defs_queue();
    let types = order_ty_defs(lowerer.type_defs, project);
    for ty in types {
        module.insert(&ty.name.0, ir::Decl::Type(ty.ty));
    }
    module
}

fn order_ty_defs(mut by_name: HashMap<pr::Path, ir::Ty>, project: &Project) -> Vec<ir::TyDef> {
    // apply project order needed types
    let mut tys_proj = Vec::with_capacity(by_name.len());
    for group in &project.ordering {
        for p in group {
            if let Some(ty) = by_name.remove(p) {
                tys_proj.push(ir::TyDef {
                    name: ir::Path(p.clone().into_iter().collect()),
                    ty,
                });
            }
        }
    }

    // remaining types come from dependencies
    // (they should preceed types from this project)
    let tys_deps = by_name
        .into_iter()
        .sorted_by(|a, b| a.0.cmp(&b.0))
        .map(|(p, ty)| ir::TyDef {
            name: ir::Path(p.clone().into_iter().collect()),
            ty,
        });

    tys_deps.chain(tys_proj).collect()
}

/// Get the entry point's input type.
/// Returns the type and a bool indicating if the type is a tuple, packed from multiple input params.
fn get_entry_point_input(expr: &pr::Expr) -> (pr::Ty, bool) {
    let ty = expr.ty.as_ref().unwrap();
    let Some(ty_func) = ty.kind.as_func() else {
        return (pr::Ty::new(pr::TyKind::Tuple(vec![])), true);
    };

    if ty_func.params.len() == 1 {
        return (ty_func.params[0].ty.clone().unwrap(), false);
    }

    let ty = pr::Ty::new(pr::TyKind::Tuple(
        ty_func
            .params
            .iter()
            .map(|p| pr::TyTupleField {
                ty: p.ty.clone().unwrap(),
                unpack: false,
                name: None,
            })
            .collect(),
    ));
    (ty, true)
}

fn is_native_func(expr: &pr::Expr) -> bool {
    expr.kind.as_func().is_some_and(|x| x.body.is_none())
}
