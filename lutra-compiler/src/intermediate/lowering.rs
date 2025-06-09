use std::collections::{HashMap, VecDeque};

use indexmap::IndexMap;
use itertools::Itertools;
use lutra_bin::ir;

use crate::utils::{self, IdGenerator};
use crate::Result;
use crate::{decl, pr};

pub fn lower_expr(root_module: &decl::RootModule, main: &pr::Expr) -> ir::Program {
    let mut lowerer = Lowerer::new(root_module);

    let mut main = lowerer.lower_expr(main).unwrap();

    if !main.ty.kind.is_function() {
        main = lowerer.wrap_into_func(main);
    }

    let main = lowerer.lower_var_bindings(main);

    lowerer.lower_ty_defs_queue();
    let types = order_ty_defs(lowerer.type_defs, root_module);

    ir::Program { main, types }
}

pub fn lower_var(root_module: &decl::RootModule, path: &pr::Path) -> ir::Program {
    // lookup path
    let decl = root_module.module.get(path).unwrap();
    let expr = decl.into_expr().unwrap();
    let ty = expr.ty.clone();

    // construct ref to the path
    let mut expr = pr::Expr::new(path.clone());
    expr.ty = ty;
    expr.target = Some(pr::Ref::FullyQualified {
        to_decl: path.clone(),
        within: pr::Path::empty(),
    });

    // lower ref
    lower_expr(root_module, &expr)
}

struct Lowerer<'a> {
    root_module: &'a decl::RootModule,

    scopes: Vec<Scope>,
    var_bindings: IndexMap<(pr::Path, Vec<pr::Ty>), u32>,

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
    },
    Local {
        /// A pre-compiled node that can be used for each scope entry
        entries: Vec<ir::Expr>,
    },
}

impl<'a> Lowerer<'a> {
    fn new(root_module: &'a decl::RootModule) -> Self {
        Self {
            root_module,

            scopes: vec![],
            var_bindings: Default::default(),
            type_defs: Default::default(),
            type_defs_queue: Default::default(),

            generator_function_scope: Default::default(),
            generator_var_binding: Default::default(),
        }
    }

    #[tracing::instrument(name = "leed", skip_all)]
    fn lower_external_expr_decl(&mut self, path: &pr::Path) -> Result<Option<ir::ExprKind>> {
        let decl = self
            .root_module
            .module
            .get(path)
            .unwrap_or_else(|| panic!("{path} does not exist"));
        let expr = decl.into_expr().unwrap();

        if !matches!(expr.kind, pr::ExprKind::Internal) {
            return Ok(None);
        }

        let external_symbol_id = path.iter().join("::");
        Ok(Some(ir::ExprKind::Pointer(ir::Pointer::External(
            ir::ExternalPtr {
                id: external_symbol_id,
            },
        ))))
    }

    #[tracing::instrument(name = "led", skip_all, fields(p = path.to_string()))]
    fn lower_expr_decl(&mut self, path: &pr::Path, ty_args: Vec<pr::Ty>) -> Result<ir::Expr> {
        let decl = self
            .root_module
            .module
            .get(path)
            .unwrap_or_else(|| panic!("{path} does not exist"));
        let expr = decl.into_expr().unwrap().clone();

        // should have been lowered earlier
        assert!(!matches!(expr.kind, pr::ExprKind::Internal));

        let mut expr = expr;

        if let pr::TyKind::Func(ty_func) = &expr.ty.as_ref().unwrap().kind {
            if !ty_func.ty_params.is_empty() {
                // replace refs to type params with inferred type args
                // For example:
                // - `let identity = func<T>(x: T) -> x` contains type param T,
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
        }

        let res = self.lower_expr(&expr)?;

        Ok(res)
    }

    #[tracing::instrument(name = "le", skip_all)]
    fn lower_expr(&mut self, expr: &pr::Expr) -> Result<ir::Expr> {
        tracing::trace!("lower_expr: {expr:?}");
        let kind = match &expr.kind {
            pr::ExprKind::Literal(lit) => {
                let lit = match lit {
                    pr::Literal::Integer(v) => ir::Literal::Int(*v),
                    pr::Literal::Float(v) => ir::Literal::Float(*v),
                    pr::Literal::Boolean(v) => ir::Literal::Bool(*v),
                    pr::Literal::Text(v) => ir::Literal::Text(v.clone()),
                    pr::Literal::Date(_) => todo!(),
                    pr::Literal::Time(_) => todo!(),
                    pr::Literal::Timestamp(_) => todo!(),
                };
                ir::ExprKind::Literal(lit)
            }

            pr::ExprKind::Tuple(fields) => ir::ExprKind::Tuple(
                fields
                    .iter()
                    .map(|f| self.lower_expr(&f.expr))
                    .try_collect()?,
            ),
            pr::ExprKind::Array(items) => ir::ExprKind::Array(self.lower_exprs(items)?),
            pr::ExprKind::EnumVariant(variant) => {
                ir::ExprKind::EnumVariant(Box::new(ir::EnumVariant {
                    tag: variant.tag as u64,
                    inner: variant
                        .inner
                        .as_ref()
                        .map(|x| self.lower_expr(x))
                        .transpose()?
                        .unwrap_or_else(|| ir::Expr {
                            kind: ir::ExprKind::Tuple(vec![]),
                            ty: ir::Ty::new(ir::TyKind::Tuple(vec![])),
                        }),
                }))
            }
            pr::ExprKind::Indirection { base, field } => {
                ir::ExprKind::TupleLookup(Box::new(ir::TupleLookup {
                    base: self.lower_expr(base)?,
                    position: match field {
                        pr::IndirectionKind::Name(_) => todo!(),
                        pr::IndirectionKind::Position(position) => *position as u16,
                        pr::IndirectionKind::Star => todo!(),
                    },
                }))
            }

            pr::ExprKind::FuncCall(call) => ir::ExprKind::Call(Box::new(ir::Call {
                function: self.lower_expr(&call.func)?,
                args: self.lower_exprs(&call.args)?,
            })),
            pr::ExprKind::Func(func) => {
                let function_id = self.generator_function_scope.gen() as u32;
                let scope = Scope {
                    id: expr.scope_id.unwrap(),
                    kind: ScopeKind::Function {
                        function_id,
                        start_of_params: func.ty_params.len(),
                    },
                };

                self.scopes.push(scope);
                let body = self.lower_expr(&func.body)?;
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
                        } => {
                            let param_position = (*offset - start_of_params) as u8;

                            ir::ExprKind::Pointer(ir::Pointer::Parameter(ir::ParameterPtr {
                                function_id: *function_id,
                                param_position,
                            }))
                        }
                        ScopeKind::Local { entries } => {
                            entries.get(*offset).map(|e| e.kind.clone()).unwrap()
                        }
                    }
                }

                pr::Ref::FullyQualified {
                    to_decl,
                    within: _within,
                } => {
                    if let Some(ptr) = self.lower_external_expr_decl(to_decl)? {
                        ptr
                    } else {
                        let reference = (to_decl.clone(), expr.ty_args.clone());
                        let entry = self.var_bindings.entry(reference);
                        let binding_id = match entry {
                            indexmap::map::Entry::Occupied(e) => *e.get(),
                            indexmap::map::Entry::Vacant(e) => {
                                let id = self.generator_var_binding.gen() as u32;
                                e.insert(id);
                                id
                            }
                        };
                        ir::ExprKind::Pointer(ir::Pointer::Binding(binding_id))
                    }
                }
            },

            pr::ExprKind::Match(match_) => {
                let subject = self.lower_expr(&match_.subject)?;
                let subject_id = self.generator_var_binding.gen() as u32;
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
                        kind: ir::ExprKind::Literal(ir::Literal::Bool(true)),
                        ty: ir::Ty::new(ir::TyPrimitive::bool),
                    });

                    // collect pattern scope
                    let mut entries = Vec::new();
                    self.collect_pattern_scope(&branch.pattern, subject_ref.clone(), &mut entries);
                    self.scopes.push(Scope {
                        id: branch.value.scope_id.unwrap(),
                        kind: ScopeKind::Local { entries },
                    });

                    // compile value
                    let value = self.lower_expr(&branch.value)?;

                    self.scopes.pop();

                    switch_branches.push(ir::SwitchBranch { condition, value })
                }

                let ty = self.lower_ty(expr.ty.clone().unwrap());
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
            pr::ExprKind::FString(_) => todo!(),

            // consumed by type resolver
            pr::ExprKind::TypeAnnotation(_) => unreachable!(),

            // caught in lower_var_decl
            pr::ExprKind::Internal => unreachable!(),

            // desugared away
            pr::ExprKind::Pipeline(_) => unreachable!(),
            pr::ExprKind::Binary(_) => unreachable!(),
            pr::ExprKind::Unary(_) => unreachable!(),
            pr::ExprKind::Range(_) => unreachable!(),
        };
        Ok(ir::Expr {
            kind,
            ty: self.lower_ty(expr.ty.clone().unwrap()),
        })
    }

    fn lower_pattern_to_condition(
        &mut self,
        subject: &ir::Expr,
        pattern: &pr::Pattern,
    ) -> Result<Option<ir::Expr>> {
        match &pattern.kind {
            // match a enum variant
            pr::PatternKind::Enum(_, inner) => {
                let tag = pattern.variant_tag.unwrap();

                let mut expr = ir::Expr {
                    kind: ir::ExprKind::EnumEq(Box::new(ir::EnumEq {
                        expr: subject.clone(),
                        tag: pattern.variant_tag.unwrap() as u64,
                    })),
                    ty: ir::Ty::new(ir::TyPrimitive::bool),
                };

                if let Some(inner) = inner {
                    let subject_ty = self.get_ty_mat(subject.ty.clone());

                    let subject_variants = subject_ty.kind.into_enum().unwrap();
                    let inner_ty = subject_variants.into_iter().nth(tag).unwrap().ty;
                    let inner_ref = ir::Expr {
                        kind: ir::ExprKind::EnumUnwrap(Box::new(ir::EnumUnwrap {
                            expr: subject.clone(),
                            tag: tag as u64,
                        })),
                        ty: inner_ty,
                    };

                    let inner_cond = self.lower_pattern_to_condition(&inner_ref, inner)?;

                    if let Some(inner_cond) = inner_cond {
                        // new_bin_op(expr, "std::and", inner_cond)
                        expr = ir::Expr {
                            kind: ir::ExprKind::Call(Box::new(ir::Call {
                                function: ir::Expr {
                                    kind: ir::ExprKind::Pointer(ir::Pointer::External(
                                        ir::ExternalPtr {
                                            id: "std::and".to_string(),
                                        },
                                    )),
                                    ty: ir::Ty::new(ir::TyFunction {
                                        params: vec![
                                            ir::Ty::new(ir::TyPrimitive::bool),
                                            ir::Ty::new(ir::TyPrimitive::bool),
                                        ],
                                        body: ir::Ty::new(ir::TyPrimitive::bool),
                                    }),
                                },
                                args: vec![expr, inner_cond],
                            })),
                            ty: ir::Ty::new(ir::TyPrimitive::bool),
                        }
                    }
                }
                Ok(Some(expr))
            }

            // bind always matches
            pr::PatternKind::Bind(_) => Ok(None),
        }
    }

    fn lower_exprs(&mut self, exprs: &[pr::Expr]) -> Result<Vec<ir::Expr>> {
        exprs.iter().map(|e| self.lower_expr(e)).collect()
    }

    fn lower_var_bindings(&mut self, main: ir::Expr) -> ir::Expr {
        if self.var_bindings.is_empty() {
            return main;
        }

        let main_ty = main.ty.clone();
        let main_func_ty = main.ty.kind.as_function().unwrap().clone();

        // construct a new main function
        let f_id = self.generator_function_scope.gen() as u32;
        let mut main = ir::Expr {
            kind: ir::ExprKind::Call(Box::new(ir::Call {
                function: main,
                args: main_func_ty
                    .params
                    .iter()
                    .enumerate()
                    .map(|(p_pos, p)| ir::Expr {
                        kind: ir::ExprKind::Pointer(ir::Pointer::Parameter(ir::ParameterPtr {
                            function_id: f_id,
                            param_position: p_pos as u8,
                        })),
                        ty: p.clone(),
                    })
                    .collect(),
            })),
            ty: main_func_ty.body.clone(),
        };

        // fold each of the bindings
        let mut i = 0;
        loop {
            let Some((reference, id)) = self.var_bindings.get_index(i) else {
                break;
            };
            i += 1;
            let reference = reference.clone();
            let id = *id;

            let expr = self.lower_expr_decl(&reference.0, reference.1).unwrap();
            main = ir::Expr {
                ty: main.ty.clone(),
                kind: ir::ExprKind::Binding(Box::new(ir::Binding { id, expr, main })),
            }
        }

        // place bindings into function body
        ir::Expr {
            kind: ir::ExprKind::Function(Box::new(ir::Function {
                id: f_id,
                body: main,
            })),
            ty: main_ty,
        }
    }

    #[tracing::instrument(name = "lt", skip_all)]
    fn lower_ty(&mut self, ty: pr::Ty) -> ir::Ty {
        log::trace!("lower ty: {}", crate::printer::print_ty(&ty));

        if let Some(target) = ty.target {
            if let pr::Ref::FullyQualified { to_decl, .. } = target {
                self.type_defs_queue.push_back(to_decl.clone());

                log::debug!("lower ty ident: {to_decl}");

                return ir::Ty {
                    kind: ir::TyKind::Ident(ir::Path(to_decl.into_iter().collect_vec())),
                    layout: None,
                    name: ty.name,
                    variants_recursive: vec![],
                };
            } else {
                panic!("{:?} {target:?}", ty.kind)
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
            pr::TyKind::Tuple(fields) => ir::TyKind::Tuple(
                fields
                    .into_iter()
                    .map(|f| {
                        let name = f.name.clone();
                        let ty = self.lower_ty(f.ty);
                        ir::TyTupleField { name, ty }
                    })
                    .collect(),
            ),
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
                    .map(|p| self.lower_ty(p.unwrap()))
                    .collect(),
                body: self.lower_ty(*func.body.clone().unwrap()),
            })),
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

    /// Lowers a type definition
    fn lower_ty_def(&mut self, path: pr::Path) {
        if self.type_defs.contains_key(&path) {
            return;
        }

        let decl = self.root_module.module.get(&path).unwrap();
        let ty = decl.into_ty().unwrap().clone();

        let ty = self.lower_ty(ty);

        self.type_defs.insert(path, ty);
    }

    fn lower_ty_defs_queue(&mut self) {
        while let Some(path) = self.type_defs_queue.pop_front() {
            self.lower_ty_def(path)
        }
    }

    /// For a given pattern, this function collects all scope entries (variable bindings)
    /// and returns ir nodes that can be used as a reference to this entry.
    fn collect_pattern_scope(
        &mut self,
        pattern: &pr::Pattern,
        subject_ref: ir::Expr,
        entries: &mut Vec<ir::Expr>,
    ) {
        match &pattern.kind {
            // bind always matches
            pr::PatternKind::Bind(_) => {
                entries.push(subject_ref);
            }

            pr::PatternKind::Enum(_, inner) => {
                let tag = pattern.variant_tag.unwrap();

                if let Some(inner) = inner {
                    let subject_ty = self.get_ty_mat(subject_ref.ty.clone());

                    let subject_variants = subject_ty.kind.into_enum().unwrap();
                    let inner_ty = subject_variants.into_iter().nth(tag).unwrap().ty;
                    let inner_ref = ir::Expr {
                        kind: ir::ExprKind::EnumUnwrap(Box::new(ir::EnumUnwrap {
                            expr: subject_ref,
                            tag: tag as u64,
                        })),
                        ty: inner_ty,
                    };

                    self.collect_pattern_scope(inner, inner_ref, entries)
                }
            }
        }
    }

    fn wrap_into_func(&mut self, body: ir::Expr) -> ir::Expr {
        ir::Expr {
            ty: ir::Ty::new(ir::TyFunction {
                body: body.ty.clone(),
                params: vec![],
            }),
            kind: ir::ExprKind::Function(Box::new(ir::Function {
                id: self.generator_function_scope.gen() as u32,
                body,
            })),
        }
    }
}

pub fn lower_type_defs(root_module: &decl::RootModule) -> ir::Module {
    let mut lowerer = Lowerer::new(root_module);

    let mut module = ir::Module { decls: Vec::new() };

    for (name, decl) in root_module.module.iter_decls_re() {
        if name.starts_with_part("std") {
            continue;
        }

        match &decl.kind {
            decl::DeclKind::Module(_) => {
                unreachable!("iter_decls_re does not return modules");
            }

            decl::DeclKind::Expr(expr) => {
                let ty = lowerer.lower_ty(expr.ty.clone().unwrap());

                module.insert(name.full_path(), ir::Decl::Var(ty));
            }

            decl::DeclKind::Ty(_) => {
                lowerer.type_defs_queue.push_back(name);
            }

            decl::DeclKind::Import(_) => {}

            decl::DeclKind::Unresolved(_) => panic!(),
        }
    }

    lowerer.lower_ty_defs_queue();
    let types = order_ty_defs(lowerer.type_defs, root_module);
    for ty in types {
        module.insert(&ty.name.0, ir::Decl::Type(ty.ty));
    }
    module
}

fn order_ty_defs(
    mut by_name: HashMap<pr::Path, ir::Ty>,
    root_module: &decl::RootModule,
) -> Vec<ir::TyDef> {
    let mut r = Vec::with_capacity(by_name.len());
    for group in &root_module.ordering {
        for p in group {
            if let Some(ty) = by_name.remove(p) {
                r.push(ir::TyDef {
                    name: ir::Path(p.clone().into_iter().collect()),
                    ty,
                });
            }
        }
    }
    r
}
