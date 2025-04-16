use std::collections::{HashMap, HashSet, VecDeque};

use indexmap::IndexMap;
use itertools::Itertools;
use lutra_bin::ir;

use crate::utils::{self, IdGenerator};
use crate::Result;
use crate::{decl, pr};

pub fn lower_expr(root_module: &decl::RootModule, main: &pr::Expr) -> ir::Program {
    let mut lowerer = Lowerer::new(root_module);

    let main = lowerer.lower_expr(main).unwrap();
    let main = lowerer.lower_var_bindings(main);

    let types = lowerer.lower_ty_defs();
    let types = order_ty_defs(types, root_module);

    ir::Program { main, types }
}

pub fn lower_var(root_module: &decl::RootModule, path: &pr::Path) -> ir::Program {
    let mut lowerer = Lowerer::new(root_module);

    let main = lowerer.lower_expr_decl(path, vec![]).unwrap();
    let main = lowerer.lower_var_bindings(main);

    let types = lowerer.lower_ty_defs();
    let types = order_ty_defs(types, root_module);

    ir::Program { main, types }
}

struct Lowerer<'a> {
    root_module: &'a decl::RootModule,

    function_scopes: Vec<FuncScope>,
    var_bindings: IndexMap<(pr::Path, Vec<pr::Ty>), u32>,
    type_defs_needed: VecDeque<pr::Path>,

    generator_function_scope: IdGenerator<usize>,
    generator_var_binding: IdGenerator<usize>,
}

struct FuncScope {
    scope_id: usize,
    start_of_params: usize,
    func_id: u32,
}

impl<'a> Lowerer<'a> {
    fn new(root_module: &'a decl::RootModule) -> Self {
        Self {
            root_module,

            function_scopes: vec![],
            var_bindings: Default::default(),
            type_defs_needed: Default::default(),

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
                let func_id = self.generator_function_scope.gen() as u32;
                let scope = FuncScope {
                    scope_id: expr.scope_id.unwrap(),
                    func_id,
                    start_of_params: func.ty_params.len(),
                };

                self.function_scopes.push(scope);
                let body = self.lower_expr(&func.body)?;
                self.function_scopes.pop();

                let func = ir::Function { id: func_id, body };

                ir::ExprKind::Function(Box::new(func))
            }

            pr::ExprKind::Ident(_) => match expr.target.as_ref().unwrap() {
                pr::Ref::Local { scope, offset } => {
                    let scope = (self.function_scopes.iter())
                        .find(|s| s.scope_id == *scope)
                        .unwrap();
                    let param_position = (*offset - scope.start_of_params) as u8;

                    ir::ExprKind::Pointer(ir::Pointer::Parameter(ir::ParameterPtr {
                        function_id: scope.func_id,
                        param_position,
                    }))
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
                    switch_branches.push(ir::SwitchBranch {
                        condition: self.lower_pattern_match(&subject_ref, &branch.pattern)?,
                        value: self.lower_expr(&branch.value)?,
                    })
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

    fn lower_pattern_match(
        &mut self,
        subject: &ir::Expr,
        pattern: &pr::Pattern,
    ) -> Result<ir::Expr> {
        let kind = match &pattern.kind {
            // consumed by resolver
            pr::PatternKind::Ident(_) => unreachable!(),

            pr::PatternKind::EnumEq(tag) => ir::ExprKind::EnumEq(Box::new(ir::EnumEq {
                expr: subject.clone(),
                tag: *tag as u64,
            })),
        };
        Ok(ir::Expr {
            kind,
            ty: ir::Ty::new(ir::TyPrimitive::bool),
        })
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
                self.type_defs_needed.push_back(to_decl.clone());

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

    fn lower_ty_defs(&mut self) -> HashMap<pr::Path, ir::Ty> {
        let mut defs = HashMap::with_capacity(self.type_defs_needed.len());
        let mut names_done = HashSet::new();

        while let Some(path) = self.type_defs_needed.pop_front() {
            if names_done.contains(&path) {
                continue;
            }

            let decl = self.root_module.module.get(&path).unwrap();
            let ty = decl.into_ty().unwrap().clone();

            let ty = self.lower_ty(ty);

            defs.insert(path.clone(), ty);
            names_done.insert(path);
        }

        defs
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
                lowerer.type_defs_needed.push_back(name);
            }

            decl::DeclKind::Import(_) => {}

            decl::DeclKind::Unresolved(_) => panic!(),
        }
    }

    let types = lowerer.lower_ty_defs();
    let types = order_ty_defs(types, root_module);
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
