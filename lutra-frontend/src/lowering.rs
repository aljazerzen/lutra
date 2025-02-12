use std::iter::zip;

use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use lutra_bin::ir::{self, ExecutionHost};

use crate::diagnostic::Diagnostic;
use crate::utils::IdGenerator;
use crate::Result;
use crate::{decl, pr, semantic};

pub fn lower(root_module: &decl::RootModule, path: &pr::Path) -> ir::Program {
    let mut lowerer = Lowerer::new(root_module);

    let main = lowerer.lower_expr_decl(path).unwrap();
    let main = lowerer.lower_var_bindings(main);

    ir::Program { main }
}

struct Lowerer<'a> {
    root_module: &'a decl::RootModule,
    current_host: ir::ExecutionHost,

    function_scopes: Vec<u32>,
    var_bindings: IndexMap<pr::Path, u32>,
    var_bindings_host: IndexMap<u32, ir::ExecutionHost>,

    generator_function_scope: IdGenerator<usize>,
    generator_var_binding: IdGenerator<usize>,
}

impl<'a> Lowerer<'a> {
    fn new(root_module: &'a decl::RootModule) -> Self {
        Self {
            root_module,
            current_host: ir::ExecutionHost::Any,

            function_scopes: vec![],
            var_bindings: Default::default(),
            var_bindings_host: Default::default(),

            generator_function_scope: Default::default(),
            generator_var_binding: Default::default(),
        }
    }

    fn lower_external_expr_decl(&mut self, path: &pr::Path) -> Result<Option<ir::ExprKind>> {
        let decl = self
            .root_module
            .module
            .get(path)
            .unwrap_or_else(|| panic!("{path} does not exist"));
        let decl::DeclKind::Expr(expr) = &decl.kind else {
            panic!();
        };

        if !matches!(expr.kind, pr::ExprKind::Internal) {
            return Ok(None);
        }

        let host = determine_decl_execution_host(self.root_module, path)?;

        let external_symbol_id = path.iter().join("::");
        Ok(Some(ir::ExprKind::Pointer(ir::Pointer::External(
            ir::ExternalPtr {
                id: external_symbol_id,
                host,
            },
        ))))
    }

    fn lower_expr_decl(&mut self, path: &pr::Path) -> Result<ir::Expr> {
        let decl = self
            .root_module
            .module
            .get(path)
            .unwrap_or_else(|| panic!("{path} does not exist"));
        let decl::DeclKind::Expr(expr) = &decl.kind else {
            panic!();
        };

        // should have been lowered earlier
        assert!(!matches!(expr.kind, pr::ExprKind::Internal));

        // determine the execution host requirements of this decl
        let host = determine_decl_execution_host(self.root_module, path)?;

        let prev_host = self.current_host.clone();
        self.current_host = host;
        let res = self.lower_expr(expr.as_ref())?;
        self.current_host = prev_host;

        Ok(res)
    }

    fn lower_expr(&mut self, expr: &pr::Expr) -> Result<ir::Expr> {
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

            pr::ExprKind::Tuple(fields) => ir::ExprKind::Tuple(self.lower_exprs(fields)?),
            pr::ExprKind::Array(items) => ir::ExprKind::Array(self.lower_exprs(items)?),
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
                function: self.lower_expr(&call.name)?,
                args: self.lower_exprs(&call.args)?,
            })),
            pr::ExprKind::Func(func) => {
                let function_id = self.generator_function_scope.gen() as u32;

                self.function_scopes.push(function_id);
                let body = self.lower_expr(&func.body)?;
                self.function_scopes.pop();

                let mut func = ir::Function {
                    id: function_id,
                    host: self.current_host.clone(),
                    body,
                };
                func.host = determine_func_execution_host(&func, &self.var_bindings_host);

                ir::ExprKind::Function(Box::new(func))
            }

            pr::ExprKind::Ident(path) => {
                if path.starts_with_part("func") {
                    let mut path = path.iter().peekable();
                    path.next(); // func

                    // walk the scope stack for each `up` in ident
                    let mut scope = self.function_scopes.iter().rev();
                    while path.peek().map_or(false, |x| *x == "up") {
                        path.next();
                        scope.next();
                    }

                    let param_position = path.next().unwrap().parse::<u8>().unwrap();
                    let function_id = *scope.next().unwrap();
                    ir::ExprKind::Pointer(ir::Pointer::Parameter(ir::ParameterPtr {
                        function_id,
                        param_position,
                    }))
                } else if let Some(ptr) = self.lower_external_expr_decl(path)? {
                    ptr
                } else {
                    let entry = self.var_bindings.entry(path.clone());
                    let binding_id = match entry {
                        indexmap::map::Entry::Occupied(e) => *e.get(),
                        indexmap::map::Entry::Vacant(e) => {
                            let id = self.generator_var_binding.gen() as u32;
                            e.insert(id);

                            let host = determine_decl_execution_host(self.root_module, path)?;
                            self.var_bindings_host.insert(id, host);

                            id
                        }
                    };
                    ir::ExprKind::Pointer(ir::Pointer::Binding(binding_id))
                }
            }

            pr::ExprKind::Case(_) => todo!(),
            pr::ExprKind::FString(_) => todo!(),

            // caught in lower_var_decl
            pr::ExprKind::Internal => todo!(),

            // desugared away
            pr::ExprKind::Pipeline(_) => todo!(),
            pr::ExprKind::Binary(_) => todo!(),
            pr::ExprKind::Unary(_) => todo!(),
            pr::ExprKind::Range(_) => todo!(),
        };
        Ok(ir::Expr {
            kind,
            ty: ir::Ty::from(expr.ty.clone().unwrap()),
        })
    }

    fn lower_exprs(&mut self, exprs: &[pr::Expr]) -> Result<Vec<ir::Expr>> {
        exprs.iter().map(|e| self.lower_expr(e)).collect()
    }

    fn lower_var_bindings(&mut self, main: ir::Expr) -> ir::Expr {
        let mut main = main;
        let mut i = 0;
        loop {
            let Some((path, id)) = self.var_bindings.get_index(i) else {
                break;
            };
            i += 1;
            let path = path.clone();
            let id = *id;

            let expr = self.lower_expr_decl(&path).unwrap();
            main = ir::Expr {
                ty: main.ty.clone(),
                kind: ir::ExprKind::Binding(Box::new(ir::Binding { id, expr, main })),
            }
        }
        main
    }
}

fn determine_decl_execution_host(
    root_module: &decl::RootModule,
    path: &pr::Path,
) -> Result<ir::ExecutionHost, Diagnostic> {
    let module_path = root_module.module.get_module_path(path.path()).unwrap();
    let remote = pr::Path::from_name("remote");

    for (module, step) in zip(module_path.into_iter().rev(), path.iter().rev()) {
        let decl = module.names.get(step).unwrap();
        if let Some(args) = semantic::decl_get_annotation(decl, &remote) {
            let arg = args.iter().exactly_one().map_err(|_| {
                Diagnostic::new_custom("@remote requires exactly one argument").with_span(decl.span)
            })?;

            let arg = arg
                .kind
                .as_literal()
                .and_then(|l| l.as_text())
                .ok_or_else(|| {
                    Diagnostic::new_custom("@remote requires a text literal").with_span(arg.span)
                })?;

            return Ok(ir::ExecutionHost::Remote(arg.clone()));
        }
    }
    Ok(ir::ExecutionHost::Any)
}

fn determine_func_execution_host(
    func: &ir::Function,
    bindings: &IndexMap<u32, ir::ExecutionHost>,
) -> ir::ExecutionHost {
    if func.host != ir::ExecutionHost::Any {
        return func.host.clone();
    }

    let mut finder = HostFinder {
        hosts: Default::default(),
        bindings,
    };
    finder.fold_expr(&func.body);

    finder.hosts.retain(|h| h != &ir::ExecutionHost::Any);

    let all_equal = finder.hosts.into_iter().all_equal_value();
    match all_equal {
        Ok(host) => host,
        Err(None) => ir::ExecutionHost::Any,
        Err(Some(_)) => ir::ExecutionHost::Local,
    }
}

struct HostFinder<'a> {
    hosts: IndexSet<ExecutionHost>,
    bindings: &'a IndexMap<u32, ir::ExecutionHost>,
}

impl HostFinder<'_> {
    fn fold_expr(&mut self, expr: &ir::Expr) {
        match &expr.kind {
            ir::ExprKind::Pointer(ir::Pointer::External(external_ptr)) => {
                self.hosts.insert(external_ptr.host.clone());
            }
            ir::ExprKind::Pointer(ir::Pointer::Parameter(_)) => {}
            ir::ExprKind::Pointer(ir::Pointer::Binding(id)) => {
                let host = self.bindings.get(id).unwrap();
                self.hosts.insert(host.clone());
            }
            ir::ExprKind::Literal(_) => {}
            ir::ExprKind::Call(call) => {
                self.fold_expr(&call.function);
                self.fold_exprs(&call.args);
            }
            ir::ExprKind::Function(func) => {
                self.hosts.insert(func.host.clone());
            }
            ir::ExprKind::Tuple(fields) => self.fold_exprs(fields),
            ir::ExprKind::Array(items) => self.fold_exprs(items),
            ir::ExprKind::TupleLookup(lookup) => self.fold_expr(&lookup.base),
            ir::ExprKind::Binding(binding) => {
                self.fold_expr(&binding.expr);
                self.fold_expr(&binding.main);
            }
            ir::ExprKind::RemoteCall(call) => {
                self.hosts
                    .insert(ExecutionHost::Remote(call.remote_id.clone()));
            }
        }
    }

    fn fold_exprs(&mut self, exprs: &[ir::Expr]) {
        for expr in exprs {
            self.fold_expr(expr);
        }
    }
}
