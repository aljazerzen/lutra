use indexmap::IndexMap;
use itertools::Itertools;
use lutra_bin::ir::{self};

use crate::utils::IdGenerator;
use crate::Result;
use crate::{decl, pr};

pub fn lower(root_module: &decl::RootModule, path: &pr::Path) -> ir::Program {
    let mut lowerer = Lowerer::new(root_module);

    let main = lowerer.lower_expr_decl(path).unwrap();
    let main = lowerer.lower_var_bindings(main);

    ir::Program { main }
}

struct Lowerer<'a> {
    root_module: &'a decl::RootModule,

    function_scopes: Vec<u32>,
    var_bindings: IndexMap<pr::Path, u32>,

    generator_function_scope: IdGenerator<usize>,
    generator_var_binding: IdGenerator<usize>,
}

impl<'a> Lowerer<'a> {
    fn new(root_module: &'a decl::RootModule) -> Self {
        Self {
            root_module,

            function_scopes: vec![],
            var_bindings: Default::default(),

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

        let external_symbol_id = path.iter().join("::");
        Ok(Some(ir::ExprKind::Pointer(ir::Pointer::External(
            ir::ExternalPtr {
                id: external_symbol_id,
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

        if let pr::TyKind::Function(ty_func) = &expr.ty.as_ref().unwrap().kind {
            if !ty_func.ty_params.is_empty() {
                // todo!("instantiation of funcs with type params in lowering");
            }
        }

        let res = self.lower_expr(expr.as_ref())?;

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

            pr::ExprKind::Tuple(fields) => ir::ExprKind::Tuple(
                fields
                    .iter()
                    .map(|f| self.lower_expr(&f.expr))
                    .try_collect()?,
            ),
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
                function: self.lower_expr(&call.func)?,
                args: self.lower_exprs(&call.args)?,
            })),
            pr::ExprKind::Func(func) => {
                let function_id = self.generator_function_scope.gen() as u32;

                self.function_scopes.push(function_id);
                let body = self.lower_expr(&func.body)?;
                self.function_scopes.pop();

                let func = ir::Function {
                    id: function_id,
                    body,
                };

                ir::ExprKind::Function(Box::new(func))
            }

            pr::ExprKind::Ident(path) => {
                if path.starts_with_part("scope") {
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
