use indexmap::IndexMap;
use itertools::Itertools;
use lutra_bin::ir;

use crate::{decl, pr, utils::IdGenerator, Result};

pub fn lower(root_module: &decl::RootModule, path: &pr::Path) -> ir::Program {
    let mut lowerer = Lowerer::new(root_module);

    let main = lowerer.lower_expr_decl(path).unwrap();
    let main = lowerer.lower_var_bindings(main);

    ir::Program {
        externals: lowerer.externals,
        main,
    }
}

struct Lowerer<'a> {
    root_module: &'a decl::RootModule,

    externals: Vec<ir::ExternalSymbol>,
    function_scopes: Vec<u32>,
    var_bindings: IndexMap<pr::Path, u32>,

    generator_function_scope: IdGenerator<usize>,
    #[allow(dead_code)]
    generator_var_binding: IdGenerator<usize>,
}

impl<'a> Lowerer<'a> {
    fn new(root_module: &'a decl::RootModule) -> Self {
        Self {
            root_module,

            externals: Default::default(),
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

        let sid = ir::Sid(self.externals.len() as u32);
        self.externals.push(ir::ExternalSymbol {
            id: path.iter().join("::"),
        });
        Ok(Some(ir::ExprKind::Pointer(sid)))
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

        self.lower_expr(expr.as_ref())
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
                let symbol_ns = self.generator_function_scope.gen() as u32;
                let symbol_ns = (symbol_ns << 8) | 0x80000000u32;

                self.function_scopes.push(symbol_ns);
                let body = self.lower_expr(&func.body)?;
                self.function_scopes.pop();

                ir::ExprKind::Function(Box::new(ir::Function {
                    symbol_ns: ir::Sid(symbol_ns),
                    body,
                }))
            }

            pr::ExprKind::Ident(path) => {
                if path.starts_with_part("func") {
                    let mut path = path.iter().peekable();
                    path.next();
                    let mut scope = self.function_scopes.iter().rev();
                    while path.peek().map_or(false, |x| *x == "up") {
                        path.next();
                        scope.next();
                    }
                    let param_index = path.next().unwrap().parse::<u32>().unwrap();
                    let scope = scope.next().unwrap();
                    ir::ExprKind::Pointer(ir::Sid(scope + param_index))
                } else if let Some(ptr) = self.lower_external_expr_decl(path)? {
                    ptr
                } else {
                    let entry = self.var_bindings.entry(path.clone());
                    let entry = entry.or_insert_with(|| {
                        let binding_id = self.generator_var_binding.gen() as u32;
                        binding_id | 0x40000000u32
                    });
                    ir::ExprKind::Pointer(ir::Sid(*entry))
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
            let Some((path, symbol)) = self.var_bindings.get_index(i) else {
                break;
            };
            i += 1;
            let path = path.clone();
            let symbol = *symbol;

            let expr = self.lower_expr_decl(&path).unwrap();
            main = ir::Expr {
                ty: main.ty.clone(),
                kind: ir::ExprKind::Binding(Box::new(ir::Binding {
                    symbol: ir::Sid(symbol),
                    expr,
                    main,
                })),
            }
        }
        main
    }
}
