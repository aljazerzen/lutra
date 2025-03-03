use crate::utils::IdGenerator;

use super::cr;
use indexmap::IndexMap;
use lutra_bin::ir;

struct Context {
    bindings: IndexMap<u32, String>,
    cte_alias_gen: IdGenerator<usize>,
}

pub fn compile(program: &ir::Program) -> cr::RelExpr {
    let mut ctx = Context {
        bindings: Default::default(),
        cte_alias_gen: Default::default(),
    };

    // find the top-level function
    let ir::ExprKind::Function(func) = &program.main.kind else {
        todo!("top-level: {:?}", program.main)
    };

    ctx.compile_rel(&func.body)
}

impl Context {
    fn compile_rel(&mut self, expr: &ir::Expr) -> cr::RelExpr {
        let kind = match &expr.kind {
            ir::ExprKind::Literal(_) => {
                cr::RelExprKind::Constructed(vec![vec![self.compile_expr(expr)]])
            }
            ir::ExprKind::Tuple(fields) => {
                let row = fields.iter().map(|x| self.compile_expr(x)).collect();
                cr::RelExprKind::Constructed(vec![row])
            }
            ir::ExprKind::Array(items) => {
                let rows = items.iter().map(|x| self.compile_tuple(x)).collect();
                cr::RelExprKind::Constructed(rows)
            }

            ir::ExprKind::Call(call) => match &call.function.kind {
                ir::ExprKind::Pointer(ir::Pointer::External(ptr))
                    if ptr.id.starts_with("std::") =>
                {
                    self.compile_rel_std(expr)
                }
                ir::ExprKind::Pointer(ir::Pointer::External(ptr)) => {
                    let is_table = call.function.ty.kind.as_function().map_or(false, |f| {
                        if !f.params.is_empty() {
                            return false;
                        }

                        f.body.kind.as_array().map_or(false, |a| a.kind.is_tuple())
                    });

                    if !is_table {
                        todo!("only supported external refs are table functions (no params, return array of tuples)");
                    }

                    cr::RelExprKind::FromTable(ptr.id.clone())
                }
                ir::ExprKind::Pointer(_) => todo!(),
                ir::ExprKind::Call(_) => todo!(),
                ir::ExprKind::Function(func) if call.args.is_empty() => {
                    // shortcut: `(call (func -> x))` is equivalent to just `x`
                    return self.compile_rel(&func.body);
                }
                ir::ExprKind::Function(_) => todo!(),

                ir::ExprKind::TupleLookup(_) => todo!(),
                ir::ExprKind::Binding(_) => todo!(),

                ir::ExprKind::Literal(_) | ir::ExprKind::Tuple(_) | ir::ExprKind::Array(_) => {
                    unreachable!()
                }
            },

            ir::ExprKind::Pointer(ir::Pointer::Binding(ptr)) => {
                let name = self.bindings.get(ptr).unwrap();
                cr::RelExprKind::FromBinding(name.clone())
            }
            ir::ExprKind::Pointer(_) => todo!(),

            ir::ExprKind::Function(_) => todo!(),

            ir::ExprKind::TupleLookup(lookup) => {
                let base = self.compile_rel(&lookup.base);
                cr::RelExprKind::ProjectColumn(Box::new(base), lookup.position)
            }
            ir::ExprKind::Binding(binding) => {
                if let ir::TyKind::Function(_) = &binding.expr.ty.kind {
                    todo!()
                } else {
                    let alias = format!("t{}", self.cte_alias_gen.gen());

                    // compile expr
                    let expr = self.compile_rel(&binding.expr);
                    self.bindings.insert(binding.id, alias);

                    // compile main with expr in ctx
                    let main = self.compile_rel(&binding.main);

                    let alias = self.bindings.swap_remove(&binding.id).unwrap();
                    cr::RelExprKind::With(alias, Box::new(expr), Box::new(main))
                }
            }
        };
        cr::RelExpr {
            kind,
            ty: expr.ty.clone(),
        }
    }

    fn compile_rel_std(&mut self, expr: &ir::Expr) -> cr::RelExprKind {
        let ir::ExprKind::Call(call) = &expr.kind else {
            unreachable!()
        };
        let ir::ExprKind::Pointer(ir::Pointer::External(ptr)) = &call.function.kind else {
            unreachable!()
        };

        match ptr.id.as_str() {
            "std::slice" => {
                let array = self.compile_rel(&call.args[0]);
                let start = self.compile_expr(&call.args[1]);
                let end = self.compile_expr(&call.args[2]);

                let offset = cr::RelExpr {
                    kind: cr::RelExprKind::Offset(Box::new(array), start.clone()),
                    ty: expr.ty.clone(),
                };

                cr::RelExprKind::Limit(
                    Box::new(offset),
                    new_bin_op(end, "std::sub", start, ir::PrimitiveSet::int64),
                )
            }
            "std::index" => {
                let array = self.compile_rel(&call.args[0]);
                let index = self.compile_expr(&call.args[1]);

                let offset = cr::RelExpr {
                    kind: cr::RelExprKind::Offset(Box::new(array), index.clone()),
                    ty: expr.ty.clone(),
                };

                let limit = cr::RelExpr {
                    kind: cr::RelExprKind::Limit(Box::new(offset), new_int(1)),
                    ty: expr.ty.clone(),
                };

                cr::RelExprKind::ProjectUnIndex(Box::new(limit))
            }
            _ => {
                let expr = self.compile_expr_std(expr);
                cr::RelExprKind::Constructed(vec![vec![expr]])
            }
        }
    }

    /// Compiles an expression that can be placed into a list of columns.
    /// It must have type of tuple.
    fn compile_tuple(&mut self, expr: &ir::Expr) -> Vec<cr::Expr> {
        match &expr.kind {
            ir::ExprKind::Literal(_) => vec![self.compile_expr(expr)],

            ir::ExprKind::Tuple(fields) => fields.iter().map(|x| self.compile_expr(x)).collect(),

            ir::ExprKind::Pointer(_) => todo!(),
            ir::ExprKind::Call(_) => todo!(),
            ir::ExprKind::Function(_) => todo!(),
            ir::ExprKind::Array(_) => todo!(),
            ir::ExprKind::TupleLookup(_) => todo!(),
            ir::ExprKind::Binding(_) => todo!(),
        }
    }

    fn compile_expr(&mut self, expr: &ir::Expr) -> cr::Expr {
        // for simple things, return immediately
        if let ir::ExprKind::Literal(lit) = &expr.kind {
            return cr::Expr {
                kind: cr::ExprKind::Literal(lit.clone()),
                ty: expr.ty.clone(),
            };
        }

        // compile as if this was a rel, which can sink complex operations
        // into the relational expression
        let rel = self.compile_rel(expr);

        match rel.kind {
            // it is just one value: unwrap
            cr::RelExprKind::Constructed(mut rows) if rows.len() == 1 && rows[0].len() == 1 => {
                rows.remove(0).remove(0)
            }

            // it is actually complex: subquery
            _ => cr::Expr {
                kind: cr::ExprKind::Subquery(Box::new(rel)),
                ty: expr.ty.clone(),
            },
        }
    }

    fn compile_expr_std(&mut self, expr: &ir::Expr) -> cr::Expr {
        let ir::ExprKind::Call(call) = &expr.kind else {
            unreachable!()
        };

        let ir::ExprKind::Pointer(ir::Pointer::External(ptr)) = &call.function.kind else {
            unreachable!()
        };

        let args = call.args.iter().map(|x| self.compile_expr(x)).collect();
        cr::Expr {
            kind: cr::ExprKind::FuncCall(ptr.id.clone(), args),
            ty: expr.ty.clone(),
        }
    }
}

fn new_bin_op(left: cr::Expr, op: &str, right: cr::Expr, ty: ir::PrimitiveSet) -> cr::Expr {
    let kind = cr::ExprKind::FuncCall(op.to_string(), vec![left, right]);
    cr::Expr {
        kind,
        ty: ir::Ty {
            kind: ir::TyKind::Primitive(ty),
            name: None,
            layout: None,
        },
    }
}

fn new_int(int: i64) -> cr::Expr {
    let kind = cr::ExprKind::Literal(ir::Literal::Int(int));
    cr::Expr {
        kind,
        ty: ir::Ty {
            kind: ir::TyKind::Primitive(ir::PrimitiveSet::int64),
            name: None,
            layout: None,
        },
    }
}
