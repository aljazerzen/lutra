use crate::utils::IdGenerator;

use super::cr;
use indexmap::IndexMap;
use lutra_bin::ir;

struct Context {
    bindings: IndexMap<u32, String>,
    functions: IndexMap<u32, String>,
    cte_alias_gen: IdGenerator<usize>,
}

pub fn compile(program: &ir::Program) -> cr::RelExpr {
    let mut ctx = Context {
        bindings: Default::default(),
        functions: Default::default(),
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
            ir::ExprKind::Literal(lit) => cr::RelExprKind::Constructed(vec![vec![cr::Expr {
                kind: cr::ExprKind::Literal(lit.clone()),
                ty: expr.ty.clone(),
            }]]),
            ir::ExprKind::Tuple(fields) => {
                let row = fields.iter().flat_map(|x| self.compile_cols(x)).collect();
                cr::RelExprKind::Constructed(vec![row])
            }
            ir::ExprKind::Array(items) => {
                let rows = items
                    .iter()
                    .enumerate()
                    .map(|(index, x)| {
                        let mut cols = vec![cr::Expr {
                            kind: cr::ExprKind::Literal(ir::Literal::Int(index as i64)),
                            ty: ir::Ty::new(ir::PrimitiveSet::int64),
                        }];
                        cols.extend(self.compile_cols(x));
                        cols
                    })
                    .collect();
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
            ir::ExprKind::Pointer(ir::Pointer::Parameter(ptr)) => {
                let _ = self.functions.get(&ptr.function_id).unwrap();
                assert_eq!(ptr.param_position, 0);

                cr::RelExprKind::SelectRelVar
            }
            ir::ExprKind::Pointer(ir::Pointer::External(_)) => todo!(),

            ir::ExprKind::Function(_) => todo!(),

            ir::ExprKind::TupleLookup(lookup) => {
                let base = self.compile_rel(&lookup.base);
                cr::RelExprKind::ProjectRetain(Box::new(base), vec![lookup.position as usize])
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
                let start = self.compile_col(&call.args[1]);
                let end = self.compile_col(&call.args[2]);

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
                let index = self.compile_col(&call.args[1]);

                let offset = cr::RelExpr {
                    ty: array.ty.clone(),
                    kind: cr::RelExprKind::Offset(Box::new(array), index.clone()),
                };

                let limit = cr::RelExpr {
                    ty: offset.ty.clone(),
                    kind: cr::RelExprKind::Limit(Box::new(offset), new_int(1)),
                };

                cr::RelExprKind::ProjectDrop(Box::new(limit), vec![0])
            }
            "std::map" => {
                let array = self.compile_rel(&call.args[0]);
                let func = &call.args[1];
                let func = func.kind.as_function().unwrap();

                let mut row = vec![new_column_of(
                    array.ty.clone(),
                    0,
                    ir::Ty::new(ir::PrimitiveSet::int64),
                )];

                self.functions.insert(func.id, "does_not_matter".into());
                row.extend(self.compile_cols(&func.body));
                self.functions.swap_remove(&func.id);

                cr::RelExprKind::ProjectReplace(Box::new(array), row)
            }
            "std::filter" => {
                let array = self.compile_rel(&call.args[0]);
                let func = &call.args[1];
                let func = func.kind.as_function().unwrap();

                self.functions.insert(func.id, "does_not_matter".into());
                let cond = self.compile_col(&func.body);
                self.functions.swap_remove(&func.id);

                cr::RelExprKind::Where(Box::new(array), cond)
            }
            "std::sort" => {
                let array = self.compile_rel(&call.args[0]);

                let func = &call.args[1];
                let func = func.kind.as_function().unwrap();

                self.functions.insert(func.id, "does_not_matter".into());
                let key = self.compile_col(&func.body);
                self.functions.swap_remove(&func.id);

                cr::RelExprKind::OrderBy(Box::new(array), key)
            }

            // aggregation functions
            "std::min" | "std::max" | "std::sum" | "std::average" | "std::count" | "std::any"
            | "std::all" | "std::contains" => {
                let array = self.compile_rel(&call.args[0]);
                let item_ty = call.args[0].ty.kind.as_array().unwrap();

                let item = cr::Expr {
                    kind: cr::ExprKind::Subquery(Box::new(cr::RelExpr {
                        kind: cr::RelExprKind::SelectRelVar,
                        ty: *item_ty.clone(),
                    })),
                    ty: *item_ty.clone(),
                };

                let mut args = vec![item];
                args.extend(call.args[1..].iter().map(|a| self.compile_col(a)));

                cr::RelExprKind::Aggregate(
                    Box::new(array),
                    vec![cr::Expr {
                        kind: cr::ExprKind::FuncCall(ptr.id.clone(), args),
                        ty: expr.ty.clone(),
                    }],
                )
            }

            // window functions
            "std::row_number" | "std::lead" | "std::lag" => {
                let array = self.compile_rel(&call.args[0]);

                let item_ty = expr.ty.kind.as_array().unwrap();

                let item = cr::Expr {
                    kind: cr::ExprKind::Subquery(Box::new(cr::RelExpr {
                        kind: cr::RelExprKind::SelectRelVar,
                        ty: *item_ty.clone(),
                    })),
                    ty: *item_ty.clone(),
                };

                let mut args = vec![item];
                args.extend(call.args[1..].iter().map(|a| self.compile_col(a)));

                cr::RelExprKind::ProjectReplace(
                    Box::new(array),
                    vec![
                        new_column_of(
                            call.args[0].ty.clone(),
                            0,
                            ir::Ty::new(ir::PrimitiveSet::int64),
                        ),
                        cr::Expr {
                            kind: cr::ExprKind::FuncCall(ptr.id.clone(), args),
                            ty: expr.ty.clone(),
                        },
                    ],
                )
            }

            _ => {
                let expr = self.compile_expr_std(expr);
                cr::RelExprKind::Constructed(vec![vec![expr]])
            }
        }
    }

    /// Compiles an expression that can be placed into a list of columns.
    /// It must have type of tuple.
    fn compile_cols(&mut self, expr: &ir::Expr) -> Vec<cr::Expr> {
        // compile as if this was a rel, which can sink complex operations
        // into the relational expression
        let rel = self.compile_rel(expr);

        match rel.kind {
            // it is just one value: unwrap
            cr::RelExprKind::Constructed(mut rows) if rows.len() == 1 => rows.remove(0),

            // it is actually complex: subquery
            _ => vec![cr::Expr {
                kind: cr::ExprKind::Subquery(Box::new(rel)),
                ty: expr.ty.clone(),
            }],
        }
    }

    fn compile_col(&mut self, expr: &ir::Expr) -> cr::Expr {
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

        let args = call.args.iter().map(|x| self.compile_col(x)).collect();
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

fn new_column_of(ty: ir::Ty, col_index: usize, col_ty: ir::Ty) -> cr::Expr {
    let kind = cr::ExprKind::Subquery(Box::new(cr::RelExpr {
        kind: cr::RelExprKind::ProjectRetain(
            Box::new(cr::RelExpr {
                kind: cr::RelExprKind::SelectRelVar,
                ty,
            }),
            vec![col_index],
        ),
        ty: col_ty.clone(),
    }));
    cr::Expr { kind, ty: col_ty }
}
