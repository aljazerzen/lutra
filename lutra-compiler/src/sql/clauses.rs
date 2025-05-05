use std::{collections::HashMap, rc::Rc};

use crate::utils::IdGenerator;

use super::cr;
use lutra_bin::ir;

struct Context<'t> {
    bindings: HashMap<u32, String>,
    functions: HashMap<u32, FuncProvider>,

    types: HashMap<&'t ir::Path, &'t ir::Ty>,

    cte_alias_gen: IdGenerator<usize>,
    rvar_alias_gen: IdGenerator<usize>,
}

enum FuncProvider {
    RelExpr(cr::RelExprKind),
    Params,
}

pub fn compile(program: &ir::Program) -> (cr::RelExpr, HashMap<&ir::Path, &ir::Ty>) {
    let mut ctx = Context {
        bindings: Default::default(),
        functions: Default::default(),
        types: program
            .types
            .iter()
            .map(|def| (&def.name, &def.ty))
            .collect(),
        cte_alias_gen: Default::default(),
        rvar_alias_gen: Default::default(),
    };

    // find the top-level function
    let ir::ExprKind::Function(func) = &program.main.kind else {
        todo!("top-level: {:?}", program.main)
    };
    ctx.functions.insert(func.id, FuncProvider::Params);

    let body = ctx.compile_rel(&func.body);

    (body, ctx.types)
}

impl<'a> Context<'a> {
    fn get_ty_mat(&self, ty: &'a ir::Ty) -> &'a ir::Ty {
        match &ty.kind {
            ir::TyKind::Ident(path) => self.types.get(path).unwrap(),
            _ => ty,
        }
    }

    fn compile_rel(&mut self, expr: &ir::Expr) -> cr::RelExpr {
        let kind = match &expr.kind {
            ir::ExprKind::Literal(lit) => cr::RelExprKind::Constructed(vec![vec![cr::Expr {
                kind: cr::ExprKind::Literal(lit.clone()),
                ty: expr.ty.clone(),
            }]]),
            ir::ExprKind::Tuple(fields) => {
                let row = fields
                    .iter()
                    .flat_map(|x| self.compile_column_list(x))
                    .collect();
                cr::RelExprKind::Constructed(vec![row])
            }
            ir::ExprKind::Array(items) => {
                let rows = items
                    .iter()
                    .enumerate()
                    .map(|(index, x)| {
                        let mut cols = vec![cr::Expr {
                            kind: cr::ExprKind::Literal(ir::Literal::Int(index as i64)),
                            ty: ir::Ty::new(ir::TyPrimitive::int64),
                        }];
                        cols.extend(self.compile_column_list(x));
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
                    let is_table = self.ptr_is_table(&call.function);

                    if !is_table {
                        tracing::debug!("expected a table getter: {:?}", call.function.ty);
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

                ir::ExprKind::Literal(_)
                | ir::ExprKind::Tuple(_)
                | ir::ExprKind::Array(_)
                | ir::ExprKind::EnumVariant(_)
                | ir::ExprKind::EnumEq(_)
                | ir::ExprKind::EnumUnwrap(_)
                | ir::ExprKind::Switch(_) => {
                    unreachable!()
                }
            },

            ir::ExprKind::Pointer(ir::Pointer::Binding(ptr)) => {
                let name = self.bindings.get(ptr).unwrap();
                cr::RelExprKind::FromBinding(name.clone())
            }
            ir::ExprKind::Pointer(ir::Pointer::Parameter(ptr)) => {
                let provider = self.functions.get(&ptr.function_id).unwrap();
                match provider {
                    FuncProvider::RelExpr(r_expr) => {
                        assert_eq!(ptr.param_position, 0);
                        r_expr.clone()
                    }
                    FuncProvider::Params => {
                        let expr = cr::Expr {
                            kind: cr::ExprKind::Param(ptr.param_position),
                            ty: expr.ty.clone(),
                        };
                        cr::RelExprKind::Constructed(vec![vec![expr]])
                    }
                }
            }
            ir::ExprKind::Pointer(ir::Pointer::External(_)) => todo!(),

            ir::ExprKind::Function(_) => todo!(),

            ir::ExprKind::EnumVariant(variant) => {
                let ty_variants = expr.ty.kind.as_enum().unwrap();

                let mut row = Vec::with_capacity(ty_variants.len() + 1);

                // tag
                row.push(cr::Expr {
                    kind: cr::ExprKind::Literal(ir::Literal::Int(variant.tag as i64)),
                    ty: ir::Ty::new(ir::TyPrimitive::int8),
                });

                // spacing
                for ty_variant in &ty_variants[0..(variant.tag as usize)] {
                    if ty_variant.ty.is_unit() {
                        continue;
                    }
                    row.push(cr::Expr {
                        kind: cr::ExprKind::Null,
                        ty: ty_variant.ty.clone(),
                    });
                }

                // inner
                row.extend(self.compile_column_list(&variant.inner));

                // spacing
                for ty_variant in &ty_variants[((variant.tag as usize) + 1)..] {
                    if ty_variant.ty.is_unit() {
                        continue;
                    }
                    row.push(cr::Expr {
                        kind: cr::ExprKind::Null,
                        ty: ty_variant.ty.clone(),
                    });
                }

                cr::RelExprKind::Constructed(vec![row])
            }
            ir::ExprKind::EnumEq(_) => todo!(),
            ir::ExprKind::EnumUnwrap(_) => todo!(),

            ir::ExprKind::TupleLookup(lookup) => {
                let base = self.compile_rel(&lookup.base);

                // TODO: this does not take nested tuples or enums into account
                let mut inner =
                    cr::RelExprKind::ProjectRetain(Box::new(base), vec![lookup.position as usize]);

                // In a tuple lookup the repr of the field might change.
                // Currently this only happens for arrays, which change from JSON to SQL repr.
                if expr.ty.kind.is_array() {
                    inner = cr::RelExprKind::JsonUnpack(Box::new(cr::Expr::new_subquery(
                        cr::RelExpr {
                            kind: inner,
                            ty: expr.ty.clone(),
                        },
                    )));
                }

                inner
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

                    let alias = self.bindings.remove(&binding.id).unwrap();
                    cr::RelExprKind::With(alias, Box::new(expr), Box::new(main))
                }
            }

            ir::ExprKind::Switch(_) => todo!(),
        };
        cr::RelExpr {
            kind,
            ty: expr.ty.clone(),
        }
    }

    fn ptr_is_table(&mut self, expr: &ir::Expr) -> bool {
        let ir::TyKind::Function(ty_func) = &self.get_ty_mat(&expr.ty).kind else {
            return false;
        };

        if !ty_func.params.is_empty() {
            return false;
        }
        let ir::TyKind::Array(ty_item) = &self.get_ty_mat(&ty_func.body).kind else {
            return false;
        };

        self.get_ty_mat(ty_item).kind.is_tuple()
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
                let start = self.compile_column(&call.args[1]);
                let end = self.compile_column(&call.args[2]);

                let offset = cr::RelExpr {
                    kind: cr::RelExprKind::Offset(Box::new(array), start.clone()),
                    ty: expr.ty.clone(),
                };

                cr::RelExprKind::Limit(
                    Box::new(offset),
                    new_bin_op(end, "std::sub", start, ir::TyPrimitive::int64),
                )
            }
            "std::index" => {
                let array = self.compile_rel(&call.args[0]);
                let index = self.compile_column(&call.args[1]);

                let offset = cr::RelExpr {
                    ty: array.ty.clone(),
                    kind: cr::RelExprKind::Offset(Box::new(array), index.clone()),
                };

                let limit = cr::RelExpr {
                    ty: offset.ty.clone(),
                    kind: cr::RelExprKind::Limit(Box::new(offset), new_int(1)),
                };

                // drop index column
                cr::RelExprKind::ProjectDrop(Box::new(limit), vec![0])
            }
            "std::map" => {
                let array = self.compile_rel(&call.args[0]);
                let func = &call.args[1];
                let array_ty = array.ty.clone();
                let item_ty = array_ty.kind.as_array().unwrap();

                let needs_unpack = item_ty.kind.is_array();
                if needs_unpack {
                    let iter_name = format!("r{}", self.rvar_alias_gen.gen());

                    // index
                    let mut row = Vec::with_capacity(1);
                    row.push(new_column_of_rel(
                        Some(iter_name.clone()),
                        array_ty.clone(),
                        0,
                        ir::Ty::new(ir::TyPrimitive::int64),
                    ));

                    let unpacked = cr::RelExpr::new_json_unpack(new_column_of_rel(
                        Some(iter_name.clone()),
                        array_ty.clone(),
                        1,
                        *item_ty.clone(),
                    ));

                    // compile func body
                    let func = func.kind.as_function().unwrap();
                    self.functions
                        .insert(func.id, FuncProvider::RelExpr(unpacked.kind));
                    row.extend(self.compile_column_list(&func.body));
                    self.functions.remove(&func.id);

                    let body = cr::RelExpr {
                        kind: cr::RelExprKind::Constructed(vec![row]),
                        ty: expr.ty.clone(),
                    };

                    cr::RelExpr::new_for_each(iter_name, array, body).kind
                } else {
                    let iter_name = format!("r{}", self.rvar_alias_gen.gen());

                    // index
                    let mut row = Vec::with_capacity(1);
                    row.push(new_column_of_rel(
                        Some(iter_name.clone()),
                        array_ty.clone(),
                        0,
                        ir::Ty::new(ir::TyPrimitive::int64),
                    ));

                    // compile func body
                    let func = func.kind.as_function().unwrap();
                    self.functions.insert(
                        func.id,
                        FuncProvider::RelExpr(cr::RelExprKind::SelectRelVar(Some(
                            iter_name.clone(),
                        ))),
                    );
                    row.extend(self.compile_column_list(&func.body));
                    self.functions.remove(&func.id);

                    cr::RelExprKind::ProjectReplace(iter_name, Box::new(array), row)
                }
            }
            "std::filter" => {
                let array = self.compile_rel(&call.args[0]);
                let func = &call.args[1];
                let func = func.kind.as_function().unwrap();

                let iter_name = format!("r{}", self.rvar_alias_gen.gen());

                let item_ref = cr::RelExprKind::SelectRelVar(Some(iter_name.clone()));
                self.functions
                    .insert(func.id, FuncProvider::RelExpr(item_ref));
                let cond = self.compile_column(&func.body);
                self.functions.remove(&func.id);

                cr::RelExprKind::Where(iter_name, Box::new(array), cond)
            }
            "std::sort" => {
                let array = self.compile_rel(&call.args[0]);

                let func = &call.args[1];
                let func = func.kind.as_function().unwrap();

                let iter_name = format!("r{}", self.rvar_alias_gen.gen());

                let item_ref = cr::RelExprKind::SelectRelVar(Some(iter_name.clone()));
                self.functions
                    .insert(func.id, FuncProvider::RelExpr(item_ref));
                let key = self.compile_column(&func.body);
                self.functions.remove(&func.id);

                cr::RelExprKind::OrderBy(iter_name, Box::new(array), key)
            }

            // aggregation functions
            "std::min" | "std::max" | "std::sum" | "std::average" | "std::count" | "std::any"
            | "std::all" | "std::contains" => {
                let array = self.compile_rel(&call.args[0]);
                let item_ty = call.args[0].ty.kind.as_array().unwrap();

                let item = cr::Expr {
                    kind: cr::ExprKind::Subquery(Box::new(cr::RelExpr {
                        kind: cr::RelExprKind::SelectRelVar(None),
                        ty: *item_ty.clone(),
                    })),
                    ty: *item_ty.clone(),
                };

                let mut args = vec![item];
                args.extend(call.args[1..].iter().map(|a| self.compile_column(a)));

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

                let rvar_name = format!("r{}", self.rvar_alias_gen.gen());

                let item = cr::Expr {
                    kind: cr::ExprKind::Subquery(Box::new(cr::RelExpr {
                        kind: cr::RelExprKind::SelectRelVar(Some(rvar_name.clone())),
                        ty: *item_ty.clone(),
                    })),
                    ty: *item_ty.clone(),
                };

                let mut args = vec![item];
                args.extend(call.args[1..].iter().map(|a| self.compile_column(a)));

                cr::RelExprKind::ProjectReplace(
                    rvar_name,
                    Box::new(array),
                    vec![
                        new_column_of_rel(
                            None,
                            call.args[0].ty.clone(),
                            0,
                            ir::Ty::new(ir::TyPrimitive::int64),
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

    /// Compiles an expression into a list of columns
    fn compile_column_list(&mut self, expr: &ir::Expr) -> Vec<cr::Expr> {
        // compile as if this was a rel, which can sink complex operations
        // into the relational expression
        let rel = self.compile_rel(expr);

        match rel.kind {
            // it is just one value: unwrap
            cr::RelExprKind::Constructed(mut rows) if rows.len() == 1 => rows.remove(0),

            // it is actually complex: subquery
            _ => {
                let kind = match &self.get_ty_mat(&expr.ty).kind {
                    ir::TyKind::Primitive(_) => cr::ExprKind::Subquery(Box::new(rel)),
                    ir::TyKind::Array(_) => {
                        // to place an array into columns, it needs to be packed to JSON
                        cr::ExprKind::JsonPack(Box::new(rel))
                    }
                    ir::TyKind::Tuple(fields) => {
                        // non-constructed tuples need to be declared as a rel var and unpacked
                        let rvar_alias = format!("r{}", self.rvar_alias_gen.gen());
                        let rvar = Rc::new(cr::RelVar {
                            rel: Box::new(rel),
                            alias: rvar_alias.clone(),
                        });
                        return fields
                            .iter()
                            .enumerate()
                            .map(|(index, field)| {
                                let rvar_ref = cr::RelExpr {
                                    kind: cr::RelExprKind::SelectRelVar(Some(rvar_alias.clone())),
                                    ty: expr.ty.clone(),
                                };
                                let retain = cr::RelExpr {
                                    kind: cr::RelExprKind::ProjectRetain(
                                        Box::new(rvar_ref),
                                        vec![index],
                                    ),
                                    ty: field.ty.clone(),
                                };
                                let subquery = cr::Expr {
                                    kind: cr::ExprKind::Subquery(Box::new(retain)),
                                    ty: field.ty.clone(),
                                };
                                cr::Expr {
                                    kind: cr::ExprKind::Scoped(rvar.clone(), Box::new(subquery)),
                                    ty: field.ty.clone(),
                                }
                            })
                            .collect();
                    }
                    _ => todo!(),
                };
                vec![cr::Expr {
                    kind,
                    ty: expr.ty.clone(),
                }]
            }
        }
    }

    fn compile_column(&mut self, expr: &ir::Expr) -> cr::Expr {
        let cols = self.compile_column_list(expr);
        assert!(cols.len() == 1, "expected a single column, found: {cols:?}");
        cols.into_iter().next().unwrap()
    }

    fn compile_expr_std(&mut self, expr: &ir::Expr) -> cr::Expr {
        let ir::ExprKind::Call(call) = &expr.kind else {
            unreachable!()
        };

        let ir::ExprKind::Pointer(ir::Pointer::External(ptr)) = &call.function.kind else {
            unreachable!()
        };

        let args = call.args.iter().map(|x| self.compile_column(x)).collect();
        cr::Expr {
            kind: cr::ExprKind::FuncCall(ptr.id.clone(), args),
            ty: expr.ty.clone(),
        }
    }
}

fn new_bin_op(left: cr::Expr, op: &str, right: cr::Expr, ty: ir::TyPrimitive) -> cr::Expr {
    let kind = cr::ExprKind::FuncCall(op.to_string(), vec![left, right]);
    cr::Expr {
        kind,
        ty: ir::Ty::new(ty),
    }
}

fn new_int(int: i64) -> cr::Expr {
    let kind = cr::ExprKind::Literal(ir::Literal::Int(int));
    cr::Expr {
        kind,
        ty: ir::Ty::new(ir::TyPrimitive::int64),
    }
}

fn new_column_of_rel(
    rvar_name: Option<String>,
    rel_ty: ir::Ty,
    col_position: usize,
    col_ty: ir::Ty,
) -> cr::Expr {
    let kind = cr::ExprKind::Subquery(Box::new(cr::RelExpr {
        kind: cr::RelExprKind::ProjectRetain(
            Box::new(cr::RelExpr {
                kind: cr::RelExprKind::SelectRelVar(rvar_name),
                ty: rel_ty,
            }),
            vec![col_position],
        ),
        ty: col_ty.clone(),
    }));
    cr::Expr { kind, ty: col_ty }
}
