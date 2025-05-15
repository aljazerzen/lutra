use std::collections::HashMap;

use crate::utils::IdGenerator;

use super::cr::{self, RelExpr};
use lutra_bin::ir;

struct Context<'t> {
    bindings: HashMap<u32, usize>,
    functions: HashMap<u32, FuncProvider>,

    types: HashMap<&'t ir::Path, &'t ir::Ty>,

    scope_id_gen: IdGenerator,
}

enum FuncProvider {
    Expr(cr::RelExprKind),
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
        scope_id_gen: Default::default(),
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

    fn new_binding(&mut self, rel: cr::RelExpr) -> Box<cr::BoundRelExpr> {
        Box::new(cr::BoundRelExpr {
            rel,
            id: self.scope_id_gen.gen(),
        })
    }

    fn compile_rel(&mut self, expr: &ir::Expr) -> cr::RelExpr {
        let kind = match &expr.kind {
            ir::ExprKind::Literal(lit) => cr::RelExprKind::From(cr::From::Row(vec![cr::ColExpr {
                kind: cr::ColExprKind::Literal(lit.clone()),
                ty: expr.ty.clone(),
            }])),
            ir::ExprKind::Tuple(fields) => {
                let ty_fields = expr.ty.kind.as_tuple().unwrap();

                let mut res_rels = Vec::new();

                let mut res_cols = Vec::new();
                let mut res_ty_fields = Vec::new();
                for (field, ty_field) in std::iter::zip(fields, ty_fields) {
                    match self.compile_column_list(field) {
                        ColumnsOrUnpack::Columns(cols) => {
                            res_cols.extend(cols);
                            res_ty_fields.push(ty_field.clone());
                        }
                        ColumnsOrUnpack::Unpack(rel) => {
                            // finish prev rel
                            if !res_cols.is_empty() {
                                res_rels.push(cr::RelExpr {
                                    kind: cr::RelExprKind::From(cr::From::Row(res_cols)),
                                    ty: ir::Ty::new(ir::TyKind::Tuple(res_ty_fields)),
                                });
                                res_cols = Vec::new();
                                res_ty_fields = Vec::new();
                            }

                            // push this rel
                            res_rels.push(rel);
                        }
                    }
                }

                if res_rels.is_empty() {
                    // simple case: only simple columns, just From::Construction

                    cr::RelExprKind::From(cr::From::Row(res_cols))
                } else {
                    // there are rels, Join needed

                    // finish last rel
                    if !res_cols.is_empty() {
                        res_rels.push(cr::RelExpr {
                            kind: cr::RelExprKind::From(cr::From::Row(res_cols)),
                            ty: ir::Ty::new(ir::TyKind::Tuple(res_ty_fields)),
                        });
                    }

                    let mut res_rels = res_rels.into_iter();
                    let mut result = res_rels.next().unwrap();
                    for r in res_rels {
                        result = cr::RelExpr {
                            ty: ty_concat_tuples(result.ty.clone(), r.ty.clone()),
                            kind: cr::RelExprKind::Join(
                                self.new_binding(result),
                                self.new_binding(r),
                                None,
                            ),
                        };
                    }
                    result.kind
                }
            }
            ir::ExprKind::Array(items) => {
                let rows = items
                    .iter()
                    .enumerate()
                    .map(|(index, x)| {
                        let mut cols = vec![cr::ColExpr {
                            kind: cr::ColExprKind::Literal(ir::Literal::Int(index as i64)),
                            ty: ir::Ty::new(ir::TyPrimitive::int64),
                        }];
                        cols.extend(self.compile_column_list(x).unwrap_columns());

                        cr::RelExpr {
                            kind: cr::RelExprKind::From(cr::From::Row(cols)),
                            ty: expr.ty.clone(),
                        }
                    })
                    .collect();

                cr::RelExprKind::Union(rows)
            }

            ir::ExprKind::Call(call) => match &call.function.kind {
                ir::ExprKind::Pointer(ir::Pointer::External(ptr))
                    if ptr.id.starts_with("std::") =>
                {
                    return self.compile_rel_std(expr);
                }
                ir::ExprKind::Pointer(ir::Pointer::External(ptr)) => {
                    let is_table = self.ptr_is_table(&call.function);

                    if !is_table {
                        tracing::debug!("expected a table getter: {:?}", call.function.ty);
                        todo!("only supported external refs are table functions (no params, return array of tuples)");
                    }

                    cr::RelExprKind::From(cr::From::Table(ptr.id.clone()))
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
                cr::RelExprKind::From(cr::From::RelRef(*name))
            }
            ir::ExprKind::Pointer(ir::Pointer::Parameter(ptr)) => {
                let provider = self.functions.get(&ptr.function_id).unwrap();
                match provider {
                    FuncProvider::Expr(r_expr) => {
                        assert_eq!(ptr.param_position, 0);
                        r_expr.clone()
                    }
                    FuncProvider::Params => {
                        let expr = cr::ColExpr {
                            kind: cr::ColExprKind::Param(ptr.param_position),
                            ty: expr.ty.clone(),
                        };
                        cr::RelExprKind::From(cr::From::Row(vec![expr]))
                    }
                }
            }
            ir::ExprKind::Pointer(ir::Pointer::External(_)) => todo!(),

            ir::ExprKind::Function(_) => todo!(),

            ir::ExprKind::EnumVariant(variant) => {
                let ty_variants = expr.ty.kind.as_enum().unwrap();

                let mut row = Vec::with_capacity(ty_variants.len() + 1);

                // tag
                row.push(cr::ColExpr {
                    kind: cr::ColExprKind::Literal(ir::Literal::Int(variant.tag as i64)),
                    ty: ir::Ty::new(ir::TyPrimitive::int8),
                });

                // spacing
                for ty_variant in &ty_variants[0..(variant.tag as usize)] {
                    if ty_variant.ty.is_unit() {
                        continue;
                    }
                    row.push(cr::ColExpr {
                        kind: cr::ColExprKind::Null,
                        ty: ty_variant.ty.clone(),
                    });
                }

                // inner
                row.extend(self.compile_column_list(&variant.inner).unwrap_columns());

                // spacing
                for ty_variant in &ty_variants[((variant.tag as usize) + 1)..] {
                    if ty_variant.ty.is_unit() {
                        continue;
                    }
                    row.push(cr::ColExpr {
                        kind: cr::ColExprKind::Null,
                        ty: ty_variant.ty.clone(),
                    });
                }

                cr::RelExprKind::From(cr::From::Row(row))
            }
            ir::ExprKind::EnumEq(_) => todo!(),
            ir::ExprKind::EnumUnwrap(_) => todo!(),

            ir::ExprKind::TupleLookup(lookup) => {
                let base = self.compile_rel(&lookup.base);

                // TODO: this does not take nested tuples or enums into account
                let mut rel = cr::RelExprKind::Transform(
                    self.new_binding(base),
                    cr::Transform::ProjectRetain(vec![lookup.position as usize]),
                );

                // In a tuple lookup the repr of the field might change.
                // Currently this only happens for arrays, which change from JSON to SQL repr.
                if expr.ty.kind.is_array() {
                    rel = cr::RelExprKind::Transform(
                        self.new_binding(RelExpr {
                            kind: rel,
                            ty: ir::Ty::new(ir::TyPrimitive::text),
                        }),
                        cr::Transform::JsonUnpack,
                    );
                }

                rel
            }
            ir::ExprKind::Binding(binding) => {
                if let ir::TyKind::Function(_) = &binding.expr.ty.kind {
                    todo!()
                } else {
                    // compile expr
                    let expr = self.compile_rel(&binding.expr);
                    let expr = self.new_binding(expr);
                    self.bindings.insert(binding.id, expr.id);

                    // compile main with expr in ctx
                    let main = self.compile_rel(&binding.main);

                    self.bindings.remove(&binding.id).unwrap();

                    let is_exactly_one_row =
                        expr.rel.ty.kind.is_primitive() || expr.rel.ty.kind.is_tuple();
                    if is_exactly_one_row {
                        // if possible, use BindCorrelated, because it is easier for optimizers to work with
                        cr::RelExprKind::BindCorrelated(expr, Box::new(main))
                    } else {
                        cr::RelExprKind::Bind(expr, Box::new(main))
                    }
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

    fn compile_rel_std(&mut self, expr: &ir::Expr) -> cr::RelExpr {
        let ir::ExprKind::Call(call) = &expr.kind else {
            unreachable!()
        };
        let ir::ExprKind::Pointer(ir::Pointer::External(ptr)) = &call.function.kind else {
            unreachable!()
        };

        let kind = match ptr.id.as_str() {
            "std::slice" => {
                let array = self.compile_rel(&call.args[0]);
                let start = self.compile_column(&call.args[1]);
                let end = self.compile_column(&call.args[2]);

                let start_clamped = new_bin_op(
                    start.clone(),
                    "std::greatest",
                    new_int(0),
                    ir::TyPrimitive::int64,
                );
                let length = new_bin_op(
                    end,
                    "std::sub",
                    start_clamped.clone(),
                    ir::TyPrimitive::int64,
                );
                let length_clamped =
                    new_bin_op(length, "std::greatest", new_int(0), ir::TyPrimitive::int64);

                let rel_offset = cr::RelExpr::new_transform_preserve_ty(
                    array,
                    cr::Transform::Offset(start_clamped),
                    self.scope_id_gen.gen(),
                );

                cr::RelExprKind::Transform(
                    self.new_binding(rel_offset),
                    cr::Transform::Limit(length_clamped),
                )
            }
            "std::index" => {
                let array = self.compile_rel(&call.args[0]);
                let index = self.compile_column(&call.args[1]);

                let rel_offset = cr::RelExpr::new_transform_preserve_ty(
                    array,
                    cr::Transform::Offset(index.clone()),
                    self.scope_id_gen.gen(),
                );

                let rel_limit = cr::RelExpr::new_transform_preserve_ty(
                    rel_offset,
                    cr::Transform::Limit(new_int(1)),
                    self.scope_id_gen.gen(),
                );

                // drop index column
                cr::RelExprKind::Transform(
                    self.new_binding(rel_limit),
                    cr::Transform::ProjectDiscard(vec![0]),
                )
            }
            "std::map" => {
                let array = self.compile_rel(&call.args[0]);
                let array = self.new_binding(array);
                let item_ty = array.rel.ty.kind.as_array().unwrap().clone();
                let func = &call.args[1];

                // index
                let mut row = Vec::with_capacity(1);
                row.push(cr::ColExpr::new_rel_col(
                    array.id,
                    0,
                    ir::Ty::new(ir::TyPrimitive::int64),
                ));

                // compile func body
                let func = func.kind.as_function().unwrap();
                let mut item_ref = cr::RelExprKind::Transform(
                    self.new_binding(cr::RelExpr {
                        kind: cr::RelExprKind::From(cr::From::RelRef(array.id)),
                        ty: array.rel.ty.clone(),
                    }),
                    cr::Transform::ProjectDiscard(vec![0]), // discard index
                );

                if item_ty.kind.is_array() {
                    item_ref = cr::RelExprKind::Transform(
                        self.new_binding(RelExpr {
                            kind: item_ref,
                            ty: ir::Ty::new(ir::TyPrimitive::text),
                        }),
                        cr::Transform::JsonUnpack,
                    );
                }

                self.functions.insert(func.id, FuncProvider::Expr(item_ref));
                row.extend(self.compile_column_list(&func.body).unwrap_columns());
                self.functions.remove(&func.id);

                cr::RelExprKind::BindCorrelated(
                    array,
                    Box::new(cr::RelExpr {
                        kind: cr::RelExprKind::From(cr::From::Row(row)),
                        ty: expr.ty.clone(),
                    }),
                )
            }
            "std::filter" => {
                let array = self.compile_rel(&call.args[0]);
                let array = self.new_binding(array);
                let func = &call.args[1];
                let func = func.kind.as_function().unwrap();

                let item_ref = cr::RelExprKind::Transform(
                    self.new_binding(cr::RelExpr {
                        kind: cr::RelExprKind::From(cr::From::RelRef(array.id)),
                        ty: array.rel.ty.clone(),
                    }),
                    cr::Transform::ProjectDiscard(vec![0]), // discard index
                );

                self.functions.insert(func.id, FuncProvider::Expr(item_ref));
                let cond = self.compile_column(&func.body);
                self.functions.remove(&func.id);

                cr::RelExprKind::Transform(array, cr::Transform::Where(cond))
            }
            "std::sort" => {
                let array = self.compile_rel(&call.args[0]);
                let array = self.new_binding(array);

                let func = &call.args[1];
                let func = func.kind.as_function().unwrap();

                let item_ref = cr::RelExprKind::Transform(
                    self.new_binding(cr::RelExpr {
                        kind: cr::RelExprKind::From(cr::From::RelRef(array.id)),
                        ty: array.rel.ty.clone(),
                    }),
                    cr::Transform::ProjectDiscard(vec![0]), // discard index
                );

                self.functions.insert(func.id, FuncProvider::Expr(item_ref));
                let key = self.compile_column(&func.body);
                self.functions.remove(&func.id);

                cr::RelExprKind::Transform(array, cr::Transform::OrderBy(key))
            }

            // aggregation functions
            "std::min" | "std::max" | "std::sum" | "std::average" | "std::count" | "std::any"
            | "std::all" | "std::contains" => {
                let array = self.compile_rel(&call.args[0]);
                let array = self.new_binding(array);

                let item_ty = call.args[0].ty.kind.as_array().unwrap();

                let item = cr::ColExpr::new_rel_col(array.id, 1, *item_ty.clone());

                let mut args = vec![item];
                args.extend(call.args[1..].iter().map(|a| self.compile_column(a)));

                cr::RelExprKind::Transform(
                    array,
                    cr::Transform::Aggregate(vec![cr::ColExpr {
                        kind: cr::ColExprKind::FuncCall(ptr.id.clone(), args),
                        ty: expr.ty.clone(),
                    }]),
                )
            }

            // window functions
            "std::row_number" | "std::lead" | "std::lag" => {
                let array = self.compile_rel(&call.args[0]);
                let array = self.new_binding(array);

                let item_ty = expr.ty.kind.as_array().unwrap();
                let item = cr::ColExpr::new_rel_col(array.id, 1, *item_ty.clone());

                let mut args = vec![item];
                args.extend(call.args[1..].iter().map(|a| self.compile_column(a)));

                let row = vec![
                    cr::ColExpr::new_rel_col(array.id, 0, ir::Ty::new(ir::TyPrimitive::int64)),
                    cr::ColExpr {
                        kind: cr::ColExprKind::FuncCall(ptr.id.clone(), args),
                        ty: expr.ty.clone(),
                    },
                ];
                cr::RelExprKind::Transform(array, cr::Transform::Aggregate(row))
            }

            "std::to_columnar" => {
                let array = self.compile_rel(&call.args[0]);
                let array = self.new_binding(array);

                let ty_out_fields = expr.ty.kind.as_tuple().unwrap();

                let field0 = cr::RelExpr {
                    kind: cr::RelExprKind::Transform(
                        self.new_binding(cr::RelExpr {
                            kind: cr::RelExprKind::From(cr::From::RelRef(array.id)),
                            ty: array.rel.ty.clone(),
                        }),
                        cr::Transform::ProjectRetain(vec![
                            0, // index
                            1, // first tuple field
                        ]),
                    ),
                    ty: ty_out_fields[0].ty.clone(),
                };
                let field1 = cr::RelExpr {
                    kind: cr::RelExprKind::Transform(
                        self.new_binding(cr::RelExpr {
                            kind: cr::RelExprKind::From(cr::From::RelRef(array.id)),
                            ty: array.rel.ty.clone(),
                        }),
                        cr::Transform::ProjectRetain(vec![
                            0, // index
                            2, // second tuple field
                        ]),
                    ),
                    ty: ty_out_fields[0].ty.clone(),
                };

                let field0_ref = cr::ColExpr::new_subquery(field0);
                let field1_ref = cr::ColExpr::new_subquery(field1);

                cr::RelExprKind::Transform(
                    array,
                    cr::Transform::Aggregate(vec![field0_ref, field1_ref]),
                )
            }

            "std::from_columnar" => {
                let tuple = self.compile_rel(&call.args[0]);
                let tuple = self.new_binding(tuple);

                let ty_in_fields = tuple.rel.ty.kind.as_tuple().unwrap();

                let ty_out_item = expr.ty.kind.as_array().unwrap();
                let ty_out_fields = ty_out_item.kind.as_tuple().unwrap();

                // construct correlated subqueries
                let field0 = {
                    let input = cr::RelExpr::new_rel_col(
                        tuple.id,
                        tuple.rel.ty.clone(),
                        0,
                        ir::Ty::new(ir::TyPrimitive::text),
                        &mut self.scope_id_gen,
                    );
                    RelExpr {
                        ty: ty_in_fields[0].ty.clone(),
                        kind: cr::RelExprKind::Transform(
                            self.new_binding(input),
                            cr::Transform::JsonUnpack,
                        ),
                    }
                };
                let field0 = self.new_binding(field0);
                let field1 = {
                    let input = cr::RelExpr::new_rel_col(
                        tuple.id,
                        tuple.rel.ty.clone(),
                        1,
                        ir::Ty::new(ir::TyPrimitive::text),
                        &mut self.scope_id_gen,
                    );
                    RelExpr {
                        ty: ty_in_fields[1].ty.clone(),
                        kind: cr::RelExprKind::Transform(
                            self.new_binding(input),
                            cr::Transform::JsonUnpack,
                        ),
                    }
                };
                let field1 = self.new_binding(field1);

                let ty_index = ir::Ty::new(ir::TyPrimitive::int64);
                let join_cond = cr::ColExpr {
                    ty: ir::Ty::new(ir::TyPrimitive::bool),
                    kind: cr::ColExprKind::FuncCall(
                        "std::eq".into(),
                        vec![
                            cr::ColExpr::new_rel_col(field0.id, 0, ty_index.clone()),
                            cr::ColExpr::new_rel_col(field1.id, 0, ty_index.clone()),
                        ],
                    ),
                };

                let ty_index_field = ir::TyTupleField {
                    ty: ty_index.clone(),
                    name: None,
                };
                let joined_type = ir::Ty::new(ir::TyKind::Tuple(vec![
                    ty_index_field.clone(),
                    ty_out_fields[0].clone(),
                    ty_index_field,
                    ty_out_fields[1].clone(),
                ]));

                cr::RelExprKind::BindCorrelated(
                    tuple,
                    Box::new(cr::RelExpr {
                        kind: cr::RelExprKind::Transform(
                            self.new_binding(cr::RelExpr {
                                kind: cr::RelExprKind::Join(
                                    field0,
                                    field1,
                                    Some(Box::new(join_cond)),
                                ),
                                ty: joined_type,
                            }),
                            cr::Transform::ProjectRetain(vec![0, 1, 3]),
                        ),
                        ty: expr.ty.clone(),
                    }),
                )
            }

            _ => {
                let expr = self.compile_expr_std(expr);
                cr::RelExprKind::From(cr::From::Row(vec![expr]))
            }
        };
        cr::RelExpr {
            kind,
            ty: expr.ty.clone(),
        }
    }

    /// Compiles an expression into a column
    fn compile_column(&mut self, expr: &ir::Expr) -> cr::ColExpr {
        let cols = self.compile_column_list(expr);
        let ColumnsOrUnpack::Columns(cols) = cols else {
            panic!("expected columns, found: {cols:?}");
        };
        assert!(cols.len() == 1, "expected a single column, found: {cols:?}");
        cols.into_iter().next().unwrap()
    }

    /// Compiles an expression into a list of columns
    fn compile_column_list(&mut self, expr: &ir::Expr) -> ColumnsOrUnpack {
        // compile as if this was a rel, which can sink complex operations
        // into the relational expression
        let rel = self.compile_rel(expr);

        // optimization: simplify `ProjectRetain` and `ProjectDiscard` to InputRelCol(x)
        if let Some((scope_id, col_position)) = try_simplify_input_rel_col(&rel) {
            tracing::debug!(
                "simplifying:\n{rel:#?} .. to ColExprKind::InputRelCol({scope_id:?}, {col_position})"
            );
            return ColumnsOrUnpack::Columns(vec![cr::ColExpr::new_rel_col(
                scope_id,
                col_position,
                rel.ty,
            )]);
        }

        match rel.kind {
            // it is just one constructed row: unwrap
            cr::RelExprKind::From(cr::From::Row(row))
                if !self.get_ty_mat(&expr.ty).kind.is_array() =>
            {
                ColumnsOrUnpack::Columns(row)
            }

            // it is actually complex: subquery
            _ => {
                match &self.get_ty_mat(&expr.ty).kind {
                    ir::TyKind::Primitive(_) | ir::TyKind::Array(_) => {
                        // return as a subquery

                        // for arrays, using a subquery will trigger JSON packing in queries.rs
                        ColumnsOrUnpack::Columns(vec![cr::ColExpr::new_subquery(rel)])
                    }
                    ir::TyKind::Tuple(_) => {
                        // non-constructed tuples need to be declared as a rel var and unpacked

                        ColumnsOrUnpack::Unpack(rel)
                    }
                    _ => todo!(),
                }
            }
        }
    }

    fn compile_expr_std(&mut self, expr: &ir::Expr) -> cr::ColExpr {
        let ir::ExprKind::Call(call) = &expr.kind else {
            unreachable!()
        };

        let ir::ExprKind::Pointer(ir::Pointer::External(ptr)) = &call.function.kind else {
            unreachable!()
        };

        let args = call.args.iter().map(|x| self.compile_column(x)).collect();
        cr::ColExpr {
            kind: cr::ColExprKind::FuncCall(ptr.id.clone(), args),
            ty: expr.ty.clone(),
        }
    }
}

#[derive(Debug)]
enum ColumnsOrUnpack {
    Columns(Vec<cr::ColExpr>),

    // A relation whose columns should be unpacked
    Unpack(cr::RelExpr),
}

impl ColumnsOrUnpack {
    #[track_caller]
    fn unwrap_columns(self) -> Vec<cr::ColExpr> {
        match self {
            ColumnsOrUnpack::Columns(cols) => cols,
            ColumnsOrUnpack::Unpack(rel) => panic!("{rel:?}"),
        }
    }
}

fn new_bin_op(left: cr::ColExpr, op: &str, right: cr::ColExpr, ty: ir::TyPrimitive) -> cr::ColExpr {
    let kind = cr::ColExprKind::FuncCall(op.to_string(), vec![left, right]);
    cr::ColExpr {
        kind,
        ty: ir::Ty::new(ty),
    }
}

fn new_int(int: i64) -> cr::ColExpr {
    let kind = cr::ColExprKind::Literal(ir::Literal::Int(int));
    cr::ColExpr {
        kind,
        ty: ir::Ty::new(ir::TyPrimitive::int64),
    }
}

fn try_simplify_input_rel_col(rel: &cr::RelExpr) -> Option<(usize, usize)> {
    match &rel.kind {
        cr::RelExprKind::From(cr::From::RelRef(scope_id)) => Some((*scope_id, 0)),
        cr::RelExprKind::From(_) => None,

        cr::RelExprKind::Transform(input, transform) => {
            let (scope_id, offset) = try_simplify_input_rel_col(&input.rel)?;

            match transform {
                cr::Transform::ProjectRetain(retain) if retain.len() == 1 => {
                    Some((scope_id, offset + retain[0]))
                }
                cr::Transform::ProjectDiscard(discards) if discards == &[0] => {
                    Some((scope_id, offset + 1))
                }

                _ => None,
            }
        }

        _ => None,
    }
}

fn ty_concat_tuples(a: ir::Ty, b: ir::Ty) -> ir::Ty {
    let a = a.kind.into_tuple().unwrap();
    let b = b.kind.into_tuple().unwrap();
    let mut concat = a;
    concat.extend(b);
    ir::Ty::new(ir::TyKind::Tuple(concat))
}
