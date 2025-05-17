use std::collections::HashMap;

use crate::utils::IdGenerator;

use super::cr::{self, Expr};
use lutra_bin::ir;

struct Context<'t> {
    bindings: HashMap<u32, usize>,
    functions: HashMap<u32, FuncProvider>,

    types: HashMap<&'t ir::Path, &'t ir::Ty>,

    scope_id_gen: IdGenerator,
}

enum FuncProvider {
    Expr(cr::ExprKind),
    Params,
}

pub fn compile(program: &ir::Program) -> (cr::Expr, HashMap<&ir::Path, &ir::Ty>) {
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

    fn new_binding(&mut self, rel: cr::Expr) -> Box<cr::BoundExpr> {
        Box::new(cr::BoundExpr {
            rel,
            id: self.scope_id_gen.gen(),
        })
    }

    pub fn new_rel_col(
        &mut self,
        rel: &cr::BoundExpr,
        col_position: usize,
        col_ty: ir::Ty,
    ) -> cr::Expr {
        Expr {
            kind: cr::ExprKind::Transform(
                self.new_binding(Expr {
                    kind: cr::ExprKind::From(cr::From::RelRef(rel.id)),
                    ty: rel.rel.ty.clone(),
                }),
                cr::Transform::ProjectRetain(vec![col_position]),
            ),
            ty: col_ty,
        }
    }

    fn compile_rel(&mut self, expr: &ir::Expr) -> cr::Expr {
        let kind = match &expr.kind {
            ir::ExprKind::Literal(lit) => cr::ExprKind::From(cr::From::Row(vec![cr::Expr {
                kind: cr::ExprKind::From(cr::From::Literal(lit.clone())),
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
                                res_rels.push(cr::Expr {
                                    kind: cr::ExprKind::From(cr::From::Row(res_cols)),
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
                    // simple case: only simple columns, just From::Row

                    cr::ExprKind::From(cr::From::Row(res_cols))
                } else {
                    // there are rels, Join needed

                    // finish last rel
                    if !res_cols.is_empty() {
                        res_rels.push(cr::Expr {
                            kind: cr::ExprKind::From(cr::From::Row(res_cols)),
                            ty: ir::Ty::new(ir::TyKind::Tuple(res_ty_fields)),
                        });
                    }

                    let mut res_rels = res_rels.into_iter();
                    let mut result = res_rels.next().unwrap();
                    for r in res_rels {
                        result = cr::Expr {
                            ty: ty_concat_tuples(result.ty.clone(), r.ty.clone()),
                            kind: cr::ExprKind::Join(
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
                        let mut cols = vec![cr::Expr {
                            kind: cr::ExprKind::From(cr::From::Literal(ir::Literal::Int(
                                index as i64,
                            ))),
                            ty: ir::Ty::new(ir::TyPrimitive::int64),
                        }];
                        cols.extend(self.compile_column_list(x).unwrap_columns());

                        cr::Expr {
                            kind: cr::ExprKind::From(cr::From::Row(cols)),
                            ty: expr.ty.clone(),
                        }
                    })
                    .collect();

                cr::ExprKind::Union(rows)
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

                    let parts = ptr.id.split("::");
                    let table_name = parts.last().unwrap().to_string();
                    cr::ExprKind::From(cr::From::Table(table_name))
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
                cr::ExprKind::From(cr::From::RelRef(*name))
            }
            ir::ExprKind::Pointer(ir::Pointer::Parameter(ptr)) => {
                let provider = self.functions.get(&ptr.function_id).unwrap();
                match provider {
                    FuncProvider::Expr(r_expr) => {
                        assert_eq!(ptr.param_position, 0);
                        r_expr.clone()
                    }
                    FuncProvider::Params => {
                        let expr = cr::Expr {
                            kind: cr::ExprKind::From(cr::From::Param(ptr.param_position)),
                            ty: expr.ty.clone(),
                        };
                        cr::ExprKind::From(cr::From::Row(vec![expr]))
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
                    kind: cr::ExprKind::From(cr::From::Literal(ir::Literal::Int(
                        variant.tag as i64,
                    ))),
                    ty: ir::Ty::new(ir::TyPrimitive::int8),
                });

                // spacing
                for ty_variant in &ty_variants[0..(variant.tag as usize)] {
                    if ty_variant.ty.is_unit() {
                        continue;
                    }
                    row.push(cr::Expr {
                        kind: cr::ExprKind::From(cr::From::Null),
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
                    row.push(cr::Expr {
                        kind: cr::ExprKind::From(cr::From::Null),
                        ty: ty_variant.ty.clone(),
                    });
                }

                cr::ExprKind::From(cr::From::Row(row))
            }
            ir::ExprKind::EnumEq(_) => todo!(),
            ir::ExprKind::EnumUnwrap(_) => todo!(),

            ir::ExprKind::TupleLookup(lookup) => {
                let base = self.compile_rel(&lookup.base);

                // TODO: this does not take nested tuples or enums into account
                let mut rel = cr::ExprKind::Transform(
                    self.new_binding(base),
                    cr::Transform::ProjectRetain(vec![lookup.position as usize]),
                );

                // In a tuple lookup the repr of the field might change.
                // Currently this only happens for arrays, which change from JSON to SQL repr.
                if expr.ty.kind.is_array() {
                    rel = cr::ExprKind::From(cr::From::JsonUnpack(Box::new(Expr {
                        kind: rel,
                        ty: ir::Ty::new(ir::TyPrimitive::text),
                    })));
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
                        cr::ExprKind::BindCorrelated(expr, Box::new(main))
                    } else {
                        cr::ExprKind::Bind(expr, Box::new(main))
                    }
                }
            }

            ir::ExprKind::Switch(_) => todo!(),
        };
        cr::Expr {
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

    fn compile_rel_std(&mut self, expr: &ir::Expr) -> cr::Expr {
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

                let rel_offset = cr::Expr::new_transform_preserve_ty(
                    array,
                    cr::Transform::Offset(Box::new(start_clamped)),
                    self.scope_id_gen.gen(),
                );

                cr::ExprKind::Transform(
                    self.new_binding(rel_offset),
                    cr::Transform::Limit(Box::new(length_clamped)),
                )
            }
            "std::index" => {
                let array = self.compile_rel(&call.args[0]);
                let index = self.compile_column(&call.args[1]);

                let rel_offset = cr::Expr::new_transform_preserve_ty(
                    array,
                    cr::Transform::Offset(Box::new(index)),
                    self.scope_id_gen.gen(),
                );

                let rel_limit = cr::Expr::new_transform_preserve_ty(
                    rel_offset,
                    cr::Transform::Limit(Box::new(new_int(1))),
                    self.scope_id_gen.gen(),
                );

                // drop index column
                cr::ExprKind::Transform(
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
                row.push(self.new_rel_col(&array, 0, ir::Ty::new(ir::TyPrimitive::int64)));

                // compile func body
                let func = func.kind.as_function().unwrap();
                let mut item_ref = cr::ExprKind::Transform(
                    self.new_binding(cr::Expr {
                        kind: cr::ExprKind::From(cr::From::RelRef(array.id)),
                        ty: array.rel.ty.clone(),
                    }),
                    cr::Transform::ProjectDiscard(vec![0]), // discard index
                );

                if item_ty.kind.is_array() {
                    item_ref = cr::ExprKind::From(cr::From::JsonUnpack(Box::new(Expr {
                        kind: item_ref,
                        ty: ir::Ty::new(ir::TyPrimitive::text),
                    })));
                }

                self.functions.insert(func.id, FuncProvider::Expr(item_ref));
                row.extend(self.compile_column_list(&func.body).unwrap_columns());
                self.functions.remove(&func.id);

                cr::ExprKind::BindCorrelated(
                    array,
                    Box::new(cr::Expr {
                        kind: cr::ExprKind::From(cr::From::Row(row)),
                        ty: expr.ty.clone(),
                    }),
                )
            }
            "std::filter" => {
                let array = self.compile_rel(&call.args[0]);
                let array = self.new_binding(array);
                let func = &call.args[1];
                let func = func.kind.as_function().unwrap();

                let item_ref = cr::ExprKind::Transform(
                    self.new_binding(cr::Expr {
                        kind: cr::ExprKind::From(cr::From::RelRef(array.id)),
                        ty: array.rel.ty.clone(),
                    }),
                    cr::Transform::ProjectDiscard(vec![0]), // discard index
                );

                self.functions.insert(func.id, FuncProvider::Expr(item_ref));
                let cond = self.compile_column(&func.body);
                self.functions.remove(&func.id);

                cr::ExprKind::Transform(array, cr::Transform::Where(Box::new(cond)))
            }
            "std::sort" => {
                let array = self.compile_rel(&call.args[0]);
                let array = self.new_binding(array);

                let func = &call.args[1];
                let func = func.kind.as_function().unwrap();

                let item_ref = cr::ExprKind::Transform(
                    self.new_binding(cr::Expr {
                        kind: cr::ExprKind::From(cr::From::RelRef(array.id)),
                        ty: array.rel.ty.clone(),
                    }),
                    cr::Transform::ProjectDiscard(vec![0]), // discard index
                );

                self.functions.insert(func.id, FuncProvider::Expr(item_ref));
                let key = self.compile_column(&func.body);
                self.functions.remove(&func.id);

                cr::ExprKind::Transform(array, cr::Transform::OrderBy(Box::new(key)))
            }

            // aggregation functions
            "std::min" | "std::max" | "std::sum" | "std::average" | "std::count" | "std::any"
            | "std::all" | "std::contains" => {
                let array = self.compile_rel(&call.args[0]);
                let array = self.new_binding(array);

                let item_ty = call.args[0].ty.kind.as_array().unwrap();

                let item = self.new_rel_col(&array, 1, *item_ty.clone());

                let mut args = vec![item];
                args.extend(call.args[1..].iter().map(|a| self.compile_column(a)));

                cr::ExprKind::Transform(
                    array,
                    cr::Transform::Aggregate(vec![cr::Expr {
                        kind: cr::ExprKind::From(cr::From::FuncCall(ptr.id.clone(), args)),
                        ty: expr.ty.clone(),
                    }]),
                )
            }

            // window functions
            "std::row_number" | "std::lead" | "std::lag" => {
                let array = self.compile_rel(&call.args[0]);
                let array = self.new_binding(array);

                let item_ty = expr.ty.kind.as_array().unwrap();
                let item = self.new_rel_col(&array, 1, *item_ty.clone());

                let mut args = vec![item];
                args.extend(call.args[1..].iter().map(|a| self.compile_column(a)));

                let row = vec![
                    self.new_rel_col(&array, 0, ir::Ty::new(ir::TyPrimitive::int64)),
                    cr::Expr {
                        kind: cr::ExprKind::From(cr::From::FuncCall(ptr.id.clone(), args)),
                        ty: expr.ty.clone(),
                    },
                ];
                cr::ExprKind::Transform(array, cr::Transform::Window(row))
            }

            "std::to_columnar" => {
                let array = self.compile_rel(&call.args[0]);
                let array = self.new_binding(array);

                let ty_out_fields = expr.ty.kind.as_tuple().unwrap();

                let field0 = cr::Expr {
                    kind: cr::ExprKind::Transform(
                        self.new_binding(cr::Expr {
                            kind: cr::ExprKind::From(cr::From::RelRef(array.id)),
                            ty: array.rel.ty.clone(),
                        }),
                        cr::Transform::ProjectRetain(vec![
                            0, // index
                            1, // first tuple field
                        ]),
                    ),
                    ty: ty_out_fields[0].ty.clone(),
                };
                let field1 = cr::Expr {
                    kind: cr::ExprKind::Transform(
                        self.new_binding(cr::Expr {
                            kind: cr::ExprKind::From(cr::From::RelRef(array.id)),
                            ty: array.rel.ty.clone(),
                        }),
                        cr::Transform::ProjectRetain(vec![
                            0, // index
                            2, // second tuple field
                        ]),
                    ),
                    ty: ty_out_fields[0].ty.clone(),
                };

                cr::ExprKind::Transform(array, cr::Transform::Aggregate(vec![field0, field1]))
            }

            "std::from_columnar" => {
                let tuple = self.compile_rel(&call.args[0]);
                let tuple = self.new_binding(tuple);

                let ty_in_fields = tuple.rel.ty.kind.as_tuple().unwrap();

                let ty_out_item = expr.ty.kind.as_array().unwrap();
                let ty_out_fields = ty_out_item.kind.as_tuple().unwrap();

                // construct correlated subqueries
                let field0 = {
                    let input = self.new_rel_col(&tuple, 0, ir::Ty::new(ir::TyPrimitive::text));
                    Expr {
                        ty: ty_in_fields[0].ty.clone(),
                        kind: cr::ExprKind::From(cr::From::JsonUnpack(Box::new(input))),
                    }
                };
                let field0 = self.new_binding(field0);
                let field1 = {
                    let input = self.new_rel_col(&tuple, 1, ir::Ty::new(ir::TyPrimitive::text));
                    Expr {
                        ty: ty_in_fields[1].ty.clone(),
                        kind: cr::ExprKind::From(cr::From::JsonUnpack(Box::new(input))),
                    }
                };
                let field1 = self.new_binding(field1);

                let ty_index = ir::Ty::new(ir::TyPrimitive::int64);
                let join_cond = cr::Expr {
                    ty: ir::Ty::new(ir::TyPrimitive::bool),
                    kind: cr::ExprKind::From(cr::From::FuncCall(
                        "std::eq".into(),
                        vec![
                            self.new_rel_col(&field0, 0, ty_index.clone()),
                            self.new_rel_col(&field1, 0, ty_index.clone()),
                        ],
                    )),
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

                cr::ExprKind::BindCorrelated(
                    tuple,
                    Box::new(cr::Expr {
                        kind: cr::ExprKind::Transform(
                            self.new_binding(cr::Expr {
                                kind: cr::ExprKind::Join(field0, field1, Some(Box::new(join_cond))),
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
                cr::ExprKind::From(cr::From::Row(vec![expr]))
            }
        };
        cr::Expr {
            kind,
            ty: expr.ty.clone(),
        }
    }

    /// Compiles an expression into a column
    fn compile_column(&mut self, expr: &ir::Expr) -> cr::Expr {
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

        match rel.kind {
            // it is just one constructed row: unwrap
            cr::ExprKind::From(cr::From::Row(row))
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
                        ColumnsOrUnpack::Columns(vec![rel])
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

    fn compile_expr_std(&mut self, expr: &ir::Expr) -> cr::Expr {
        let ir::ExprKind::Call(call) = &expr.kind else {
            unreachable!()
        };

        let ir::ExprKind::Pointer(ir::Pointer::External(ptr)) = &call.function.kind else {
            unreachable!()
        };

        let args = call.args.iter().map(|x| self.compile_column(x)).collect();
        cr::Expr {
            kind: cr::ExprKind::From(cr::From::FuncCall(ptr.id.clone(), args)),
            ty: expr.ty.clone(),
        }
    }
}

#[derive(Debug)]
enum ColumnsOrUnpack {
    Columns(Vec<cr::Expr>),

    // A relation whose columns should be unpacked
    Unpack(cr::Expr),
}

impl ColumnsOrUnpack {
    #[track_caller]
    fn unwrap_columns(self) -> Vec<cr::Expr> {
        match self {
            ColumnsOrUnpack::Columns(cols) => cols,
            ColumnsOrUnpack::Unpack(rel) => panic!("{rel:?}"),
        }
    }
}

fn new_bin_op(left: cr::Expr, op: &str, right: cr::Expr, ty: ir::TyPrimitive) -> cr::Expr {
    let kind = cr::ExprKind::From(cr::From::FuncCall(op.to_string(), vec![left, right]));
    cr::Expr {
        kind,
        ty: ir::Ty::new(ty),
    }
}

fn new_int(int: i64) -> cr::Expr {
    let kind = cr::ExprKind::From(cr::From::Literal(ir::Literal::Int(int)));
    cr::Expr {
        kind,
        ty: ir::Ty::new(ir::TyPrimitive::int64),
    }
}

fn ty_concat_tuples(a: ir::Ty, b: ir::Ty) -> ir::Ty {
    let a = a.kind.into_tuple().unwrap();
    let b = b.kind.into_tuple().unwrap();
    let mut concat = a;
    concat.extend(b);
    ir::Ty::new(ir::TyKind::Tuple(concat))
}
