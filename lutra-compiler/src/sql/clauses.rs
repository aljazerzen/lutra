use std::{borrow::Cow, collections::HashMap};

use crate::sql::utils::RelCols;
use crate::sql::{cr, utils};
use crate::utils::IdGenerator;
use lutra_bin::ir;

pub(super) struct Context<'t> {
    bindings: HashMap<u32, usize>,
    functions: HashMap<u32, FuncProvider>,

    defs: HashMap<&'t ir::Path, &'t ir::Ty>,

    scope_id_gen: IdGenerator,
}

enum FuncProvider {
    Expr(cr::ExprKind),
    QueryParam,
}

pub fn compile(program: &ir::Program) -> (cr::Expr, HashMap<&ir::Path, &ir::Ty>) {
    let mut ctx = Context {
        bindings: Default::default(),
        functions: Default::default(),
        defs: program
            .defs
            .iter()
            .map(|def| (&def.name, &def.ty))
            .collect(),
        scope_id_gen: Default::default(),
    };

    assert!(
        program.main.ty.kind.is_function(),
        "expected program.main to be a function, got: {:?}",
        program.main.ty
    );

    // find the top-level function
    let func = match &program.main.kind {
        ir::ExprKind::Function(func) => Cow::Borrowed(func.as_ref()),
        _ => Cow::Owned(wrap_into_func_call(&program.main)),
    };
    ctx.functions.insert(func.id, FuncProvider::QueryParam);

    let body = ctx.compile_rel(&func.body);

    (body, ctx.defs)
}

impl<'a> Context<'a> {
    pub(super) fn get_ty_mat(&self, ty: &'a ir::Ty) -> &'a ir::Ty {
        match &ty.kind {
            ir::TyKind::Ident(path) => self.defs.get(path).unwrap(),
            _ => ty,
        }
    }

    fn new_binding(&mut self, rel: cr::Expr) -> Box<cr::BoundExpr> {
        Box::new(cr::BoundExpr {
            rel,
            id: self.scope_id_gen.next(),
        })
    }

    pub fn new_rel_col(
        &mut self,
        rel: &cr::BoundExpr,
        col_position: usize,
        col_ty: ir::Ty,
    ) -> cr::Expr {
        cr::Expr {
            kind: cr::ExprKind::Transform(
                self.new_binding(cr::Expr::new_rel_ref(rel)),
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
                    match self.compile_column_list(&field.expr) {
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
                        let row = vec![cr::Expr {
                            kind: cr::ExprKind::From(cr::From::Literal(ir::Literal::int64(
                                index as i64,
                            ))),
                            ty: ir::Ty::new(ir::TyPrimitive::int64),
                        }];

                        let x = self.compile_column_list(x);

                        cr::Expr {
                            kind: self.row_or_join(row, x),
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
                    FuncProvider::QueryParam => {
                        assert_eq!(ptr.param_position, 0);

                        match &self.get_ty_mat(&expr.ty).kind {
                            ir::TyKind::Primitive(_) | ir::TyKind::Tuple(_) => {
                                let columns = self
                                    .rel_cols_ty_nested(&expr.ty)
                                    .enumerate()
                                    .map(|(p, field_ty)| cr::Expr {
                                        kind: cr::ExprKind::From(cr::From::Param(p as u8)),
                                        ty: field_ty.into_owned(),
                                    })
                                    .collect();
                                cr::ExprKind::From(cr::From::Row(columns))
                            }

                            ir::TyKind::Array(_) | ir::TyKind::Enum(_) => {
                                // param will be encoded as JSON
                                cr::ExprKind::From(cr::From::JsonUnpack(Box::new(cr::Expr {
                                    kind: cr::ExprKind::From(cr::From::Param(0)),
                                    ty: expr.ty.clone(),
                                })))
                            }

                            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => unreachable!(),
                        }
                    }
                }
            }
            ir::ExprKind::Pointer(ir::Pointer::External(_)) => todo!(),

            ir::ExprKind::Function(_) => todo!(),

            ir::ExprKind::EnumVariant(variant) => {
                let ir::TyKind::Enum(ty_variants) = &self.get_ty_mat(&expr.ty).kind else {
                    panic!("invalid program");
                };

                if utils::is_maybe(ty_variants) {
                    // special case: nullable column

                    if variant.tag == 0 {
                        let null = cr::Expr {
                            kind: cr::ExprKind::From(cr::From::Null),
                            ty: ty_variants[1].ty.clone(),
                        };
                        cr::ExprKind::From(cr::From::Row(vec![null]))
                    } else {
                        self.compile_rel(&variant.inner).kind
                    }
                } else {
                    let mut row = Vec::with_capacity(ty_variants.len() + 1);

                    // tag
                    row.push(cr::Expr {
                        kind: cr::ExprKind::From(cr::From::Literal(ir::Literal::int8(
                            variant.tag as i8,
                        ))),
                        ty: ir::Ty::new(ir::TyPrimitive::int8),
                    });

                    // spacing
                    let selected = variant.tag as usize;
                    row.extend(ty_variants.iter().take(selected).flat_map(|v| {
                        self.rel_cols_ty_nested(&v.ty).map(|ty| cr::Expr {
                            kind: cr::ExprKind::From(cr::From::Null),
                            ty: ty.into_owned(),
                        })
                    }));

                    // inner
                    row.extend(self.compile_column_list(&variant.inner).unwrap_columns());

                    // spacing
                    row.extend(ty_variants.iter().skip(selected + 1).flat_map(|v| {
                        self.rel_cols_ty_nested(&v.ty).map(|ty| cr::Expr {
                            kind: cr::ExprKind::From(cr::From::Null),
                            ty: ty.into_owned(),
                        })
                    }));

                    cr::ExprKind::From(cr::From::Row(row))
                }
            }
            ir::ExprKind::EnumEq(enum_eq) => {
                let base = self.compile_rel(&enum_eq.subject);

                let ir::TyKind::Enum(variants) = &self.get_ty_mat(&enum_eq.subject.ty).kind else {
                    panic!("invalid program");
                };
                if utils::is_maybe(variants) {
                    // a nullable column
                    let op = if enum_eq.tag == 0 {
                        "is_null"
                    } else {
                        "is_not_null"
                    };
                    cr::ExprKind::From(cr::From::FuncCall(op.into(), vec![base]))
                } else {
                    // tag + one column for variant

                    let base = self.new_binding(base);

                    let tag = cr::Expr {
                        kind: cr::ExprKind::Transform(base, cr::Transform::ProjectRetain(vec![0])),
                        ty: ir::Ty::new(ir::TyPrimitive::int8),
                    };

                    let args = vec![tag, new_int8(enum_eq.tag as i8)];
                    cr::ExprKind::From(cr::From::FuncCall("std::eq".to_string(), args))
                }
            }
            ir::ExprKind::EnumUnwrap(enum_unwrap) => {
                let base = self.compile_rel(&enum_unwrap.subject);

                let ir::TyKind::Enum(variants) = &self.get_ty_mat(&enum_unwrap.subject.ty).kind
                else {
                    panic!("invalid program");
                };

                if utils::is_maybe(variants) {
                    // a nullable column
                    return base;
                } else {
                    // tag + one column for variant

                    let base = self.new_binding(base);

                    // count number of columns in front of selected variant and the first tag
                    let start = 1 + variants
                        .iter()
                        .take(enum_unwrap.tag as usize)
                        .map(|v| self.rel_cols_nested(&v.ty, String::new()).count())
                        .sum::<usize>();

                    // count number of columns of the selected variant
                    let end = start
                        + self
                            .rel_cols_nested(&variants[enum_unwrap.tag as usize].ty, String::new())
                            .count();

                    cr::ExprKind::Transform(
                        base,
                        cr::Transform::ProjectRetain((start..end).collect()),
                    )
                }
            }

            ir::ExprKind::TupleLookup(lookup) => {
                let base = self.compile_rel(&lookup.base);
                let position = lookup.position as usize;

                let ir::TyKind::Tuple(fields) = &self.get_ty_mat(&lookup.base.ty).kind else {
                    panic!("invalid program");
                };

                // The target might be packed. Performing a tuple lookup of such target must
                // change the repr into normal SQL repr.
                // Currently this only happens for arrays, which change from JSON to SQL repr.
                let unpack = expr.ty.kind.is_array();

                // In simple case start == lookup.position. But this tuple might contains fields
                // before lookup.position which are represented with multiple columns.
                // So we must iterate over all preceding fields and count how many columns do they
                // produce.
                let start: usize = fields
                    .iter()
                    .take(position)
                    .map(|f| self.rel_cols_ty_nested(&f.ty).count())
                    .sum();

                // Also, target of lookup might not contain a single column
                // (e.g. it might be a nested tuple with two fields).
                let end = start
                    + if unpack {
                        1
                    } else {
                        self.rel_cols_ty_nested(&fields[position].ty).count()
                    };

                let mut rel = cr::ExprKind::Transform(
                    self.new_binding(base),
                    cr::Transform::ProjectRetain((start..end).collect()),
                );

                if unpack {
                    rel = cr::ExprKind::From(cr::From::JsonUnpack(Box::new(cr::Expr {
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

                    let is_exactly_one_row = expr.rel.ty.kind.is_primitive()
                        || expr.rel.ty.kind.is_tuple()
                        || expr.rel.ty.kind.is_enum();
                    if is_exactly_one_row {
                        // if possible, use BindCorrelated, because it is easier for optimizers to work with
                        cr::ExprKind::BindCorrelated(expr, Box::new(main))
                    } else {
                        cr::ExprKind::Bind(expr, Box::new(main))
                    }
                }
            }

            ir::ExprKind::Switch(switch) => {
                let single_col = self.rel_cols(&expr.ty, true).nth(1).is_none();

                if single_col {
                    // CASE
                    // WHEN b0.condition THEN b0.value
                    // WHEN b1.condition THEN b1.value
                    //                   ELSE b2.value
                    let mut cases = Vec::with_capacity(switch.len());
                    for branch in switch {
                        let condition = self.compile_column(&branch.condition);
                        let value = self.compile_column(&branch.value);

                        cases.push((condition, value));
                    }
                    cr::ExprKind::From(cr::From::Case(cases))
                } else {
                    // WITH selector AS (CASE ...)
                    // b0.value WHERE selector = 0
                    // UNION ALL
                    // b1.value WHERE selector = 1
                    // UNION ALL
                    // b2.value WHERE selector = 2

                    fn new_branch_selector(index: usize) -> cr::Expr {
                        cr::Expr {
                            kind: cr::ExprKind::From(cr::From::Literal(ir::Literal::int16(
                                index as i16,
                            ))),
                            ty: ir::Ty::new(ir::TyPrimitive::int16),
                        }
                    }

                    let mut cases = Vec::with_capacity(switch.len());
                    for (index, branch) in switch.iter().enumerate() {
                        let condition = self.compile_column(&branch.condition);
                        let value = new_branch_selector(index);
                        cases.push((condition, value));
                    }
                    let selector = cr::Expr {
                        kind: cr::ExprKind::From(cr::From::Case(cases)),
                        ty: ir::Ty::new(ir::TyPrimitive::int16),
                    };
                    let selector = self.new_binding(selector);

                    let selector_ref = self.new_rel_col(&selector, 0, selector.rel.ty.clone());

                    let mut branches = Vec::with_capacity(switch.len());
                    for (index, branch) in switch.iter().enumerate() {
                        let condition = cr::Expr {
                            kind: cr::ExprKind::From(cr::From::FuncCall(
                                "std::eq".into(),
                                vec![selector_ref.clone(), new_branch_selector(index)],
                            )),
                            ty: ir::Ty::new(ir::TyPrimitive::bool),
                        };

                        let value = self.compile_rel(&branch.value);
                        let value = self.new_binding(value);

                        branches.push(cr::Expr::new_iso_transform(
                            value,
                            cr::Transform::Where(Box::new(condition)),
                        ));
                    }
                    let union = cr::Expr {
                        kind: cr::ExprKind::Union(branches),
                        ty: expr.ty.clone(),
                    };
                    cr::ExprKind::Bind(selector, Box::new(union))
                }
            }
        };
        cr::Expr {
            kind,
            ty: expr.ty.clone(),
        }
    }

    fn row_or_join(&mut self, mut row: Vec<cr::Expr>, expr: ColumnsOrUnpack) -> cr::ExprKind {
        match expr {
            ColumnsOrUnpack::Columns(cols) => {
                row.extend(cols);
                cr::ExprKind::From(cr::From::Row(row))
            }
            ColumnsOrUnpack::Unpack(e) => cr::ExprKind::Join(
                self.new_binding(cr::Expr {
                    kind: cr::ExprKind::From(cr::From::Row(row)),
                    ty: ir::Ty::new(ir::TyPrimitive::int64),
                }),
                self.new_binding(e),
                None,
            ),
        }
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
                    "greatest",
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
                    new_bin_op(length, "greatest", new_int(0), ir::TyPrimitive::int64);

                let rel_offset = cr::Expr::new_iso_transform(
                    self.new_binding(array),
                    cr::Transform::Offset(Box::new(start_clamped)),
                );

                cr::ExprKind::Transform(
                    self.new_binding(rel_offset),
                    cr::Transform::Limit(Box::new(length_clamped)),
                )
            }
            "std::index" => {
                let array = self.compile_rel(&call.args[0]);
                let index = self.compile_column(&call.args[1]);

                let rel_offset = cr::Expr::new_iso_transform(
                    self.new_binding(array),
                    cr::Transform::Offset(Box::new(index)),
                );

                let rel_limit = cr::Expr::new_iso_transform(
                    self.new_binding(rel_offset),
                    cr::Transform::Limit(Box::new(new_int(1))),
                );

                // drop index column
                let mut rel = cr::ExprKind::Transform(
                    self.new_binding(rel_limit),
                    cr::Transform::ProjectDiscard(vec![0]),
                );

                if expr.ty.kind.is_array() {
                    rel = cr::ExprKind::From(cr::From::JsonUnpack(Box::new(cr::Expr {
                        kind: rel,
                        ty: ir::Ty::new(ir::TyPrimitive::text),
                    })));
                }

                rel
            }
            "std::map" => {
                let array = self.compile_rel(&call.args[0]);
                let array = self.new_binding(array);
                let item_ty = array.rel.ty.kind.as_array().unwrap().clone();
                let func = &call.args[1];

                // index
                let row = vec![self.new_rel_col(&array, 0, ir::Ty::new(ir::TyPrimitive::int64))];

                // compile func body
                let func = func.kind.as_function().unwrap();
                let mut item_ref = cr::ExprKind::Transform(
                    self.new_binding(cr::Expr::new_rel_ref(&array)),
                    cr::Transform::ProjectDiscard(vec![0]), // discard index
                );

                if item_ty.kind.is_array() {
                    item_ref = cr::ExprKind::From(cr::From::JsonUnpack(Box::new(cr::Expr {
                        kind: item_ref,
                        ty: ir::Ty::new(ir::TyPrimitive::text),
                    })));
                }

                self.functions.insert(func.id, FuncProvider::Expr(item_ref));
                let mapped_item = self.compile_column_list(&func.body);
                self.functions.remove(&func.id);

                let item = self.row_or_join(row, mapped_item);

                cr::ExprKind::BindCorrelated(
                    array,
                    Box::new(cr::Expr {
                        kind: item,
                        ty: expr.ty.clone(),
                    }),
                )
            }
            "std::flat_map" => {
                let array = self.compile_rel(&call.args[0]);
                let array = self.new_binding(array);
                let item_ty = array.rel.ty.kind.as_array().unwrap().clone();
                let func = &call.args[1];

                let output_item_ty = *expr.ty.kind.clone().into_array().unwrap();

                // index
                let output_row =
                    vec![self.new_rel_col(&array, 0, ir::Ty::new(ir::TyPrimitive::int64))];

                // compile func body
                let func = func.kind.as_function().unwrap();
                let mut item_ref = cr::ExprKind::Transform(
                    self.new_binding(cr::Expr::new_rel_ref(&array)),
                    cr::Transform::ProjectDiscard(vec![0]), // discard index
                );

                if item_ty.kind.is_array() {
                    item_ref = cr::ExprKind::From(cr::From::JsonUnpack(Box::new(cr::Expr {
                        kind: item_ref,
                        ty: ir::Ty::new(ir::TyPrimitive::text),
                    })));
                }

                self.functions.insert(func.id, FuncProvider::Expr(item_ref));
                let mapped_items = self.compile_rel(&func.body);
                self.functions.remove(&func.id);

                let mapped_items = self.new_binding(mapped_items);

                let mapped_items_ref = self.new_binding(cr::Expr::new_rel_ref(&mapped_items));
                // remove unneeded index of mapped_items
                let mapped_items_ref = self.to_column_list(cr::Expr {
                    kind: cr::ExprKind::Transform(
                        mapped_items_ref,
                        cr::Transform::ProjectDiscard(vec![0]),
                    ),
                    ty: output_item_ty,
                });

                cr::ExprKind::BindCorrelated(
                    array,
                    Box::new(cr::Expr {
                        kind: cr::ExprKind::BindCorrelated(
                            mapped_items,
                            Box::new(cr::Expr {
                                kind: self.row_or_join(output_row, mapped_items_ref),
                                ty: expr.ty.clone(),
                            }),
                        ),
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
                    self.new_binding(cr::Expr::new_rel_ref(&array)),
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
                    self.new_binding(cr::Expr::new_rel_ref(&array)),
                    cr::Transform::ProjectDiscard(vec![0]), // discard index
                );

                self.functions.insert(func.id, FuncProvider::Expr(item_ref));
                let key = self.compile_column(&func.body);
                self.functions.remove(&func.id);

                cr::ExprKind::Transform(array, cr::Transform::IndexBy(Some(Box::new(key))))
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

                let field0 = cr::Expr::new_json_pack(cr::Expr {
                    kind: cr::ExprKind::Transform(
                        self.new_binding(cr::Expr::new_rel_ref(&array)),
                        cr::Transform::ProjectRetain(vec![
                            0, // index
                            1, // first tuple field
                        ]),
                    ),
                    ty: ty_out_fields[0].ty.clone(),
                });
                let field1 = cr::Expr::new_json_pack(cr::Expr {
                    kind: cr::ExprKind::Transform(
                        self.new_binding(cr::Expr::new_rel_ref(&array)),
                        cr::Transform::ProjectRetain(vec![
                            0, // index
                            2, // second tuple field
                        ]),
                    ),
                    ty: ty_out_fields[0].ty.clone(),
                });

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
                    cr::Expr {
                        ty: ty_in_fields[0].ty.clone(),
                        kind: cr::ExprKind::From(cr::From::JsonUnpack(Box::new(input))),
                    }
                };
                let field0 = self.new_binding(field0);
                let field1 = {
                    let input = self.new_rel_col(&tuple, 1, ir::Ty::new(ir::TyPrimitive::text));
                    cr::Expr {
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

            "std::group" => {
                let array = self.compile_rel(&call.args[0]);
                let array = self.new_binding(array);

                let func = &call.args[1];
                let func = func.kind.as_function().unwrap();

                let item_ref = cr::ExprKind::Transform(
                    self.new_binding(cr::Expr::new_rel_ref(&array)),
                    cr::Transform::ProjectDiscard(vec![0]), // discard index
                );

                self.functions
                    .insert(func.id, FuncProvider::Expr(item_ref.clone()));
                let key = self.compile_column_list(&func.body).unwrap_columns();
                self.functions.remove(&func.id);

                let mut values = key.clone();
                values.push(cr::Expr::new_json_pack(cr::Expr::new_rel_ref(&array)));

                cr::ExprKind::Transform(array, cr::Transform::Group(key, values))
            }

            "std::append" => {
                // compute each rel and apply order to it
                let first = self.compile_rel(&call.args[0]);
                let first =
                    cr::Expr::new_iso_transform(self.new_binding(first), cr::Transform::Order);

                let second = self.compile_rel(&call.args[1]);
                let second =
                    cr::Expr::new_iso_transform(self.new_binding(second), cr::Transform::Order);

                // union
                let union = self.new_binding(cr::Expr {
                    kind: cr::ExprKind::Union(vec![first, second]),
                    ty: expr.ty.clone(),
                });

                // reindex
                cr::ExprKind::Transform(union, cr::Transform::IndexBy(None))
            }

            "std::sql::from" => {
                let table_name = &call.args[0];
                let ir::ExprKind::Literal(ir::Literal::text(table_name)) = &table_name.kind else {
                    panic!("TODO: std::sql::from expects a literal table name, not an expression")
                };

                cr::ExprKind::From(cr::From::Table(table_name.clone()))
            }
            "std::sql::insert" => {
                let rows = self.compile_rel(&call.args[0]);
                let rows = self.new_binding(rows);

                let table_name = &call.args[1];
                let ir::ExprKind::Literal(ir::Literal::text(table_name)) = &table_name.kind else {
                    panic!("TODO: std::sql::from expects a literal table name, not an expression")
                };

                cr::ExprKind::Transform(rows, cr::Transform::Insert(table_name.clone()))
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
    #[track_caller]
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

        self.to_column_list(rel)
    }

    fn to_column_list(&self, rel: cr::Expr) -> ColumnsOrUnpack {
        let ty_mat = self.get_ty_mat(&rel.ty);

        match rel.kind {
            // it is just one constructed row: unwrap
            cr::ExprKind::From(cr::From::Row(row)) if !ty_mat.kind.is_array() => {
                ColumnsOrUnpack::Columns(row)
            }

            // it is actually complex: subquery
            _ => {
                match &ty_mat.kind {
                    ir::TyKind::Primitive(_) => {
                        // return as a subquery
                        ColumnsOrUnpack::Columns(vec![rel])
                    }
                    ir::TyKind::Array(_) => {
                        // arrays need to packed to JSON
                        ColumnsOrUnpack::Columns(vec![cr::Expr {
                            ty: ir::Ty::new(ir::TyPrimitive::text),
                            kind: cr::ExprKind::From(cr::From::JsonPack(Box::new(rel))),
                        }])
                    }
                    ir::TyKind::Tuple(_) => {
                        // non-constructed tuples need to be declared as a rel var and unpacked

                        ColumnsOrUnpack::Unpack(rel)
                    }
                    ir::TyKind::Enum(_) => {
                        let is_single_col = self.rel_cols_ty_nested(ty_mat).nth(1).is_none();
                        if is_single_col {
                            ColumnsOrUnpack::Columns(vec![rel])
                        } else {
                            ColumnsOrUnpack::Unpack(rel)
                        }
                    }
                    ir::TyKind::Function(_) | ir::TyKind::Ident(_) => unreachable!(),
                }
            }
        }
    }

    /// Compiles `ir::Call { function: Pointer(External), args }` to cr::From(FuncCall { .. })
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
    let kind = cr::ExprKind::From(cr::From::Literal(ir::Literal::int64(int)));
    cr::Expr {
        kind,
        ty: ir::Ty::new(ir::TyPrimitive::int64),
    }
}

fn new_int8(int: i8) -> cr::Expr {
    let kind = cr::ExprKind::From(cr::From::Literal(ir::Literal::int8(int)));
    cr::Expr {
        kind,
        ty: ir::Ty::new(ir::TyPrimitive::int8),
    }
}

fn ty_concat_tuples(a: ir::Ty, b: ir::Ty) -> ir::Ty {
    let a = a.kind.into_tuple().unwrap();
    let b = b.kind.into_tuple().unwrap();
    let mut concat = a;
    concat.extend(b);
    ir::Ty::new(ir::TyKind::Tuple(concat))
}

/// Wraps an expr of func type into a call to this function
///
/// Example: p: func(a, b) -> c` is wrapped into `func (x: a, y: b) -> p(a, b)`
fn wrap_into_func_call(expr: &ir::Expr) -> ir::Function {
    let ty_function = expr.ty.kind.as_function().unwrap();

    ir::Function {
        id: u32::MAX,
        body: ir::Expr {
            ty: ty_function.body.clone(),
            kind: ir::ExprKind::Call(Box::new(ir::Call {
                function: expr.clone(),
                args: ty_function
                    .params
                    .iter()
                    .enumerate()
                    .map(|(position, ty_p)| ir::Expr {
                        kind: ir::ExprKind::Pointer(ir::Pointer::Parameter(ir::ParameterPtr {
                            function_id: u32::MAX,
                            param_position: position as u8,
                        })),
                        ty: ty_p.clone(),
                    })
                    .collect(),
            })),
        },
    }
}
