use lutra_bin::ir;

use crate::Result;
use crate::pr;
use crate::resolver::NS_STD;

use super::Lowerer;
use crate::intermediate::ir_utils::*;

impl<'a> Lowerer<'a> {
    /// Determines if std::ops::cmp needs an expansion, or if it can use External(std::ops::cmp).
    pub(super) fn std_cmp_expands(&self, ty: &'a pr::Ty) -> bool {
        let (ty, frame_name) = self.get_ty_mat_pr(ty);
        frame_name.is_none()
            && matches!(
                &ty.kind,
                pr::TyKind::Tuple(_) | pr::TyKind::Array(_) | pr::TyKind::Enum(_)
            )
    }

    pub(super) fn impl_std_cmp(&mut self, ty: pr::Ty) -> Result<ir::Expr> {
        let ir_ty = self.lower_ty(&ty);

        let function_id = self.generator_function_scope.next() as u32;
        let left = new_param(function_id, 0, ir_ty.clone());
        let right = new_param(function_id, 1, ir_ty.clone());
        let body = self.construct_std_cmp_body(&ty, left, right)?;

        Ok(ir::Expr::new(
            ir::Function {
                id: function_id,
                body,
            },
            ir::Ty::new(ir::TyFunction {
                params: vec![ir_ty.clone(), ir_ty],
                body: ty_ordering(),
            }),
        ))
    }

    fn construct_std_cmp_body(
        &mut self,
        ty: &pr::Ty,
        left: ir::Expr,
        right: ir::Expr,
    ) -> Result<ir::Expr> {
        let (ty, _) = self.get_ty_mat_pr(ty);
        match &ty.kind {
            pr::TyKind::Tuple(fields) => self.expand_cmp_tuple(fields, left, right),
            pr::TyKind::Array(item_ty) => self.expand_cmp_array(item_ty, left, right),
            pr::TyKind::Enum(variants) => self.expand_cmp_enum(variants, left, right),
            pr::TyKind::Primitive(_) | pr::TyKind::Ident(_) | pr::TyKind::Func(_) => {
                unreachable!()
            }
            pr::TyKind::Option(_) | pr::TyKind::TupleComprehension(_) => unreachable!(),
        }
    }

    fn expand_cmp_tuple(
        &mut self,
        fields: &[pr::TyTupleField],
        left: ir::Expr,
        right: ir::Expr,
    ) -> Result<ir::Expr> {
        // Lexicographic: compare field 0; if equal, compare field 1; etc.
        // Built right-to-left, starting from `.equal`.
        let mut result = new_ordering_equal();
        for (index, field) in fields.iter().enumerate().rev() {
            let field_ir_ty = self.lower_ty(&field.ty);
            let left_field = ir::Expr::new(
                ir::TupleLookup::new(left.clone(), index as u16),
                field_ir_ty.clone(),
            );
            let right_field = ir::Expr::new(
                ir::TupleLookup::new(right.clone(), index as u16),
                field_ir_ty,
            );
            let field_cmp = self.call_std_cmp(&field.ty, left_field, right_field)?;
            result = self.chain_on_equal(field_cmp, result);
        }
        Ok(result)
    }

    fn expand_cmp_array(
        &mut self,
        item_ty: &pr::Ty,
        left: ir::Expr,
        right: ir::Expr,
    ) -> Result<ir::Expr> {
        // Lexicographic on the common prefix; if that prefix is equal, fall
        // back to comparing array lengths.
        // 1. pairs = zip(left, right)                  -- truncates to shorter
        // 2. elem_orderings = map(pairs, |p| cmp(p.0, p.1))
        // 3. prefix_ord = fold(elem_orderings, .equal,
        //      |acc, x| if acc == .equal then x else acc)
        // 4. len_ord = cmp(count(left), count(right))
        // 5. result = if prefix_ord == .equal then len_ord else prefix_ord
        let ordering_ty = ty_ordering();
        let item_ir_ty = self.lower_ty(item_ty);

        let pair_ty = ir::Ty::new(ir::TyKind::Tuple(vec![
            ir::TyTupleField {
                name: None,
                ty: item_ir_ty.clone(),
            },
            ir::TyTupleField {
                name: None,
                ty: item_ir_ty.clone(),
            },
        ]));

        // mapper: func ({T, T}) -> Ordering
        let mapper = {
            let mapper_id = self.generator_function_scope.next() as u32;
            let pair = new_param(mapper_id, 0, pair_ty.clone());
            let body = {
                let left = ir::Expr::new(ir::TupleLookup::new(pair.clone(), 0), item_ir_ty.clone());
                let right = ir::Expr::new(ir::TupleLookup::new(pair, 1), item_ir_ty.clone());
                self.call_std_cmp(item_ty, left, right)?
            };
            ir::Expr::new(
                ir::Function {
                    id: mapper_id,
                    body,
                },
                ir::Ty::new(ir::TyFunction {
                    params: vec![pair_ty.clone()],
                    body: ordering_ty.clone(),
                }),
            )
        };

        // zip + map
        let zipped_ty = ir::Ty::new(ir::TyKind::Array(Box::new(pair_ty)));
        let zipped = new_call_bin("std::array::zip", left.clone(), right.clone(), zipped_ty);
        let orderings_ty = ir::Ty::new(ir::TyKind::Array(Box::new(ordering_ty.clone())));
        let orderings = new_call_bin("std::array::map", zipped, mapper, orderings_ty.clone());

        // fold operation: func (Ordering, Ordering) -> Ordering
        //   |acc, x| if acc == .equal then x else acc
        let fold_op = {
            let op_id = self.generator_function_scope.next() as u32;
            let acc = new_param(op_id, 0, ordering_ty.clone());
            let x = new_param(op_id, 1, ordering_ty.clone());
            let body = self.switch_if_equal(acc, x);
            ir::Expr::new(
                ir::Function { id: op_id, body },
                ir::Ty::new(ir::TyFunction {
                    params: vec![ordering_ty.clone(), ordering_ty.clone()],
                    body: ordering_ty.clone(),
                }),
            )
        };

        // fold call: std::array::fold(orderings, .equal, fold_op): Ordering
        let prefix_ord = {
            let initial = new_ordering_equal();
            new_call_tri("std::array::fold", orderings, initial, fold_op, ordering_ty)
        };

        // length comparison
        let int64_ty = ir::Ty::new_ident(&[NS_STD, "Int64"]);
        let len_left = new_call_un("std::array::count", left, int64_ty.clone());
        let len_right = new_call_un("std::array::count", right, int64_ty);
        let len_ord = new_call_bin("std::ops::cmp", len_left, len_right, ty_ordering());

        Ok(self.chain_on_equal(prefix_ord, len_ord))
    }

    fn expand_cmp_enum(
        &mut self,
        variants: &[pr::TyEnumVariant],
        left: ir::Expr,
        right: ir::Expr,
    ) -> Result<ir::Expr> {
        // Compare tags first; if tags are equal, compare the variant's payload.
        let Some(left_tag) = self.new_enum_tag(left.clone()) else {
            assert_eq!(variants.len(), 1);
            let variant = &variants[0];
            if self.is_ty_unit_pr(&variant.ty) {
                return Ok(new_ordering_equal());
            }
            return self.enum_inner_cmp(&variant.ty, &left, &right, 0);
        };
        let Some(right_tag) = self.new_enum_tag(right.clone()) else {
            unreachable!("left and right have the same enum type")
        };

        let tag_cmp = new_call_bin("std::ops::cmp", left_tag.clone(), right_tag, ty_ordering());

        // inner comparison: switch on tag
        let mut branches = Vec::with_capacity(variants.len() + 1);
        for (tag, variant) in variants.iter().enumerate() {
            if self.is_ty_unit_pr(&variant.ty) {
                continue;
            }

            let tag_lit = self.new_prim(tag, left_tag.ty.clone());
            let condition = new_call_bin_bool("std::ops::eq", left_tag.clone(), tag_lit);
            let value = self.enum_inner_cmp(&variant.ty, &left, &right, tag)?;
            branches.push(ir::SwitchBranch { condition, value });
        }
        branches.push(ir::SwitchBranch {
            condition: ir::Expr::new_lit_bool(true),
            value: new_ordering_equal(),
        });
        let inner_cmp = ir::Expr::new(ir::ExprKind::Switch(branches), ty_ordering());

        Ok(self.chain_on_equal(tag_cmp, inner_cmp))
    }

    fn call_std_cmp(&mut self, ty: &pr::Ty, left: ir::Expr, right: ir::Expr) -> Result<ir::Expr> {
        debug_assert_eq!(left.ty, right.ty);
        let function = self.lower_std_cmp_ref(ty, left.ty.clone())?;
        Ok(ir::Expr::new(
            ir::Call {
                function,
                args: vec![left, right],
            },
            ty_ordering(),
        ))
    }

    fn lower_std_cmp_ref(&mut self, ty: &pr::Ty, ir_ty: ir::Ty) -> Result<ir::Expr> {
        let kind = self.lower_ref_global(
            &pr::Path::new([NS_STD, "ops", "cmp"]),
            std::slice::from_ref(ty),
        )?;
        Ok(ir::Expr::new(
            kind,
            ir::Ty::new(ir::TyFunction {
                params: vec![ir_ty.clone(), ir_ty],
                body: ty_ordering(),
            }),
        ))
    }

    fn enum_inner_cmp(
        &mut self,
        ty: &pr::Ty,
        left: &ir::Expr,
        right: &ir::Expr,
        tag: usize,
    ) -> Result<ir::Expr> {
        let ir_ty = self.lower_ty(ty);
        let left = ir::Expr::new(
            ir::EnumUnwrap {
                subject: left.clone(),
                tag: tag as u64,
            },
            ir_ty.clone(),
        );
        let right = ir::Expr::new(
            ir::EnumUnwrap {
                subject: right.clone(),
                tag: tag as u64,
            },
            ir_ty,
        );
        self.call_std_cmp(ty, left, right)
    }

    /// Builds: `if cmp_result == .equal then on_equal else cmp_result`.
    ///
    /// `cmp_result` is bound to a fresh binding so it can be referenced both
    /// by the condition (via `enum_tag`) and as the fallback value, without
    /// being evaluated twice.
    fn chain_on_equal(&mut self, cmp_result: ir::Expr, on_equal: ir::Expr) -> ir::Expr {
        let ordering_ty = cmp_result.ty.clone();
        let binding_id = self.generator_var_binding.next() as u32;
        let result_ref = ir::Expr::new(
            ir::ExprKind::Pointer(ir::Pointer::Binding(binding_id)),
            ordering_ty.clone(),
        );

        let switch = self.switch_if_equal(result_ref, on_equal);

        ir::Expr::new(
            ir::Binding {
                id: binding_id,
                expr: cmp_result,
                main: switch,
            },
            ordering_ty,
        )
    }

    /// Builds: `if ordering == .equal then on_equal else ordering`.
    ///
    /// `ordering` must not have side effects, as it is referenced twice.
    fn switch_if_equal(&mut self, ordering: ir::Expr, on_equal: ir::Expr) -> ir::Expr {
        let ordering_ty = ordering.ty.clone();
        let tag = self
            .new_enum_tag(ordering.clone())
            .expect("Ordering has 3 variants and therefore has a tag");
        let equal_tag_lit = self.new_prim(1, tag.ty.clone());
        let condition = new_call_bin_bool("std::ops::eq", tag, equal_tag_lit);

        ir::Expr::new(
            ir::ExprKind::Switch(vec![
                ir::SwitchBranch {
                    condition,
                    value: on_equal,
                },
                ir::SwitchBranch {
                    condition: ir::Expr::new_lit_bool(true),
                    value: ordering,
                },
            ]),
            ordering_ty,
        )
    }
}

/// Constructs a literal `std::ops::Ordering.equal`.
fn new_ordering_equal() -> ir::Expr {
    ir::Expr::new(
        ir::ExprKind::EnumVariant(Box::new(ir::EnumVariant {
            tag: 1,
            inner: ir::Expr::new(ir::ExprKind::Tuple(vec![]), ir::Ty::new_unit()),
        })),
        ty_ordering(),
    )
}

fn ty_ordering() -> ir::Ty {
    ir::Ty::new_ident(&["std", "ops", "Ordering"])
}
