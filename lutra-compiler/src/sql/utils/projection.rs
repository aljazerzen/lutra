use lutra_bin::ir;
use sqlparser::ast as sql_ast;

use crate::sql::queries::Context;
use crate::sql::{COL_ARRAY_INDEX, COL_VALUE};

use super::ExprOrSource;

impl<'a> Context<'a> {
    /// Generates the projection of a relation of type `ty`.
    pub fn projection(
        &self,
        ty: &ir::Ty,
        values: impl IntoIterator<Item = ExprOrSource>,
    ) -> Vec<sql_ast::SelectItem> {
        itertools::zip_eq(values, self.rel_cols(ty, true))
            .map(|(expr, alias)| sql_ast::SelectItem::ExprWithAlias {
                expr: expr.into_expr(),
                alias: sql_ast::Ident::new(alias),
            })
            .collect()
    }

    /// Generates an identity (no-op) projection of a relation of type `ty`, from relation variable `rel_var_name`.
    /// When `top_level` is set, the projection does not include array index.
    pub fn projection_noop(
        &self,
        rel_var_name: Option<&str>,
        ty: &ir::Ty,
        include_index: bool,
    ) -> Vec<sql_ast::SelectItem> {
        self.rel_cols(ty, include_index)
            .map(|name| sql_ast::SelectItem::UnnamedExpr(super::ident(rel_var_name, name)))
            .collect()
    }

    /// Names of relational columns for a given type
    /// If include_index is false, top-level arrays does not produce index column.
    pub fn rel_cols(
        &'a self,
        ty: &'a ir::Ty,
        include_index: bool,
    ) -> Box<dyn Iterator<Item = String> + 'a> {
        let ty_mat = self.get_ty_mat(ty);
        match &ty_mat.kind {
            ir::TyKind::Primitive(_) => Box::new(Some(COL_VALUE.to_string()).into_iter()),
            ir::TyKind::Array(item) => {
                let mut index = None;
                if include_index {
                    index = Some(COL_ARRAY_INDEX.to_string());
                }

                Box::new(
                    index
                        .into_iter()
                        .chain(self.rel_cols_re(item, "".to_string())),
                )
            }

            ir::TyKind::Tuple(_) | ir::TyKind::Enum(_) => self.rel_cols_re(ty_mat, "".to_string()),

            ir::TyKind::Function(_) => todo!(),
            ir::TyKind::Ident(_) => todo!(),
        }
    }

    fn rel_cols_re(
        &'a self,
        ty: &'a ir::Ty,
        name_prefix: String,
    ) -> Box<dyn Iterator<Item = String> + 'a> {
        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Primitive(_) | ir::TyKind::Array(_) => {
                let name = if name_prefix.is_empty() {
                    COL_VALUE.to_string()
                } else {
                    name_prefix
                };
                Box::new(Some(name).into_iter())
            }

            ir::TyKind::Tuple(fields) => {
                Box::new(fields.iter().enumerate().flat_map(move |(i, field)| {
                    let prefix = format!("{name_prefix}_{i}");
                    self.rel_cols_re(&field.ty, prefix)
                }))
            }

            ir::TyKind::Enum(variants) => Box::new(
                Some(
                    format!("{name_prefix}_t"), // tag
                )
                .into_iter()
                .chain(
                    variants // non-unit fields
                        .iter()
                        .enumerate()
                        .flat_map(move |(i, variant)| {
                            let name_prefix = format!("{name_prefix}_{i}");
                            self.rel_cols_re(&variant.ty, name_prefix)
                        }),
                ),
            ),
            ir::TyKind::Function(_) => todo!(),
            ir::TyKind::Ident(_) => todo!(),
        }
    }
}

#[cfg(test)]
mod rel_repr {
    use itertools::Itertools;
    use lutra_bin::ir;

    /// return list of relation columns for a given type
    fn r(ty: &ir::Ty) -> String {
        let ctx = super::Context::new(Default::default());
        let x = ctx.rel_cols(ty, true).join(", ");
        x
    }

    #[test]
    fn rel_cols_brute_force() {
        // test column names of relations that represent certain types

        // depth 1 (no nested types)
        assert_eq!(r(&prim()), "value");
        assert_eq!(r(&tuple_0()), "");
        assert_eq!(r(&enum_(vec![])), "_t");

        // depth 2
        assert_eq!(r(&tuple_1(prim())), "_0");
        assert_eq!(r(&tuple_1(tuple_0())), "");
        assert_eq!(r(&array(prim())), "index, value");
        assert_eq!(r(&array(tuple_0())), "index");
        assert_eq!(r(&enum_(vec![tuple_0(), tuple_0(), tuple_0()])), "_t");
        assert_eq!(r(&enum_(vec![tuple_0(), prim()])), "_t, _1");

        // depth 3
        assert_eq!(r(&tuple_1(tuple_1(prim()))), "_0_0");
        assert_eq!(r(&tuple_1(tuple_1(tuple_0()))), "");
        assert_eq!(r(&tuple_1(array(prim()))), "_0");
        assert_eq!(r(&tuple_1(array(tuple_0()))), "_0");
        assert_eq!(r(&tuple_1(enum_(vec![prim(), prim()]))), "_0_t, _0_0, _0_1");
        assert_eq!(r(&tuple_1(enum_(vec![tuple_0(), tuple_0()]))), "_0_t");

        assert_eq!(r(&array(tuple_1(prim()))), "index, _0");
        assert_eq!(r(&array(tuple_1(tuple_0()))), "index");
        assert_eq!(r(&array(array(prim()))), "index, value");
        assert_eq!(r(&array(array(tuple_0()))), "index, value");
        assert_eq!(r(&array(enum_1(prim()))), "index, _t, _0");
        assert_eq!(r(&array(enum_1(tuple_0()))), "index, _t");

        assert_eq!(r(&enum_1(tuple_1(prim()))), "_t, _0_0");
        assert_eq!(r(&enum_1(tuple_1(tuple_0()))), "_t");
        assert_eq!(r(&enum_1(array(prim()))), "_t, _0");
        assert_eq!(r(&enum_1(array(tuple_0()))), "_t, _0");
        assert_eq!(r(&enum_1(enum_1(prim()))), "_t, _0_t, _0_0");
        assert_eq!(r(&enum_1(enum_1(tuple_0()))), "_t, _0_t");
    }

    #[test]
    fn rel_cols_special() {
        assert_eq!(
            r(&tuple_3(prim(), tuple_2(prim(), tuple_0()), prim())),
            "_0, _1_0, _2"
        );
        assert_eq!(
            r(&tuple_3(
                tuple_2(prim(), tuple_0()),
                tuple_0(),
                tuple_2(tuple_0(), tuple_0())
            )),
            "_0_0"
        );
        assert_eq!(r(&tuple_3(prim(), array(prim()), prim())), "_0, _1, _2");

        assert_eq!(r(&tuple_3(prim(), tuple_0(), prim())), "_0, _2");

        assert_eq!(
            r(&enum_(vec![
                enum_(vec![prim(), prim()]),
                tuple_2(prim(), prim()),
                tuple_0(),
                tuple_1(prim()),
            ])),
            "_t, _0_t, _0_0, _0_1, _1_0, _1_1, _3_0"
        );
    }

    // helpers

    fn prim() -> ir::Ty {
        ir::Ty::new(ir::TyPrimitive::int32)
    }

    fn tuple_0() -> ir::Ty {
        ir::Ty::new(vec![])
    }

    fn tuple_1(a: ir::Ty) -> ir::Ty {
        ir::Ty::new(vec![ir::TyTupleField { name: None, ty: a }])
    }

    fn tuple_2(a: ir::Ty, b: ir::Ty) -> ir::Ty {
        ir::Ty::new(vec![
            ir::TyTupleField { name: None, ty: a },
            ir::TyTupleField { name: None, ty: b },
        ])
    }

    fn tuple_3(a: ir::Ty, b: ir::Ty, c: ir::Ty) -> ir::Ty {
        ir::Ty::new(vec![
            ir::TyTupleField { name: None, ty: a },
            ir::TyTupleField { name: None, ty: b },
            ir::TyTupleField { name: None, ty: c },
        ])
    }

    fn array(item: ir::Ty) -> ir::Ty {
        ir::Ty::new(ir::TyKind::Array(Box::new(item)))
    }

    fn enum_1(variant: ir::Ty) -> ir::Ty {
        enum_(vec![variant])
    }

    fn enum_(variants: Vec<ir::Ty>) -> ir::Ty {
        ir::Ty::new(ir::TyKind::Enum(
            variants
                .into_iter()
                .enumerate()
                .map(|(tag, ty)| ir::TyEnumVariant {
                    name: format!("variant{tag}"),
                    ty,
                })
                .collect_vec(),
        ))
    }
}
