use std::borrow::Cow;

use lutra_bin::ir;

use crate::sql::{COL_ARRAY_INDEX, COL_VALUE};
use crate::sql::{clauses, queries, utils};

/// Compute relational columns of a type in "query representation".
/// Basically, mapping of IR types to SQL relations we use in generated queries.
///
/// This is a trait, so it can work for both [clauses] and [queries] stage.
pub trait RelCols<'a> {
    fn get_ty_mat(&self, ty: &'a ir::Ty) -> &'a ir::Ty;

    /// Names of relational columns for a given type.
    /// If include_index is false, top-level arrays does not produce index column.
    fn rel_cols(
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
                        .chain(self.rel_cols_nested(item, "".to_string())),
                )
            }

            ir::TyKind::Tuple(_) | ir::TyKind::Enum(_) => {
                self.rel_cols_nested(ty_mat, "".to_string())
            }

            ir::TyKind::Function(_) => todo!(),
            ir::TyKind::Ident(_) => todo!(),
        }
    }

    /// Names of relational columns for a given type, where this type is not the top-level
    /// type of a relation.
    fn rel_cols_nested(
        &'a self,
        ty: &'a ir::Ty,
        name_prefix: String,
    ) -> Box<dyn Iterator<Item = String> + 'a> {
        let ty_mat = self.get_ty_mat(ty);
        match &ty_mat.kind {
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
                    self.rel_cols_nested(&field.ty, prefix)
                }))
            }

            ir::TyKind::Enum(variants) if utils::is_option(variants) => {
                let name = if name_prefix.is_empty() {
                    COL_VALUE.to_string()
                } else {
                    name_prefix
                };
                Box::new(Some(name).into_iter())
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
                            let is_recursive =
                                lutra_bin::layout::does_enum_variant_contain_recursive(
                                    ty_mat, i as u16,
                                );
                            if is_recursive {
                                // recursive variants are serialized
                                Box::new(Some(format!("{name_prefix}_{i}")).into_iter())
                            } else {
                                // recurse
                                let name_prefix = format!("{name_prefix}_{i}");
                                self.rel_cols_nested(&variant.ty, name_prefix)
                            }
                        }),
                ),
            ),
            ir::TyKind::Function(_) => todo!(),
            ir::TyKind::Ident(_) => todo!(),
        }
    }

    /// Types of relational columns for a given type.
    fn rel_cols_ty_nested(
        &'a self,
        ty: &'a ir::Ty,
    ) -> Box<dyn Iterator<Item = Cow<'a, ir::Ty>> + 'a> {
        let ty_mat = self.get_ty_mat(ty);
        match &ty_mat.kind {
            ir::TyKind::Primitive(_) | ir::TyKind::Array(_) => {
                Box::new(Some(Cow::Borrowed(ty_mat)).into_iter())
            }

            ir::TyKind::Tuple(fields) => {
                Box::new(fields.iter().flat_map(|f| self.rel_cols_ty_nested(&f.ty)))
            }

            ir::TyKind::Enum(variants) if utils::is_option(variants) => {
                Box::new(variants.iter().flat_map(|v| self.rel_cols_ty_nested(&v.ty)))
            }

            ir::TyKind::Enum(variants) => Box::new(itertools::chain(
                // tag
                Some(Cow::Owned(ir::Ty::new(ir::TyPrimitive::int8))),
                // variants
                variants.iter().enumerate().flat_map(|(i, v)| {
                    let is_recursive =
                        lutra_bin::layout::does_enum_variant_contain_recursive(ty_mat, i as u16);
                    if is_recursive {
                        Box::new(Some(Cow::Borrowed(&v.ty)).into_iter())
                    } else {
                        self.rel_cols_ty_nested(&v.ty)
                    }
                }),
            )),
            ir::TyKind::Function(_) => todo!(),
            ir::TyKind::Ident(_) => todo!(),
        }
    }
}

impl<'a> RelCols<'a> for queries::Context<'a> {
    fn get_ty_mat(&self, ty: &'a ir::Ty) -> &'a ir::Ty {
        queries::Context::get_ty_mat(self, ty)
    }
}

impl<'a> queries::Context<'a> {
    /// Generates the projection of a relation of type `ty`.
    #[track_caller]
    pub fn projection(
        &self,
        ty: &ir::Ty,
        values: impl IntoIterator<Item = sql_ast::Expr>,
    ) -> Vec<sql_ast::SelectItem> {
        let rel_cols = self.rel_cols(ty, true);

        let values = values.into_iter();

        // asserts eq values and rel cols
        #[cfg(debug_assertions)]
        let (values, rel_cols) = if std::env::var("OUT_DIR").is_err() {
            let rel_cols: Vec<_> = rel_cols.collect();
            let values: Vec<_> = values.collect();

            if values.len() != rel_cols.len() {
                tracing::error!("bad type detected in projection");
            }
            assert_eq!(
                rel_cols.len(),
                values.len(),
                "\n  expected columns: {rel_cols:?},\n  got: [\n{:#}\n  ],\n  ty: {}",
                sql_ast::Indent(sql_ast::Indent(sql_ast::DisplayCommaSeparated(&values))),
                ir::print_ty(ty)
            );
            (values, rel_cols)
        } else {
            (values.collect(), rel_cols.collect())
        };

        std::iter::zip(values, rel_cols)
            .map(|(expr, alias)| sql_ast::SelectItem {
                expr,
                alias: Some(utils::new_ident(alias)),
            })
            .collect()
    }

    /// Generates an identity (no-op) projection of a relation of type `ty`, from relation variable `rel_var`.
    /// When `top_level` is set, the projection does not include array index.
    pub fn projection_noop(
        &self,
        rel_var: Option<&str>,
        ty: &ir::Ty,
        include_index: bool,
    ) -> Vec<sql_ast::SelectItem> {
        self.rel_cols(ty, include_index)
            .map(|name| sql_ast::SelectItem {
                expr: super::identifier(rel_var, name),
                alias: None,
            })
            .collect()
    }
}

impl<'a> RelCols<'a> for clauses::Context<'a> {
    fn get_ty_mat(&self, ty: &'a ir::Ty) -> &'a ir::Ty {
        clauses::Context::get_ty_mat(self, ty)
    }
}

#[cfg(test)]
mod rel_repr {
    use itertools::Itertools;
    use lutra_bin::ir;

    use crate::sql::utils::RelCols;
    use crate::sql::{Dialect, queries};

    /// return list of relation columns for a given type
    fn r(ty: &ir::Ty) -> String {
        let ctx = queries::Context::new(Default::default(), Dialect::Postgres);

        ctx.rel_cols(ty, true).join(", ")
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
        assert_eq!(r(&enum_(vec![tuple_0(), prim()])), "value");

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
