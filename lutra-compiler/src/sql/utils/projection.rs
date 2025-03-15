use std::iter::zip;

use lutra_bin::ir;
use sqlparser::ast as sql_ast;

use crate::sql::queries::Context;
use crate::sql::{COL_ARRAY_INDEX, COL_VALUE};

impl<'a> Context<'a> {
    /// Generates the projection of a relation of type `ty`.
    pub fn projection(
        &self,
        ty: &ir::Ty,
        values: impl IntoIterator<Item = sql_ast::Expr>,
    ) -> Vec<sql_ast::SelectItem> {
        zip(values, self.rel_cols(ty, true))
            .map(|(expr, alias)| sql_ast::SelectItem::ExprWithAlias {
                expr,
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
    ) -> impl Iterator<Item = String> + 'a {
        self.rel_cols_re(ty, include_index, true, "f".to_string())
    }

    fn rel_cols_re(
        &'a self,
        ty: &'a ir::Ty,
        include_index: bool,
        top_level: bool,
        field_prefix: String,
    ) -> Box<dyn Iterator<Item = String> + 'a> {
        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Primitive(_) => Box::new(Some(COL_VALUE.to_string()).into_iter()),

            ir::TyKind::Tuple(fields) => {
                Box::new(fields.iter().enumerate().flat_map(move |(i, field)| {
                    match &self.get_ty_mat(&field.ty).kind {
                        ir::TyKind::Tuple(_) => {
                            let field_prefix = format!("{field_prefix}_{i}");
                            self.rel_cols_re(&field.ty, false, false, field_prefix)
                        }
                        ir::TyKind::Primitive(_) | ir::TyKind::Array(_) => {
                            Box::new(Some(format!("{field_prefix}_{i}")).into_iter())
                        }
                        _ => todo!(),
                    }
                }))
            }

            ir::TyKind::Array(item) => {
                if top_level {
                    let mut index = None;
                    if include_index {
                        index = Some(COL_ARRAY_INDEX.to_string());
                    }

                    Box::new(index.into_iter().chain(self.rel_cols_re(
                        item,
                        false,
                        false,
                        field_prefix,
                    )))
                } else {
                    Box::new(Some(COL_VALUE.to_string()).into_iter())
                }
            }
            ir::TyKind::Enum(_) => todo!(),
            ir::TyKind::Function(_) => todo!(),
            ir::TyKind::Ident(_) => todo!(),
        }
    }
}

#[cfg(test)]
mod rel_repr {
    use itertools::Itertools;
    use lutra_bin::ir;

    fn r(ty: &ir::Ty) -> String {
        let ctx = super::Context {
            types: Default::default(),
        };
        let x = ctx.rel_cols(ty, true).join(", ");
        x
    }

    #[test]
    fn rel_cols_brute_force() {
        // depth 1
        assert_eq!(r(&prim()), "value");
        assert_eq!(r(&tuple_0()), "");

        // depth 2
        assert_eq!(r(&tuple_1(prim())), "f_0");
        assert_eq!(r(&tuple_1(tuple_0())), "");
        assert_eq!(r(&array(prim())), "index, value");
        assert_eq!(r(&array(tuple_0())), "index");

        // depth 3
        assert_eq!(r(&tuple_1(tuple_1(prim()))), "f_0_0");
        assert_eq!(r(&tuple_1(tuple_1(tuple_0()))), "");
        assert_eq!(r(&tuple_1(array(prim()))), "f_0");
        assert_eq!(r(&tuple_1(array(tuple_0()))), "f_0");
        assert_eq!(r(&array(tuple_1(prim()))), "index, f_0");
        assert_eq!(r(&array(tuple_1(tuple_0()))), "index");
        assert_eq!(r(&array(array(prim()))), "index, value");
        assert_eq!(r(&array(array(tuple_0()))), "index, value");
    }

    #[test]
    fn rel_cols_special() {
        assert_eq!(
            r(&tuple_3(prim(), tuple_2(prim(), tuple_0()), prim())),
            "f_0, f_1_0, f_2"
        );
        assert_eq!(
            r(&tuple_3(
                tuple_2(prim(), tuple_0()),
                tuple_0(),
                tuple_2(tuple_0(), tuple_0())
            )),
            "f_0_0"
        );
        assert_eq!(r(&tuple_3(prim(), array(prim()), prim())), "f_0, f_1, f_2");

        assert_eq!(r(&tuple_3(prim(), tuple_0(), prim())), "f_0, f_2");
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
}
