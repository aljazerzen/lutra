use lutra_bin::ir;
use sqlparser::ast as sql_ast;

use crate::sql::COL_ARRAY_INDEX;
use crate::sql::utils::{ExprOrSource, RelCols, Scoped};
use crate::sql::{queries, utils};

impl<'a> queries::Context<'a> {
    pub(super) fn serialize_json(&mut self, mut scoped: Scoped, ty: &ir::Ty) -> Scoped {
        let input = self.scoped_as_rel_var(&mut scoped);

        let ty_mat = self.get_ty_mat(ty);
        scoped.expr = match &ty_mat.kind {
            ir::TyKind::Array(ty_item) => {
                let item_cols: Vec<_> = self.rel_cols_nested(ty_item, "".into()).collect();
                let item_serialized = self.serialize_json_nested(input, &item_cols, ty_item);

                ExprOrSource::Source(format!(
                    "COALESCE(jsonb_agg({item_serialized} ORDER BY {input}.{COL_ARRAY_INDEX}), '[]'::jsonb)"
                ))
            }
            ir::TyKind::Tuple(_) => {
                let cols: Vec<_> = self.rel_cols_nested(ty_mat, "".into()).collect();
                ExprOrSource::Source(self.serialize_json_nested(input, &cols, ty_mat))
            }
            _ => unreachable!("{:?}", ty),
        };
        scoped
    }

    fn serialize_json_nested(&self, input_rel: &str, input_cols: &[String], ty: &ir::Ty) -> String {
        let ty_mat = self.get_ty_mat(ty);
        match &ty_mat.kind {
            ir::TyKind::Primitive(prim) => {
                let r = format!("{input_rel}.{}", input_cols[0]);
                match *prim {
                    ir::TyPrimitive::int8 => {
                        format!("ASCII({r})")
                    }
                    _ => r,
                }
                // if strict { format!("to_json({r})") } else { r }
            }
            ir::TyKind::Tuple(fields) => {
                let mut input_cols = input_cols;

                let mut serialized = Vec::with_capacity(fields.len());
                for field in fields {
                    serialized.push(self.serialize_json_nested(input_rel, input_cols, &field.ty));

                    let consumed_cols = self.rel_cols_ty_nested(&field.ty).count();
                    input_cols = &input_cols[consumed_cols..];
                }
                format!("jsonb_build_array({})", serialized.join(", "))
            }
            ir::TyKind::Array(_) => {
                // array will be serialized already
                format!("{input_rel}.{}", input_cols[0])
            }
            ir::TyKind::Enum(variants) => {
                let tag = &input_cols[0];
                let mut input_cols = &input_cols[1..]; // remove tag

                let mut cases = String::new();
                for (tag, variant) in variants.iter().enumerate() {
                    cases += "WHEN ";
                    cases += &tag.to_string();
                    cases += " THEN json_build_object('";
                    cases += &tag.to_string();
                    cases += "', ";

                    let is_recursive =
                        lutra_bin::layout::does_enum_variant_contain_recursive(ty_mat, tag as u16);
                    if is_recursive {
                        cases += &format!("{input_rel}.{}", input_cols[0]);
                        input_cols = &input_cols[1..];
                    } else {
                        cases += &self.serialize_json_nested(input_rel, input_cols, &variant.ty);
                        let consumed_cols = self.rel_cols_ty_nested(&variant.ty).count();
                        input_cols = &input_cols[consumed_cols..];
                    }

                    cases += ")";
                }

                format!("CASE ASCII({input_rel}.{tag}){cases} END")
            }
            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => unreachable!(),
        }
    }

    pub(super) fn deserialize_json(&mut self, mut scoped: Scoped, ty: &ir::Ty) -> Scoped {
        if let Some(simplified) = scoped.as_simplified_expr() {
            scoped = simplified;
        }

        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Array(ty_item) => {
                let mut query = utils::select_empty();

                /*
                FROM json_array_elements(...expr...) j
                SELECT ROW_NUMBER(), j.value
                */

                query.from.push(utils::from(utils::rel_func(
                    utils::new_ident("jsonb_array_elements"),
                    vec![scoped.expr.into_expr()],
                    Some("j".into()),
                )));

                query.projection = vec![sql_ast::SelectItem::ExprWithAlias {
                    expr: ExprOrSource::Source("(ROW_NUMBER() OVER ())::int4".into()).into_expr(),
                    alias: utils::new_ident("index"),
                }];

                let values = self.deserialize_json_nested("j.value".into(), ty_item);
                let names = self.rel_cols_nested(ty_item, "".into());

                for (value, name) in std::iter::zip(values, names) {
                    query.projection.push(sql_ast::SelectItem::ExprWithAlias {
                        expr: ExprOrSource::Source(value).into_expr(),
                        alias: utils::new_ident(name),
                    });
                }

                let rel_var_name = self.rel_name_gen.next();
                let rel_var = utils::sub_rel(utils::query_select(query), rel_var_name.clone());
                scoped.rel_vars.push(utils::lateral(rel_var));
                scoped.expr = ExprOrSource::RelVar(rel_var_name);
            }
            ir::TyKind::Tuple(_) => todo!(),
            _ => unreachable!("{:?}", ty),
        }
        scoped
    }

    /// Given a serialized JSON value, accessible at `json_ref`,
    /// constructs a list of SQL column expressions that evaluate to the relational repr of the value.
    fn deserialize_json_nested(&self, json_ref: String, ty: &ir::Ty) -> Vec<String> {
        let ty_mat = self.get_ty_mat(ty);
        match &ty_mat.kind {
            ir::TyKind::Primitive(prim) => {
                let r = match prim {
                    ir::TyPrimitive::text => format!("jsonb_build_array({json_ref}) ->> 0"),
                    ir::TyPrimitive::int8 => {
                        format!("CHR({json_ref}::text::int2)::\"char\"")
                    }
                    _ => format!("{json_ref}::text::{}", self.compile_ty_name(ty)),
                };
                vec![r]
            }

            ir::TyKind::Array(_) => {
                // arrays remain serialized in a single column
                vec![json_ref.to_string()]
            }
            ir::TyKind::Tuple(fields) => {
                let mut r = Vec::new();

                for (position, f) in fields.iter().enumerate() {
                    r.extend(self.deserialize_json_nested(
                        format!("({json_ref}->{position})"),
                        self.get_ty_mat(&f.ty),
                    ))
                }

                r
            }

            _ => todo!(),
        }
    }
}
