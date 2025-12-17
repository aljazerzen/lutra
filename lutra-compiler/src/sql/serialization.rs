use lutra_bin::ir;

use crate::sql::COL_ARRAY_INDEX;
use crate::sql::utils::{Node, RelCols, is_maybe};
use crate::sql::{queries, utils};

impl<'a> queries::Context<'a> {
    pub(super) fn serialize_json(&mut self, scoped: Node, ty: &ir::Ty) -> Node {
        let (input, input_rels) = self.node_into_rel_var(scoped, ty);

        let ty_mat = self.get_ty_mat(ty);
        let expr = match &ty_mat.kind {
            ir::TyKind::Array(ty_item) => {
                let item_cols: Vec<_> = self.rel_cols_nested(ty_item, "".into()).collect();
                let item_serialized = self.serialize_json_nested(&input, &item_cols, ty_item);

                format!(
                    "COALESCE(jsonb_agg({item_serialized} ORDER BY {input}.{COL_ARRAY_INDEX}), '[]'::jsonb)"
                )
            }
            ir::TyKind::Tuple(_) => {
                let cols: Vec<_> = self.rel_cols_nested(ty_mat, "".into()).collect();
                self.serialize_json_nested(&input, &cols, ty_mat)
            }
            _ => unreachable!("{:?}", ty),
        };

        Node::Column {
            expr: Box::new(sql_ast::Expr::Source(expr)),
            rels: input_rels.into_iter().collect(),
        }
    }

    fn serialize_json_nested(&self, input_rel: &str, input_cols: &[String], ty: &ir::Ty) -> String {
        dbg!(input_cols, ty);
        let ty_mat = self.get_ty_mat(ty);
        match &ty_mat.kind {
            ir::TyKind::Primitive(_) => {
                format!("{input_rel}.{}", input_cols[0])
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
            ir::TyKind::Enum(variants) if is_maybe(variants) => {
                // maybe enum is a nullable column in SQL repr, but in json repr
                // it is an object with one of the two possible properties 0 and 1.
                let col = &input_cols[0];

                let mut r = "CASE".to_string();

                // case: none
                r += &format!(" WHEN {input_rel}.{col} IS NULL THEN jsonb '{{\"0\": []}}'");

                // case: some
                r += " ELSE jsonb_build_object('1', ";
                let is_recursive =
                    lutra_bin::layout::does_enum_variant_contain_recursive(ty_mat, 1_u16);
                if is_recursive {
                    r += &format!("{input_rel}.{col}");
                } else {
                    r += &self.serialize_json_nested(input_rel, input_cols, &variants[1].ty);
                }

                r += ") END";
                r
            }
            ir::TyKind::Enum(variants) => {
                let tag = &input_cols[0];
                let mut input_cols = &input_cols[1..]; // remove tag

                let mut cases = String::new();
                for (tag, variant) in variants.iter().enumerate() {
                    cases += "WHEN ";
                    cases += &tag.to_string();
                    cases += " THEN jsonb_build_object('";
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

                format!("CASE {input_rel}.{tag} {cases} END")
            }
            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => unreachable!(),
        }
    }

    pub(super) fn deserialize_json(&mut self, input: Node, input_ty: &ir::Ty, ty: &ir::Ty) -> Node {
        let (input_expr, input_rels) = self.node_into_column_and_rels(input, input_ty);

        let mut result = utils::select_empty();
        result.from.extend(input_rels);

        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Array(ty_item) => {
                /*
                FROM json_array_elements(...expr...) j
                SELECT ROW_NUMBER(), j.value
                */
                result.from.push(utils::lateral(utils::rel_func(
                    utils::new_ident("jsonb_array_elements"),
                    vec![input_expr],
                    Some("j".into()),
                )));

                result.projection = vec![sql_ast::SelectItem {
                    expr: utils::new_index(None),
                    alias: Some(utils::new_ident(COL_ARRAY_INDEX)),
                }];

                let values = self.deserialize_json_nested("j.value".into(), ty_item);
                let names = self.rel_cols_nested(ty_item, "".into());

                for (value, name) in std::iter::zip(values, names) {
                    result.projection.push(sql_ast::SelectItem {
                        expr: sql_ast::Expr::Source(value),
                        alias: Some(utils::new_ident(name)),
                    });
                }
            }
            ir::TyKind::Tuple(_) => todo!(),
            ir::TyKind::Enum(_) => {
                let json_ref = input_expr.to_string();

                let values = self.deserialize_json_nested(json_ref, ty);

                let names = self.rel_cols_nested(ty, "".into());
                for (value, name) in std::iter::zip(values, names) {
                    result.projection.push(sql_ast::SelectItem {
                        expr: sql_ast::Expr::Source(value),
                        alias: Some(utils::new_ident(name)),
                    });
                }
            }
            _ => unreachable!("{:?}", ty),
        };

        Node::Select(result)
    }

    /// Given a serialized JSON value, accessible at `json_ref`,
    /// constructs a list of SQL column expressions that evaluate to the relational repr of the value.
    fn deserialize_json_nested(&self, json_ref: String, ty: &ir::Ty) -> Vec<String> {
        let ty_mat = self.get_ty_mat(ty);
        match &ty_mat.kind {
            ir::TyKind::Primitive(prim) => {
                let r = match prim {
                    ir::TyPrimitive::text => format!("jsonb_build_array({json_ref}) ->> 0"),
                    _ => format!("{json_ref}::text::{}", self.compile_ty_name(ty)),
                };
                vec![r]
            }

            ir::TyKind::Array(_) => {
                // arrays remain serialized in a single column
                vec![json_ref]
            }
            ir::TyKind::Tuple(fields) => {
                let mut r = Vec::new();

                for (position, f) in fields.iter().enumerate() {
                    r.extend(
                        self.deserialize_json_nested(format!("({json_ref}->{position})"), &f.ty),
                    )
                }

                r
            }
            ir::TyKind::Enum(variants) if is_maybe(variants) => {
                let cols =
                    self.deserialize_json_nested(format!("({json_ref}->'1')"), &variants[1].ty);
                assert_eq!(cols.len(), 1);
                let col = &cols[0];

                vec![format!(
                    "(CASE WHEN ({json_ref}->'1') IS NULL THEN NULL ELSE {col} END)",
                )]
            }
            ir::TyKind::Enum(variants) => {
                let mut r = Vec::new();

                r.push(format!(
                    "(SELECT jsonb_object_keys({json_ref}) LIMIT 1)::int::int2"
                ));

                for (tag, f) in variants.iter().enumerate() {
                    r.extend(self.deserialize_json_nested(format!("({json_ref}->'{tag}')"), &f.ty))
                }

                r
            }

            _ => todo!(),
        }
    }
}
