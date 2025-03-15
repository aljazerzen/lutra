use std::collections::HashMap;

use lutra_bin::{ir, Data};

use crate::encode;

/// Converts arrays of tuples to expected type.
/// RDBMS can only return relational (array of tuples) results, but we want to
/// be able to return arbitrary types.
/// Assumes that rel_ty is an array of tuples.
pub fn repack(ty: &ir::Ty, data: Data, expected_ty: &ir::Ty, ty_defs: &[ir::TyDef]) -> Data {
    let ctx = Context {
        types: ty_defs.iter().map(|def| (&def.name, &def.ty)).collect(),
    };

    ctx.repack(ty, data, expected_ty)
}

pub(super) struct Context<'a> {
    pub(super) types: HashMap<&'a ir::Path, &'a ir::Ty>,
}

impl<'a> Context<'a> {
    pub(super) fn get_ty_mat(&self, ty: &'a ir::Ty) -> &'a ir::Ty {
        match &ty.kind {
            ir::TyKind::Ident(path) => self.types.get(path).unwrap(),
            _ => ty,
        }
    }

    fn repack(&self, ty: &ir::Ty, data: Data, expected_ty: &ir::Ty) -> Data {
        let expected_ty = self.get_ty_mat(expected_ty);

        if ty == expected_ty {
            // nothing to repack
            return data;
        }

        // TODO: currently, this function is very inefficient:
        // - it does two copies when repacking JSON,
        // - it compares types a lot (for each element of array), even though the two type do not change
        //   this should be improved via generating mapping functions which would perform such comparison only once
        // - having this as a separate step from the main query function means that we are copying
        //   data at least once. Maybe we would implement it as a facade on writers in query, which
        //   would just redirect the writes.
        // All this should be done under benchmarks, so we know that the changes actually have an impact.

        match (&ty.kind, &expected_ty.kind) {
            (ir::TyKind::Primitive(p), ir::TyKind::Primitive(e)) => {
                panic!("cannot convert from {p:?} to {e:?}");
            }

            (ir::TyKind::Array(_), ir::TyKind::Primitive(_)) => {
                // data is [{value}], we need just value
                let array = lutra_bin::ArrayReader::new_for_ty(data, ty);
                let item = array.get(0).unwrap();
                let item_ty = ty.kind.as_array().unwrap();

                self.repack(item_ty, item, expected_ty)
            }
            (ir::TyKind::Tuple(_), ir::TyKind::Primitive(_)) => {
                // data is {value}, we need just value
                let tuple = lutra_bin::TupleReader::new_for_ty(&data, ty);
                tuple.get_field(0)
            }

            (ir::TyKind::Array(item_ty), ir::TyKind::Tuple(_)) => {
                // data is [{..}], we need just {..}

                let array = lutra_bin::ArrayReader::new_for_ty(data, ty);
                let item = array.get(0).unwrap();
                self.repack(item_ty, item, expected_ty)
            }

            (ir::TyKind::Tuple(fields), ir::TyKind::Tuple(expected_fields)) => {
                let input = lutra_bin::TupleReader::new_for_ty(&data, ty);
                let mut input = fields
                    .iter()
                    .enumerate()
                    .map(|(index, f)| (input.get_field(index), &f.ty));

                let mut output = lutra_bin::TupleWriter::new_for_ty(expected_ty);
                self.repack_tuple(&mut input, expected_fields, &mut output);
                output.finish()
            }

            (ir::TyKind::Array(item_ty), ir::TyKind::Array(expected_item_ty)) => {
                // map each of the rows
                let mut output = lutra_bin::ArrayWriter::new_for_ty(expected_ty);
                for item in lutra_bin::ArrayReader::new_for_ty(data, ty) {
                    output.write_item(self.repack(item_ty, item, expected_item_ty));
                }
                output.finish()
            }

            (ir::TyKind::Primitive(ir::TyPrimitive::text), _) => {
                // TODO: this might be copying a lot of data, unnecessarily
                let data = data.flatten();
                let data = <String as lutra_bin::Decode>::decode(&data).unwrap();
                self.repack_json(data, expected_ty)
            }
            (ir::TyKind::Tuple(fields), _) if fields.len() == 1 => {
                let tuple = lutra_bin::TupleReader::new_for_ty(&data, ty);

                self.repack(&fields[0].ty, tuple.get_field(0), expected_ty)
            }

            (ir::TyKind::Function(_) | ir::TyKind::Ident(_), _)
            | (_, ir::TyKind::Function(_) | ir::TyKind::Ident(_)) => unreachable!(),

            _ => todo!("ty: {ty:?}, expected: {expected_ty:?}"),
        }
    }

    fn repack_tuple(
        &self,
        input: &mut impl Iterator<Item = (Data, &'a ir::Ty)>,
        expected_fields: &[ir::TyTupleField],
        output: &mut lutra_bin::TupleWriter<'_>,
    ) {
        for e in expected_fields {
            // special case: nested tuples
            if let ir::TyKind::Tuple(inner_expected) = &self.get_ty_mat(&e.ty).kind {
                let mut inner_tuple = lutra_bin::TupleWriter::new_for_ty(self.get_ty_mat(&e.ty));
                self.repack_tuple(input, inner_expected, &mut inner_tuple);
                output.write_field(inner_tuple.finish());
                continue;
            }

            let (data, f) = input.next().unwrap();
            output.write_field(self.repack(f, data, &e.ty))
        }
    }

    fn repack_json(&self, data: String, ty: &ir::Ty) -> Data {
        // TODO: do away with this copy
        let value: tinyjson::JsonValue = data.parse().unwrap();

        self.repack_json_re(value, ty)
    }

    fn repack_json_re(&self, value: tinyjson::JsonValue, ty: &ir::Ty) -> Data {
        let ty = self.get_ty_mat(ty);
        match &ty.kind {
            ir::TyKind::Primitive(primitive) => match (primitive, value) {
                (ir::TyPrimitive::bool, tinyjson::JsonValue::Boolean(v)) => encode(&v),
                (ir::TyPrimitive::int8, tinyjson::JsonValue::Number(v)) => encode(&(v as i8)),
                (ir::TyPrimitive::int16, tinyjson::JsonValue::Number(v)) => encode(&(v as i16)),
                (ir::TyPrimitive::int32, tinyjson::JsonValue::Number(v)) => encode(&(v as i32)),
                (ir::TyPrimitive::int64, tinyjson::JsonValue::Number(v)) => encode(&(v as i64)),
                (ir::TyPrimitive::uint8, tinyjson::JsonValue::Number(v)) => encode(&(v as u8)),
                (ir::TyPrimitive::uint16, tinyjson::JsonValue::Number(v)) => encode(&(v as u16)),
                (ir::TyPrimitive::uint32, tinyjson::JsonValue::Number(v)) => encode(&(v as u32)),
                (ir::TyPrimitive::uint64, tinyjson::JsonValue::Number(v)) => encode(&(v as u64)),
                (ir::TyPrimitive::float32, tinyjson::JsonValue::Number(v)) => encode(&(v as f32)),
                (ir::TyPrimitive::float64, tinyjson::JsonValue::Number(v)) => encode(&(v as u64)),
                (ir::TyPrimitive::text, tinyjson::JsonValue::String(v)) => encode(&v),
                (_, v) => panic!(
                    "expected {primitive:?}, found JSON {}",
                    v.stringify().unwrap()
                ),
            },
            ir::TyKind::Tuple(fields) => {
                let tinyjson::JsonValue::Array(items) = value else {
                    panic!("expected array")
                };
                let mut output = lutra_bin::TupleWriter::new_for_ty(ty);
                for (item, field) in std::iter::zip(items, fields) {
                    output.write_field(self.repack_json_re(item, &field.ty))
                }
                output.finish()
            }
            ir::TyKind::Array(item_ty) => {
                let tinyjson::JsonValue::Array(items) = value else {
                    panic!("expected array")
                };
                let mut output = lutra_bin::ArrayWriter::new_for_ty(ty);
                for item in items {
                    output.write_item(self.repack_json_re(item, item_ty))
                }
                output.finish()
            }
            ir::TyKind::Enum(_) => todo!(),
            ir::TyKind::Function(_) => todo!(),
            ir::TyKind::Ident(_) => todo!(),
        }
    }
}
