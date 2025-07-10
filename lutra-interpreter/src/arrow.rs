use arrow::array as arrow_array;
use arrow::datatypes as arrow_datatypes;

use lutra_bin::ir;

pub fn validate_schema(schema: &arrow_datatypes::Schema, item_ty: &ir::Ty) -> Result<(), String> {
    let ty_fields = item_ty.kind.as_tuple().unwrap();

    for (index, (ty_f, f)) in std::iter::zip(ty_fields, schema.fields()).enumerate() {
        let field_name = ty_f
            .name
            .clone()
            .unwrap_or_else(|| format!("field {index}"));

        match &ty_f.ty.kind {
            ir::TyKind::Primitive(prim) => {
                let expected = match prim {
                    ir::TyPrimitive::bool => arrow_datatypes::DataType::Boolean,
                    ir::TyPrimitive::int8 => arrow_datatypes::DataType::Int8,
                    ir::TyPrimitive::int16 => arrow_datatypes::DataType::Int16,
                    ir::TyPrimitive::int32 => arrow_datatypes::DataType::Int32,
                    ir::TyPrimitive::int64 => arrow_datatypes::DataType::Int64,
                    ir::TyPrimitive::uint8 => arrow_datatypes::DataType::UInt8,
                    ir::TyPrimitive::uint16 => arrow_datatypes::DataType::UInt16,
                    ir::TyPrimitive::uint32 => arrow_datatypes::DataType::UInt32,
                    ir::TyPrimitive::uint64 => arrow_datatypes::DataType::UInt64,
                    ir::TyPrimitive::float32 => arrow_datatypes::DataType::Float32,
                    ir::TyPrimitive::float64 => arrow_datatypes::DataType::Float64,
                    ir::TyPrimitive::text => arrow_datatypes::DataType::Utf8,
                };
                validate_field(expected, f).map_err(|m| format!("{field_name}: {m}"))?;
            }

            ir::TyKind::Tuple(_) => unimplemented!(),
            ir::TyKind::Array(_) => unimplemented!(),
            ir::TyKind::Enum(_) => unimplemented!(),

            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => panic!(),
        };
    }
    Ok(())
}

fn validate_field(
    expected: arrow_datatypes::DataType,
    found: &arrow_datatypes::Field,
) -> Result<(), String> {
    if &expected != found.data_type() {
        let nullable = if found.is_nullable() { "nullable " } else { "" };
        return Err(format!(
            "expected {expected}, found {nullable}{}",
            found.data_type()
        ));
    }
    Ok(())
}

pub fn arrow_to_lutra(
    reader: impl arrow_array::RecordBatchReader,
    item_ty: &ir::Ty,
) -> Result<lutra_bin::Data, ()> {
    let ty_fields = item_ty.kind.as_tuple().ok_or(())?;

    let mut output = lutra_bin::ArrayWriter::new_for_item_ty(item_ty);
    for batch in reader {
        let batch = batch.unwrap();

        for row_index in 0..batch.num_rows() {
            let mut tuple = lutra_bin::TupleWriter::new_for_ty(item_ty);
            for (col_index, ty_field) in ty_fields.iter().enumerate() {
                let array = batch.column(col_index).as_any();

                let value = match ty_field.ty.kind {
                    ir::TyKind::Primitive(ir::TyPrimitive::bool) => encode(
                        &array
                            .downcast_ref::<arrow_array::BooleanArray>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::int8) => encode(
                        &array
                            .downcast_ref::<arrow_array::Int8Array>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::int16) => encode(
                        &array
                            .downcast_ref::<arrow_array::Int16Array>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::int32) => encode(
                        &array
                            .downcast_ref::<arrow_array::Int32Array>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::int64) => encode(
                        &array
                            .downcast_ref::<arrow_array::Int64Array>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::uint8) => encode(
                        &array
                            .downcast_ref::<arrow_array::UInt8Array>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::uint16) => encode(
                        &array
                            .downcast_ref::<arrow_array::UInt16Array>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::uint32) => encode(
                        &array
                            .downcast_ref::<arrow_array::UInt32Array>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::uint64) => encode(
                        &array
                            .downcast_ref::<arrow_array::UInt64Array>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::float32) => encode(
                        &array
                            .downcast_ref::<arrow_array::Float32Array>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::float64) => encode(
                        &array
                            .downcast_ref::<arrow_array::Float64Array>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::text) => encode(
                        array
                            .downcast_ref::<arrow_array::StringArray>()
                            .unwrap()
                            .value(row_index),
                    ),

                    ir::TyKind::Tuple(_) | ir::TyKind::Array(_) | ir::TyKind::Enum(_) => {
                        todo!()
                    }
                    ir::TyKind::Function(_) | ir::TyKind::Ident(_) => {
                        return Err(());
                    }
                };

                tuple.write_field(value);
            }
            output.write_item(tuple.finish());
        }
    }
    Ok(output.finish())
}

fn encode<T: lutra_bin::Encode + lutra_bin::Layout + ?Sized>(value: &T) -> lutra_bin::Data {
    lutra_bin::Data::new(value.encode())
}
