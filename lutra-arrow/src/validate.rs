use arrow::datatypes as arrow_datatypes;
use lutra_bin::ir;

/// Checks that a Lutra type is expressed with the given Arrow schema
pub fn validate_schema(
    schema: &arrow_datatypes::Schema,
    ty: &ir::Ty,
    ty_defs: &[ir::TyDef],
) -> Result<(), String> {
    let ctx = super::Context::new(ty_defs);

    let ty_item =
        (ctx.get_ty_mat(ty).kind.as_array()).ok_or_else(|| "expected an array".to_string())?;

    let ty_fields =
        (ctx.get_ty_mat(ty_item).kind.as_tuple()).ok_or_else(|| "expected an tuple".to_string())?;

    for (index, (ty_f, f)) in std::iter::zip(ty_fields, schema.fields()).enumerate() {
        let field_name = ty_f
            .name
            .clone()
            .unwrap_or_else(|| format!("field {index}"));

        validate_field(ty_f, f, &ctx).map_err(|m| format!("{field_name}: {m}"))?;
    }
    Ok(())
}

fn validate_field(
    ty_f: &ir::TyTupleField,
    f: &std::sync::Arc<arrow_datatypes::Field>,
    ctx: &super::Context,
) -> Result<(), String> {
    match &ctx.get_ty_mat(&ty_f.ty).kind {
        ir::TyKind::Primitive(prim) => {
            let expected: &[arrow_datatypes::DataType] = match prim {
                ir::TyPrimitive::bool => &[arrow_datatypes::DataType::Boolean],
                ir::TyPrimitive::int8 => &[arrow_datatypes::DataType::Int8],
                ir::TyPrimitive::int16 => &[arrow_datatypes::DataType::Int16],
                ir::TyPrimitive::int32 => &[arrow_datatypes::DataType::Int32],
                ir::TyPrimitive::int64 => &[arrow_datatypes::DataType::Int64],
                ir::TyPrimitive::uint8 => &[arrow_datatypes::DataType::UInt8],
                ir::TyPrimitive::uint16 => &[arrow_datatypes::DataType::UInt16],
                ir::TyPrimitive::uint32 => &[arrow_datatypes::DataType::UInt32],
                ir::TyPrimitive::uint64 => &[arrow_datatypes::DataType::UInt64],
                ir::TyPrimitive::float32 => &[arrow_datatypes::DataType::Float32],
                ir::TyPrimitive::float64 => &[arrow_datatypes::DataType::Float64],
                ir::TyPrimitive::text => &[
                    arrow_datatypes::DataType::Utf8,
                    arrow_datatypes::DataType::LargeUtf8,
                ],
            };
            validate_data_type(expected, f)?;
        }

        ir::TyKind::Tuple(_) => unimplemented!(),
        ir::TyKind::Array(_) => unimplemented!(),
        ir::TyKind::Enum(_) => unimplemented!(),

        ir::TyKind::Function(_) | ir::TyKind::Ident(_) => panic!(),
    };
    Ok(())
}

fn validate_data_type(
    expected: &[arrow_datatypes::DataType],
    found: &arrow_datatypes::Field,
) -> Result<(), String> {
    for e in expected {
        if e == found.data_type() {
            return Ok(());
        }
    }
    let nullable = if found.is_nullable() { "nullable " } else { "" };
    let expected: Vec<_> = expected.iter().map(|f| f.to_string()).collect();
    let expected = expected.join(" ,");
    Err(format!(
        "expected {expected}, found {nullable}{}",
        found.data_type()
    ))
}
