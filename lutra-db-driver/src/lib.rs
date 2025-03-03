use futures::StreamExt;
use lutra_bin::{ir, Data};
use tokio_postgres::{Client, Row};

/// Converts arrays of tuples to expected type.
/// RDBMS can only return relational (array of tuples) results, but we want to
/// be able to return arbitrary types.
/// Assumes that rel_ty is an array of tuples.
pub fn repack(rel_ty: &ir::Ty, data: Data, expected_ty: &ir::Ty) -> Data {
    let tuple_ty = rel_ty.kind.as_array().unwrap();
    let _fields_ty = tuple_ty.kind.as_tuple().unwrap();

    // TODO: this function currently covers only two cases:
    // - T <- [{T}]
    // - {..} <- [{..}]
    // It does not yet handle:
    // - [T] <- [{T}] (array of primitives)
    // - [{a = {..}, b = {..}}] <- [{..}] (array of nested tuples)
    // - [{[..]}] <- [{text}] (array of nested non-tuples)
    match expected_ty.kind {
        ir::TyKind::Primitive(_) => {
            // data is [{value}], we need just value
            let array = lutra_bin::ArrayReader::new_for_ty(data, rel_ty);
            let item = array.get(0).unwrap();

            let tuple = lutra_bin::TupleReader::new_for_ty(&item, tuple_ty);

            tuple.get_field(0)
        }
        ir::TyKind::Tuple(_) => {
            // data is [{..}], we need just {..}

            let array = lutra_bin::ArrayReader::new_for_ty(data, rel_ty);
            array.get(0).unwrap()
        }
        ir::TyKind::Array(_) => {
            // nothing to unpack
            data
        }
        ir::TyKind::Enum(_) => todo!(),
        ir::TyKind::Function(_) | ir::TyKind::Ident(_) => unreachable!(),
    }
}

pub async fn query(client: Client, query: &str) -> Result<(ir::Ty, Data), tokio_postgres::Error> {
    // parse
    let stmt = client.prepare_typed(query, &[]).await?;

    let result_ty_array = get_stmt_result_ty(&stmt);
    let result_ty_tuple = result_ty_array.kind.as_array().unwrap();
    let result_ty_fields = result_ty_tuple.kind.as_tuple().unwrap();

    // execute
    const NO_PARAMS: [i32; 0] = [];
    let mut row_stream = Box::pin(client.query_raw(&stmt, NO_PARAMS).await?);

    // re-write to lutra
    let mut array = lutra_bin::ArrayWriter::new_for_ty(&result_ty_array);
    while let Some(row) = row_stream.next().await {
        let row = row?;

        let mut tuple = lutra_bin::TupleWriter::new_for_ty(result_ty_tuple);
        for (idx, ty_field) in result_ty_fields.iter().enumerate() {
            tuple.write_field(get_cell(&row, idx, &ty_field.ty));
        }
        array.write_item(tuple.finish());
    }
    let result = array.finish();

    Ok((result_ty_array, result))
}

fn get_stmt_result_ty(stmt: &tokio_postgres::Statement) -> lutra_bin::ir::Ty {
    let fields = stmt
        .columns()
        .iter()
        .map(|col| ir::TyTupleField {
            name: Some(col.name().to_string()),
            ty: get_column_ty(col.type_()),
        })
        .collect();

    let mut tuple = ir::Ty {
        kind: ir::TyKind::Tuple(fields),
        name: None,
        layout: None,
    };
    tuple.layout = lutra_bin::layout::get_layout_simple(&tuple);

    let mut array = ir::Ty {
        kind: ir::TyKind::Array(Box::new(tuple)),
        name: None,
        layout: None,
    };
    array.layout = lutra_bin::layout::get_layout_simple(&array);
    array
}

fn get_column_ty(ty: &tokio_postgres::types::Type) -> lutra_bin::ir::Ty {
    let kind = match ty.name() {
        "bool" => ir::TyKind::Primitive(ir::PrimitiveSet::bool),
        "int2" => ir::TyKind::Primitive(ir::PrimitiveSet::int16),
        "int4" => ir::TyKind::Primitive(ir::PrimitiveSet::int32),
        "int8" => ir::TyKind::Primitive(ir::PrimitiveSet::int64),
        "float4" => ir::TyKind::Primitive(ir::PrimitiveSet::float32),
        "float8" => ir::TyKind::Primitive(ir::PrimitiveSet::float64),
        _ => todo!("pg type: {}", ty.name()),
    };
    let mut ty = lutra_bin::ir::Ty {
        kind,
        layout: None,
        name: None,
    };
    ty.layout = lutra_bin::layout::get_layout_simple(&ty);
    ty
}

fn get_cell(row: &Row, idx: usize, ty: &ir::Ty) -> Data {
    let ir::TyKind::Primitive(prim_set) = &ty.kind else {
        unreachable!()
    };

    match prim_set {
        ir::PrimitiveSet::bool => encode(&row.get::<_, bool>(idx)),
        ir::PrimitiveSet::int8 => todo!(),
        ir::PrimitiveSet::int16 => encode(&row.get::<_, i16>(idx)),
        ir::PrimitiveSet::int32 => encode(&row.get::<_, i32>(idx)),
        ir::PrimitiveSet::int64 => encode(&row.get::<_, i64>(idx)),
        ir::PrimitiveSet::uint8 => todo!(),
        ir::PrimitiveSet::uint16 => todo!(),
        ir::PrimitiveSet::uint32 => todo!(),
        ir::PrimitiveSet::uint64 => todo!(),
        ir::PrimitiveSet::float32 => encode(&row.get::<_, f32>(idx)),
        ir::PrimitiveSet::float64 => encode(&row.get::<_, f64>(idx)),
        ir::PrimitiveSet::text => todo!(),
    }
}

fn encode<T: lutra_bin::Encode + lutra_bin::Layout>(value: &T) -> lutra_bin::Data {
    let mut buf = bytes::BytesMut::with_capacity(T::head_size() / 8);
    value.encode(&mut buf);
    lutra_bin::Data::new(buf.to_vec())
}
