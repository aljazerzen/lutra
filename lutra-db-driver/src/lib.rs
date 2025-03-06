use core::str;

use futures::StreamExt;
use lutra_bin::{ir, Data};
use tokio_postgres::types as pg_ty;
use tokio_postgres::{Client, Row};

/// Converts arrays of tuples to expected type.
/// RDBMS can only return relational (array of tuples) results, but we want to
/// be able to return arbitrary types.
/// Assumes that rel_ty is an array of tuples.
pub fn repack(ty: &ir::Ty, data: Data, expected_ty: &ir::Ty) -> Data {
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

            repack(item_ty, item, expected_ty)
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
            repack(item_ty, item, expected_ty)
        }

        (ir::TyKind::Tuple(fields), ir::TyKind::Tuple(expected_fields)) => {
            let input = lutra_bin::TupleReader::new_for_ty(&data, ty);
            let mut input = fields
                .iter()
                .enumerate()
                .map(|(index, f)| (input.get_field(index), &f.ty));

            let mut output = lutra_bin::TupleWriter::new_for_ty(expected_ty);
            repack_tuple(&mut input, expected_fields, &mut output);
            output.finish()
        }

        (ir::TyKind::Array(item_ty), ir::TyKind::Array(expected_item_ty)) => {
            // map each of the rows
            let mut output = lutra_bin::ArrayWriter::new_for_ty(expected_ty);
            for item in lutra_bin::ArrayReader::new_for_ty(data, ty) {
                output.write_item(repack(item_ty, item, expected_item_ty));
            }
            output.finish()
        }

        (ir::TyKind::Primitive(ir::PrimitiveSet::text), _) => {
            // TODO: this might be copying a lot of data, unnecessarily
            let data = data.flatten();
            let data = <String as lutra_bin::Decode>::decode(&data).unwrap();
            repack_json(data, expected_ty)
        }
        (ir::TyKind::Tuple(fields), _) if fields.len() == 1 => {
            let tuple = lutra_bin::TupleReader::new_for_ty(&data, ty);

            repack(&fields[0].ty, tuple.get_field(0), expected_ty)
        }

        (ir::TyKind::Function(_) | ir::TyKind::Ident(_), _)
        | (_, ir::TyKind::Function(_) | ir::TyKind::Ident(_)) => unreachable!(),

        _ => todo!("ty: {ty:?}, expected: {expected_ty:?}"),
    }
}

fn repack_tuple<'a>(
    input: &mut impl Iterator<Item = (Data, &'a ir::Ty)>,
    expected_fields: &[ir::TyTupleField],
    output: &mut lutra_bin::TupleWriter<'_>,
) {
    for e in expected_fields {
        // special case: nested tuples
        if let ir::TyKind::Tuple(inner_expected) = &e.ty.kind {
            let mut inner_tuple = lutra_bin::TupleWriter::new_for_ty(&e.ty);
            repack_tuple(input, inner_expected, &mut inner_tuple);
            output.write_field(inner_tuple.finish());
            continue;
        }

        let (data, f) = input.next().unwrap();
        output.write_field(repack(f, data, &e.ty))
    }
}

fn repack_json(data: String, ty: &ir::Ty) -> Data {
    // TODO: do away with this copy
    let value: tinyjson::JsonValue = data.parse().unwrap();

    repack_json_re(value, ty)
}

fn repack_json_re(value: tinyjson::JsonValue, ty: &ir::Ty) -> Data {
    match &ty.kind {
        ir::TyKind::Primitive(primitive) => match (primitive, value) {
            (ir::PrimitiveSet::bool, tinyjson::JsonValue::Boolean(v)) => encode(&v),
            (ir::PrimitiveSet::int8, tinyjson::JsonValue::Number(v)) => encode(&(v as i8)),
            (ir::PrimitiveSet::int16, tinyjson::JsonValue::Number(v)) => encode(&(v as i16)),
            (ir::PrimitiveSet::int32, tinyjson::JsonValue::Number(v)) => encode(&(v as i32)),
            (ir::PrimitiveSet::int64, tinyjson::JsonValue::Number(v)) => encode(&(v as i64)),
            (ir::PrimitiveSet::uint8, tinyjson::JsonValue::Number(v)) => encode(&(v as u8)),
            (ir::PrimitiveSet::uint16, tinyjson::JsonValue::Number(v)) => encode(&(v as u16)),
            (ir::PrimitiveSet::uint32, tinyjson::JsonValue::Number(v)) => encode(&(v as u32)),
            (ir::PrimitiveSet::uint64, tinyjson::JsonValue::Number(v)) => encode(&(v as u64)),
            (ir::PrimitiveSet::float32, tinyjson::JsonValue::Number(v)) => encode(&(v as f32)),
            (ir::PrimitiveSet::float64, tinyjson::JsonValue::Number(v)) => encode(&(v as u64)),
            (ir::PrimitiveSet::text, tinyjson::JsonValue::String(v)) => encode(&v),
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
                output.write_field(repack_json_re(item, &field.ty))
            }
            output.finish()
        }
        ir::TyKind::Array(item_ty) => {
            let tinyjson::JsonValue::Array(items) = value else {
                panic!("expected array")
            };
            let mut output = lutra_bin::ArrayWriter::new_for_ty(ty);
            for item in items {
                output.write_item(repack_json_re(item, item_ty))
            }
            output.finish()
        }
        ir::TyKind::Enum(_) => todo!(),
        ir::TyKind::Function(_) => todo!(),
        ir::TyKind::Ident(_) => todo!(),
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
        "text" => ir::TyKind::Primitive(ir::PrimitiveSet::text),
        "jsonb" => ir::TyKind::Primitive(ir::PrimitiveSet::text),
        "json" => ir::TyKind::Primitive(ir::PrimitiveSet::text),
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
        ir::PrimitiveSet::text => encode(&row.get::<_, TextOrJson>(idx).0.to_string()),
    }
}

fn encode<T: lutra_bin::Encode + lutra_bin::Layout>(value: &T) -> lutra_bin::Data {
    let mut buf = bytes::BytesMut::with_capacity(T::head_size() / 8);
    value.encode(&mut buf);
    lutra_bin::Data::new(buf.to_vec())
}

struct TextOrJson<'a>(&'a str);

impl<'a> pg_ty::FromSql<'a> for TextOrJson<'a> {
    fn from_sql(
        ty: &pg_ty::Type,
        raw: &'a [u8],
    ) -> Result<TextOrJson<'a>, Box<dyn std::error::Error + Sync + Send>> {
        if let pg_ty::Type::JSON = *ty {
            return Ok(TextOrJson(str::from_utf8(raw)?));
        }
        if let pg_ty::Type::JSONB = *ty {
            // we only support version 1 (currently the only version)
            assert!(raw[0] == 1);
            return Ok(TextOrJson(str::from_utf8(&raw[1..])?));
        }

        <&'a str as pg_ty::FromSql>::from_sql(ty, raw).map(TextOrJson)
    }

    fn accepts(ty: &pg_ty::Type) -> bool {
        if let pg_ty::Type::JSON | pg_ty::Type::JSONB = *ty {
            return true;
        }
        <&'a str as pg_ty::FromSql>::accepts(ty)
    }
}
