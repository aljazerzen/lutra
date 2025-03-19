mod repack;

pub use repack::repack;

use bytes::BufMut;
use core::str;
use futures::StreamExt;
use lutra_bin::{ir, Data, Decode};
use tokio_postgres::types as pg_ty;
use tokio_postgres::{Client, Row};

pub async fn query(
    client: Client,
    query: &str,
    params: impl IntoIterator<Item = &[u8]>,
) -> Result<(ir::Ty, Data), tokio_postgres::Error> {
    // parse
    let stmt = client.prepare(query).await?;

    let result_ty_array = get_stmt_result_ty(&stmt);
    let result_ty_tuple = result_ty_array.kind.as_array().unwrap();
    let result_ty_fields = result_ty_tuple.kind.as_tuple().unwrap();

    // execute
    let params: Vec<_> = params.into_iter().map(|data| Param { data }).collect();
    let mut row_stream = Box::pin(client.query_raw(&stmt, &params).await?);

    // re-write to Lutra
    let mut array = lutra_bin::ArrayWriter::new_for_ty(&result_ty_array);
    while let Some(row) = row_stream.next().await {
        let row = row?;

        let mut tuple = lutra_bin::TupleWriter::new_for_ty(result_ty_tuple);
        for (idx, ty_field) in result_ty_fields.iter().enumerate() {
            tuple.write_field(read_cell(&row, idx, &ty_field.ty));
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
            ty: pg_ty_to_ir(col.type_()),
        })
        .collect();

    let mut tuple = ir::Ty::new(ir::TyKind::Tuple(fields));
    tuple.layout = lutra_bin::layout::get_layout_simple(&tuple);

    let mut array = ir::Ty::new(ir::TyKind::Array(Box::new(tuple)));
    array.layout = lutra_bin::layout::get_layout_simple(&array);
    array
}

fn pg_ty_to_ir(ty: &pg_ty::Type) -> lutra_bin::ir::Ty {
    let kind = match ty.name() {
        "bool" => ir::TyKind::Primitive(ir::TyPrimitive::bool),
        "int2" => ir::TyKind::Primitive(ir::TyPrimitive::int16),
        "int4" => ir::TyKind::Primitive(ir::TyPrimitive::int32),
        "int8" => ir::TyKind::Primitive(ir::TyPrimitive::int64),
        "float4" => ir::TyKind::Primitive(ir::TyPrimitive::float32),
        "float8" => ir::TyKind::Primitive(ir::TyPrimitive::float64),
        "text" => ir::TyKind::Primitive(ir::TyPrimitive::text),
        "jsonb" => ir::TyKind::Primitive(ir::TyPrimitive::text),
        "json" => ir::TyKind::Primitive(ir::TyPrimitive::text),
        _ => todo!("pg type: {}", ty.name()),
    };
    let mut ty = lutra_bin::ir::Ty::new(kind);
    ty.layout = lutra_bin::layout::get_layout_simple(&ty);
    ty
}

fn read_cell(row: &Row, idx: usize, ty: &ir::Ty) -> Data {
    let ir::TyKind::Primitive(prim_set) = &ty.kind else {
        unreachable!()
    };

    match prim_set {
        ir::TyPrimitive::bool => encode(&row.get::<_, bool>(idx)),
        ir::TyPrimitive::int8 => todo!(),
        ir::TyPrimitive::int16 => encode(&row.get::<_, i16>(idx)),
        ir::TyPrimitive::int32 => encode(&row.get::<_, i32>(idx)),
        ir::TyPrimitive::int64 => encode(&row.get::<_, i64>(idx)),
        ir::TyPrimitive::uint8 => todo!(),
        ir::TyPrimitive::uint16 => todo!(),
        ir::TyPrimitive::uint32 => todo!(),
        ir::TyPrimitive::uint64 => todo!(),
        ir::TyPrimitive::float32 => encode(&row.get::<_, f32>(idx)),
        ir::TyPrimitive::float64 => encode(&row.get::<_, f64>(idx)),
        ir::TyPrimitive::text => encode(row.get::<_, TextOrJson>(idx).0),
    }
}

fn encode<T: lutra_bin::Encode + lutra_bin::Layout + ?Sized>(value: &T) -> lutra_bin::Data {
    let mut buf = bytes::BytesMut::with_capacity(T::head_size().div_ceil(8));
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

#[derive(Debug)]
struct Param<'a> {
    data: &'a [u8],
}

impl<'a> pg_ty::ToSql for Param<'a> {
    fn to_sql(
        &self,
        ty: &pg_ty::Type,
        out: &mut bytes::BytesMut,
    ) -> Result<pg_ty::IsNull, Box<dyn std::error::Error + Sync + Send>>
    where
        Self: Sized,
    {
        match ty.name() {
            "bool" => out.put_slice(&self.data[0..1]),
            "int1" => todo!(),
            "int2" => out.extend(self.data[0..2].iter().rev()),
            "int4" => out.extend(self.data[0..4].iter().rev()),
            "int8" => out.extend(self.data[0..8].iter().rev()),
            "uint8" => todo!(),
            "uint16" => todo!(),
            "uint32" => todo!(),
            "uint64" => todo!(),
            "float32" => out.put_slice(&self.data[0..4]),
            "float64" => out.put_slice(&self.data[0..8]),
            "text" => {
                let val = String::decode(self.data)?;
                out.put_slice(val.as_bytes())
            }
            _ => todo!(),
        }
        Ok(pg_ty::IsNull::No)
    }

    fn accepts(_ty: &pg_ty::Type) -> bool {
        true
    }

    pg_ty::to_sql_checked!();
}
