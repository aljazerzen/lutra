use crate::{Context, Error};
use async_duckdb::duckdb::Rows;
use bytes::{BufMut, BytesMut};
use lutra_bin::{ReversePointer, ir};

pub fn from_duckdb(mut rows: Rows, ty: &ir::Ty, ctx: &Context) -> Result<Vec<u8>, Error> {
    let mut buf = BytesMut::new();

    // Process rows based on output type
    match &ty.kind {
        ir::TyKind::Array(item_ty) => {
            // Array of rows
            encode_array(&mut buf, &mut rows, item_ty, ctx)?;
        }
        _ if ty.is_unit() => {
            // Unit type - no output
        }
        _ => {
            // Single row
            if let Some(row) = rows.next().map_err(Error::from_duck)? {
                encode_row(&mut buf, row, ty, ctx, 0)?;
            } else {
                return Err(Error::BadDatabaseResponse("expected at least one row"));
            }
        }
    }

    Ok(buf.to_vec())
}

fn encode_array(
    buf: &mut BytesMut,
    rows: &mut Rows,
    item_ty: &ir::Ty,
    ctx: &Context,
) -> Result<(), Error> {
    // We need to collect rows to know the count
    let mut row_data = Vec::new();
    while let Some(row) = rows.next().map_err(Error::from_duck)? {
        let mut row_buf = BytesMut::new();
        encode_row(&mut row_buf, row, item_ty, ctx, 0)?;
        row_data.push(row_buf);
    }

    // Write array head
    let body_ptr = ReversePointer::new(buf);
    buf.put_u32_le(row_data.len() as u32);
    body_ptr.write_cur_len(buf);

    // Write array items
    for row_buf in row_data {
        buf.put_slice(&row_buf);
    }

    Ok(())
}

fn encode_row(
    buf: &mut BytesMut,
    row: &async_duckdb::duckdb::Row,
    ty: &ir::Ty,
    ctx: &Context,
    mut col_idx: usize,
) -> Result<usize, Error> {
    let ty_mat = ctx.get_ty_mat(ty);

    match &ty_mat.kind {
        ir::TyKind::Primitive(prim) => {
            encode_primitive(buf, row, col_idx, *prim).map_err(Error::from_duck)?;
            Ok(col_idx + 1)
        }

        ir::TyKind::Tuple(fields) => {
            for field in fields {
                col_idx = encode_row(buf, row, &field.ty, ctx, col_idx)?;
            }
            Ok(col_idx)
        }

        ir::TyKind::Array(_) => Err(Error::UnsupportedDataType("array")),
        ir::TyKind::Enum(_) => Err(Error::UnsupportedDataType("enum")),

        ir::TyKind::Function(_) | ir::TyKind::Ident(_) => {
            panic!("unexpected type")
        }
    }
}

fn encode_primitive(
    buf: &mut BytesMut,
    row: &async_duckdb::duckdb::Row,
    col_idx: usize,
    prim: ir::TyPrimitive,
) -> Result<(), async_duckdb::duckdb::Error> {
    use ir::TyPrimitive::*;

    match prim {
        bool => {
            let val: std::primitive::bool = row.get(col_idx)?;
            buf.put_u8(if val { 1 } else { 0 });
        }
        int8 => {
            let val: i8 = row.get(col_idx)?;
            buf.put_i8(val);
        }
        uint8 => {
            let val: u8 = row.get(col_idx)?;
            buf.put_u8(val);
        }
        int16 => {
            let val: i16 = row.get(col_idx)?;
            buf.put_i16_le(val);
        }
        uint16 => {
            let val: u16 = row.get(col_idx)?;
            buf.put_u16_le(val);
        }
        int32 => {
            let val: i32 = row.get(col_idx)?;
            buf.put_i32_le(val);
        }
        uint32 => {
            let val: u32 = row.get(col_idx)?;
            buf.put_u32_le(val);
        }
        int64 => {
            let val: i64 = row.get(col_idx)?;
            buf.put_i64_le(val);
        }
        uint64 => {
            let val: u64 = row.get(col_idx)?;
            buf.put_u64_le(val);
        }
        float32 => {
            let val: f32 = row.get(col_idx)?;
            buf.put_f32_le(val);
        }
        float64 => {
            let val: f64 = row.get(col_idx)?;
            buf.put_f64_le(val);
        }
        text => {
            let val: String = row.get(col_idx)?;
            let bytes = val.as_bytes();

            // Write text with length prefix (Lutra format)
            let body_ptr = ReversePointer::new(buf);
            buf.put_u32_le(bytes.len() as u32);
            body_ptr.write_cur_len(buf);
            buf.put_slice(bytes);
        }
    }

    Ok(())
}
