use arrow::array as arrow_array;
use lutra_bin::bytes::{self, BufMut};
use lutra_bin::{Encode, ir};

/// Converts an Arrow reader into a Lutra buffer.
///
/// Returns `None` if Arrow schema does not conform to the Lutra type.
pub fn arrow_to_lutra(
    reader: impl arrow_array::RecordBatchReader,
    ty: &ir::Ty,
    ty_defs: &[ir::TyDef],
) -> Option<bytes::BytesMut> {
    let ctx = super::Context::new(ty_defs);

    let ty_item = ctx.get_ty_mat(ty).kind.as_array()?;
    let ty_fields = ctx.get_ty_mat(&ty_item).kind.as_tuple()?;
    let ty_fields: Vec<_> = ty_fields.iter().map(|f| ctx.get_ty_mat(&f.ty)).collect();

    // buffer for body of the result array
    let mut array_buf = bytes::BytesMut::new();

    // buffer for bodies of items
    let mut bodies_buf = bytes::BytesMut::new();

    // non-finalized pointers from array_buf into bodies_buf
    let mut ptrs = Vec::new();

    // TODO: we could pre-calculate the array_buf size so ptrs don't need to be
    // written after we are don't writing into array_buf.

    let mut array_len = 0;
    for batch in reader {
        let batch = batch.unwrap();

        array_len += batch.num_rows();
        for row_index in 0..batch.num_rows() {
            for (col_index, ty_field) in ty_fields.iter().enumerate() {
                let array = batch.column(col_index).as_any();

                match ty_field.kind {
                    ir::TyKind::Primitive(ir::TyPrimitive::bool) => array
                        .downcast_ref::<arrow_array::BooleanArray>()
                        .unwrap()
                        .value(row_index)
                        .encode_head(&mut array_buf),
                    ir::TyKind::Primitive(ir::TyPrimitive::int8) => array
                        .downcast_ref::<arrow_array::Int8Array>()
                        .unwrap()
                        .value(row_index)
                        .encode_head(&mut array_buf),
                    ir::TyKind::Primitive(ir::TyPrimitive::int16) => array
                        .downcast_ref::<arrow_array::Int16Array>()
                        .unwrap()
                        .value(row_index)
                        .encode_head(&mut array_buf),
                    ir::TyKind::Primitive(ir::TyPrimitive::int32) => array
                        .downcast_ref::<arrow_array::Int32Array>()
                        .unwrap()
                        .value(row_index)
                        .encode_head(&mut array_buf),
                    ir::TyKind::Primitive(ir::TyPrimitive::int64) => array
                        .downcast_ref::<arrow_array::Int64Array>()
                        .unwrap()
                        .value(row_index)
                        .encode_head(&mut array_buf),
                    ir::TyKind::Primitive(ir::TyPrimitive::uint8) => array
                        .downcast_ref::<arrow_array::UInt8Array>()
                        .unwrap()
                        .value(row_index)
                        .encode_head(&mut array_buf),
                    ir::TyKind::Primitive(ir::TyPrimitive::uint16) => array
                        .downcast_ref::<arrow_array::UInt16Array>()
                        .unwrap()
                        .value(row_index)
                        .encode_head(&mut array_buf),
                    ir::TyKind::Primitive(ir::TyPrimitive::uint32) => array
                        .downcast_ref::<arrow_array::UInt32Array>()
                        .unwrap()
                        .value(row_index)
                        .encode_head(&mut array_buf),
                    ir::TyKind::Primitive(ir::TyPrimitive::uint64) => array
                        .downcast_ref::<arrow_array::UInt64Array>()
                        .unwrap()
                        .value(row_index)
                        .encode_head(&mut array_buf),
                    ir::TyKind::Primitive(ir::TyPrimitive::float32) => array
                        .downcast_ref::<arrow_array::Float32Array>()
                        .unwrap()
                        .value(row_index)
                        .encode_head(&mut array_buf),
                    ir::TyKind::Primitive(ir::TyPrimitive::float64) => array
                        .downcast_ref::<arrow_array::Float64Array>()
                        .unwrap()
                        .value(row_index)
                        .encode_head(&mut array_buf),
                    ir::TyKind::Primitive(ir::TyPrimitive::text) => {
                        let string = Option::or(
                            array
                                .downcast_ref::<arrow_array::StringArray>()
                                .map(|array| array.value(row_index)),
                            array
                                .downcast_ref::<arrow_array::LargeStringArray>()
                                .map(|array| array.value(row_index)),
                        )
                        .unwrap();

                        // encode head
                        let rptr = lutra_bin::ReversePointer::new(&mut array_buf);
                        array_buf.put_u32_le(string.len() as u32);

                        // encode body
                        let bodies_marker = bodies_buf.len();
                        bodies_buf.put_slice(string.as_bytes());

                        // store pointer so we can finalize it later
                        ptrs.push((rptr, bodies_marker));
                    }

                    ir::TyKind::Tuple(_) | ir::TyKind::Array(_) | ir::TyKind::Enum(_) => {
                        todo!()
                    }

                    ir::TyKind::Function(_) => return None,
                    ir::TyKind::Ident(_) => unreachable!(),
                };
            }
        }
    }

    // finalize pointers
    for (ptr, bodies_marker) in ptrs {
        let absolute_ptr = array_buf.len() + bodies_marker;
        ptr.write(&mut array_buf, absolute_ptr);
    }

    let mut result = bytes::BytesMut::new();
    result.put_u32_le(8); // offset
    result.put_u32_le(array_len as u32); // len
    result.put(array_buf);
    result.put(bodies_buf);

    Some(result)
}
