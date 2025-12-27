use arrow::array as aa;
use arrow::datatypes as ad;
use lutra_bin::bytes::{self, BufMut};
use lutra_bin::{Encode, ir};

/// Converts an Arrow reader into a Lutra buffer.
///
/// Returns `None` if Arrow schema does not conform to the Lutra type.
pub fn arrow_to_lutra(
    reader: impl aa::RecordBatchReader,
    ty: &ir::Ty,
    ty_defs: &[ir::TyDef],
) -> Option<bytes::BytesMut> {
    let ctx = super::Context::new(ty_defs);

    let ty_item = ctx.get_ty_mat(ty).kind.as_array()?;
    let ty_fields = ctx.get_ty_mat(ty_item).kind.as_tuple()?;
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
        for row_idx in 0..batch.num_rows() {
            for (col_index, ty_field) in ty_fields.iter().enumerate() {
                let array = batch.column(col_index);
                let dt = array.data_type();
                let array = array.as_any();

                fn downcast<'a, T: 'static>(
                    array: &'a dyn std::any::Any,
                    e: &'static str,
                    dt: &ad::DataType,
                ) -> &'a T {
                    array
                        .downcast_ref::<T>()
                        .unwrap_or_else(|| panic!("expected {e}, found {dt}"))
                }

                match ty_field.kind {
                    ir::TyKind::Primitive(ir::TyPrimitive::bool) => {
                        downcast::<aa::BooleanArray>(array, "Bool", dt)
                            .value(row_idx)
                            .encode_head(&mut array_buf)
                    }
                    ir::TyKind::Primitive(ir::TyPrimitive::int8) => {
                        downcast::<aa::Int8Array>(array, "Int8", dt)
                            .value(row_idx)
                            .encode_head(&mut array_buf)
                    }
                    ir::TyKind::Primitive(ir::TyPrimitive::int16) => {
                        downcast::<aa::Int16Array>(array, "Int16", dt)
                            .value(row_idx)
                            .encode_head(&mut array_buf)
                    }
                    ir::TyKind::Primitive(ir::TyPrimitive::int32) => {
                        if matches!(dt, ad::DataType::Date32) {
                            downcast::<aa::Date32Array>(array, "Date32", dt)
                                .value(row_idx)
                                .encode_head(&mut array_buf)
                        } else {
                            downcast::<aa::Int32Array>(array, "Int32", dt)
                                .value(row_idx)
                                .encode_head(&mut array_buf)
                        }
                    }
                    ir::TyKind::Primitive(ir::TyPrimitive::int64) => {
                        if matches!(dt, ad::DataType::Timestamp(ad::TimeUnit::Microsecond, None)) {
                            downcast::<aa::TimestampMicrosecondArray>(array, "TimestampMs", dt)
                                .value(row_idx)
                                .encode_head(&mut array_buf)
                        } else {
                            downcast::<aa::Int64Array>(array, "Int64", dt)
                                .value(row_idx)
                                .encode_head(&mut array_buf)
                        }
                    }
                    ir::TyKind::Primitive(ir::TyPrimitive::uint8) => {
                        downcast::<aa::UInt8Array>(array, "UInt8", dt)
                            .value(row_idx)
                            .encode_head(&mut array_buf)
                    }
                    ir::TyKind::Primitive(ir::TyPrimitive::uint16) => {
                        downcast::<aa::UInt16Array>(array, "UInt16", dt)
                            .value(row_idx)
                            .encode_head(&mut array_buf)
                    }
                    ir::TyKind::Primitive(ir::TyPrimitive::uint32) => {
                        downcast::<aa::UInt32Array>(array, "UInt32", dt)
                            .value(row_idx)
                            .encode_head(&mut array_buf)
                    }
                    ir::TyKind::Primitive(ir::TyPrimitive::uint64) => {
                        downcast::<aa::UInt64Array>(array, "UInt64", dt)
                            .value(row_idx)
                            .encode_head(&mut array_buf)
                    }
                    ir::TyKind::Primitive(ir::TyPrimitive::float32) => {
                        downcast::<aa::Float32Array>(array, "Float32", dt)
                            .value(row_idx)
                            .encode_head(&mut array_buf)
                    }
                    ir::TyKind::Primitive(ir::TyPrimitive::float64) => {
                        downcast::<aa::Float64Array>(array, "Float64", dt)
                            .value(row_idx)
                            .encode_head(&mut array_buf)
                    }
                    ir::TyKind::Primitive(ir::TyPrimitive::text) => {
                        let string = Option::or(
                            array
                                .downcast_ref::<aa::StringArray>()
                                .map(|array| array.value(row_idx)),
                            array
                                .downcast_ref::<aa::LargeStringArray>()
                                .map(|array| array.value(row_idx)),
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
