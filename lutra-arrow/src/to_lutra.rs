use arrow::array as aa;
use arrow::datatypes as ad;
use lutra_bin::Encode;
use lutra_bin::bytes::{self, BufMut};

/// Converts an Arrow reader into a Lutra buffer.
///
/// The Arrow schema is used to infer the structure and types.
/// Returns an array of tuples where each tuple field corresponds to an Arrow field.
pub fn arrow_to_lutra(reader: impl aa::RecordBatchReader) -> bytes::BytesMut {
    let schema = reader.schema();
    let fields = schema.fields();

    // buffer for body of the result array
    let mut arr_buf = bytes::BytesMut::new();

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
            for col_index in 0..fields.len() {
                let array = batch.column(col_index);
                let dt = array.data_type();
                let array = array.as_any();

                fn downcast<T: 'static>(array: &dyn std::any::Any) -> &T {
                    array.downcast_ref::<T>().unwrap()
                }

                match dt {
                    ad::DataType::Boolean => downcast::<aa::BooleanArray>(array)
                        .value(row_idx)
                        .encode_head(&mut arr_buf),
                    ad::DataType::Int8 => downcast::<aa::Int8Array>(array)
                        .value(row_idx)
                        .encode_head(&mut arr_buf),
                    ad::DataType::Int16 => downcast::<aa::Int16Array>(array)
                        .value(row_idx)
                        .encode_head(&mut arr_buf),
                    ad::DataType::Int32 => downcast::<aa::Int32Array>(array)
                        .value(row_idx)
                        .encode_head(&mut arr_buf),
                    ad::DataType::Date32 => downcast::<aa::Date32Array>(array)
                        .value(row_idx)
                        .encode_head(&mut arr_buf),
                    ad::DataType::Int64 => downcast::<aa::Int64Array>(array)
                        .value(row_idx)
                        .encode_head(&mut arr_buf),
                    ad::DataType::Timestamp(ad::TimeUnit::Microsecond, _) => {
                        downcast::<aa::TimestampMicrosecondArray>(array)
                            .value(row_idx)
                            .encode_head(&mut arr_buf)
                    }
                    ad::DataType::UInt8 => downcast::<aa::UInt8Array>(array)
                        .value(row_idx)
                        .encode_head(&mut arr_buf),
                    ad::DataType::UInt16 => downcast::<aa::UInt16Array>(array)
                        .value(row_idx)
                        .encode_head(&mut arr_buf),
                    ad::DataType::UInt32 => downcast::<aa::UInt32Array>(array)
                        .value(row_idx)
                        .encode_head(&mut arr_buf),
                    ad::DataType::UInt64 => downcast::<aa::UInt64Array>(array)
                        .value(row_idx)
                        .encode_head(&mut arr_buf),
                    ad::DataType::Float32 => downcast::<aa::Float32Array>(array)
                        .value(row_idx)
                        .encode_head(&mut arr_buf),
                    ad::DataType::Float64 => downcast::<aa::Float64Array>(array)
                        .value(row_idx)
                        .encode_head(&mut arr_buf),
                    ad::DataType::Utf8 => {
                        let string = downcast::<aa::StringArray>(array).value(row_idx);
                        write_str(string, &mut arr_buf, &mut bodies_buf, &mut ptrs);
                    }
                    ad::DataType::LargeUtf8 => {
                        let string = downcast::<aa::LargeStringArray>(array).value(row_idx);
                        write_str(string, &mut arr_buf, &mut bodies_buf, &mut ptrs);
                    }

                    _ => {
                        panic!("Unsupported Arrow data type: {dt}")
                    }
                };
            }
        }
    }

    // finalize pointers
    for (ptr, bodies_marker) in ptrs {
        let absolute_ptr = arr_buf.len() + bodies_marker;
        ptr.write(&mut arr_buf, absolute_ptr);
    }

    let mut result = bytes::BytesMut::new();
    result.put_u32_le(8); // offset
    result.put_u32_le(array_len as u32); // len
    result.put(arr_buf);
    result.put(bodies_buf);

    result
}

fn write_str(
    string: &str,
    arr_buf: &mut bytes::BytesMut,
    bodies_buf: &mut bytes::BytesMut,
    ptrs: &mut Vec<(lutra_bin::ReversePointer, usize)>,
) {
    // encode head
    let rptr = lutra_bin::ReversePointer::new(arr_buf);
    arr_buf.put_u32_le(string.len() as u32);

    // encode body
    let bodies_marker = bodies_buf.len();
    bodies_buf.put_slice(string.as_bytes());

    // store pointer so we can finalize it later
    ptrs.push((rptr, bodies_marker));
}
