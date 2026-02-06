use std::sync::Arc;

use arrow::array as aa;
use arrow::datatypes as ad;
use lutra_bin::Encode;
use lutra_bin::bytes::{self, BufMut};

/// Converts an Arrow reader into a Lutra buffer.
///
/// The Arrow schema is used to infer the structure and types.
/// Returns an array of tuples where each tuple field corresponds to an Arrow field.
pub fn arrow_to_lutra(batches: Vec<aa::RecordBatch>) -> bytes::BytesMut {
    let mut encoder = Encoder::new();

    let num_rows: usize = batches.iter().map(|b| b.num_rows()).sum();
    let num_cols = batches.first().map(|b| b.num_columns()).unwrap_or_default();

    // array head
    let arr_body_ptr = lutra_bin::ReversePointer::new(&mut encoder.arr_buf);
    encoder.arr_buf.put_u32_le(num_rows as u32);
    arr_body_ptr.write_cur_len(&mut encoder.arr_buf);

    // first pass: encode heads
    let mut residuals = Vec::with_capacity(num_rows * num_cols);
    for batch in &batches {
        for row_idx in 0..batch.num_rows() {
            for col_idx in 0..batch.num_columns() {
                let array = batch.column(col_idx).as_ref();
                residuals.push(encoder.write_head(array, row_idx));
            }
        }
    }

    // second pass: encode bodies
    let mut residuals = residuals.into_iter();
    for batch in batches {
        for row_idx in 0..batch.num_rows() {
            for col_idx in 0..batch.num_columns() {
                let array = batch.column(col_idx).as_ref();

                encoder.write_body(residuals.next().unwrap(), array, row_idx)
            }
        }
    }

    encoder.arr_buf
}

/// Residual information from writing a head that needs body processing
#[derive(Debug)]
enum HeadResidual {
    None,                           // Primitives - no body needed
    Ptr(lutra_bin::ReversePointer), // Text & List - pointer to finalize
    Struct(Vec<HeadResidual>),      // Struct - collect residuals from fields
}

/// Encoder for converting Arrow data to Lutra binary format
struct Encoder {
    /// Buffer for array data
    arr_buf: bytes::BytesMut,
}

impl Encoder {
    fn new() -> Self {
        Self {
            arr_buf: bytes::BytesMut::new(),
        }
    }

    /// Write the head of an element from an Arrow array
    ///
    /// Returns a HeadResidual that indicates what body processing is needed.
    ///
    /// # Parameters
    /// - `array`: The Arrow array to read from
    /// - `idx`: The index of the array item to read
    fn write_head(&mut self, array: &dyn aa::Array, idx: usize) -> HeadResidual {
        fn downcast<T: 'static>(array: &dyn std::any::Any) -> &T {
            array.downcast_ref::<T>().unwrap()
        }

        match array.data_type() {
            // Primitives - encode directly, no body needed
            ad::DataType::Boolean => {
                downcast::<aa::BooleanArray>(array.as_any())
                    .value(idx)
                    .encode_head(&mut self.arr_buf);
                HeadResidual::None
            }
            ad::DataType::Int8 => {
                downcast::<aa::Int8Array>(array.as_any())
                    .value(idx)
                    .encode_head(&mut self.arr_buf);
                HeadResidual::None
            }
            ad::DataType::Int16 => {
                downcast::<aa::Int16Array>(array.as_any())
                    .value(idx)
                    .encode_head(&mut self.arr_buf);
                HeadResidual::None
            }
            ad::DataType::Int32 => {
                downcast::<aa::Int32Array>(array.as_any())
                    .value(idx)
                    .encode_head(&mut self.arr_buf);
                HeadResidual::None
            }
            ad::DataType::Date32 => {
                downcast::<aa::Date32Array>(array.as_any())
                    .value(idx)
                    .encode_head(&mut self.arr_buf);
                HeadResidual::None
            }
            ad::DataType::Int64 => {
                downcast::<aa::Int64Array>(array.as_any())
                    .value(idx)
                    .encode_head(&mut self.arr_buf);
                HeadResidual::None
            }
            ad::DataType::Timestamp(ad::TimeUnit::Microsecond, _) => {
                downcast::<aa::TimestampMicrosecondArray>(array.as_any())
                    .value(idx)
                    .encode_head(&mut self.arr_buf);
                HeadResidual::None
            }
            ad::DataType::UInt8 => {
                downcast::<aa::UInt8Array>(array.as_any())
                    .value(idx)
                    .encode_head(&mut self.arr_buf);
                HeadResidual::None
            }
            ad::DataType::UInt16 => {
                downcast::<aa::UInt16Array>(array.as_any())
                    .value(idx)
                    .encode_head(&mut self.arr_buf);
                HeadResidual::None
            }
            ad::DataType::UInt32 => {
                downcast::<aa::UInt32Array>(array.as_any())
                    .value(idx)
                    .encode_head(&mut self.arr_buf);
                HeadResidual::None
            }
            ad::DataType::UInt64 => {
                downcast::<aa::UInt64Array>(array.as_any())
                    .value(idx)
                    .encode_head(&mut self.arr_buf);
                HeadResidual::None
            }
            ad::DataType::Float32 => {
                downcast::<aa::Float32Array>(array.as_any())
                    .value(idx)
                    .encode_head(&mut self.arr_buf);
                HeadResidual::None
            }
            ad::DataType::Float64 => {
                downcast::<aa::Float64Array>(array.as_any())
                    .value(idx)
                    .encode_head(&mut self.arr_buf);
                HeadResidual::None
            }

            // Text - write pointer + length, return pointer for body
            ad::DataType::Utf8 => {
                let string = downcast::<aa::StringArray>(array.as_any()).value(idx);
                let ptr = lutra_bin::ReversePointer::new(&mut self.arr_buf);
                self.arr_buf.put_u32_le(string.len() as u32);
                HeadResidual::Ptr(ptr)
            }
            ad::DataType::LargeUtf8 => {
                let string = downcast::<aa::LargeStringArray>(array.as_any()).value(idx);
                let ptr = lutra_bin::ReversePointer::new(&mut self.arr_buf);
                self.arr_buf.put_u32_le(string.len() as u32);
                HeadResidual::Ptr(ptr)
            }

            // List - write pointer + length, return pointer for body
            ad::DataType::List(_) => {
                let list = downcast::<aa::ListArray>(array.as_any()).value(idx);
                let len = list.len();
                let ptr = lutra_bin::ReversePointer::new(&mut self.arr_buf);
                self.arr_buf.put_u32_le(len as u32);
                HeadResidual::Ptr(ptr)
            }
            ad::DataType::LargeList(_) => {
                let list = downcast::<aa::LargeListArray>(array.as_any()).value(idx);
                let len = list.len();
                let ptr = lutra_bin::ReversePointer::new(&mut self.arr_buf);
                self.arr_buf.put_u32_le(len as u32);
                HeadResidual::Ptr(ptr)
            }

            // Struct - recursively write heads for each field
            ad::DataType::Struct(_) => {
                let struct_arr = downcast::<aa::StructArray>(array.as_any());
                let mut residuals = Vec::new();
                for col_idx in 0..struct_arr.num_columns() {
                    let field = struct_arr.column(col_idx);
                    residuals.push(self.write_head(field.as_ref(), idx));
                }
                HeadResidual::Struct(residuals)
            }

            _ => panic!("Unsupported Arrow data type: {:?}", array.data_type()),
        }
    }

    /// Write the body of an element based on its head residual
    ///
    /// # Parameters
    /// - `residual`: The residual from write_head
    /// - `array`: The original Arrow array (to re-extract data)
    /// - `idx`: The index of the array item
    fn write_body(&mut self, residual: HeadResidual, array: &dyn aa::Array, idx: usize) {
        fn downcast<T: 'static>(array: &dyn std::any::Any) -> &T {
            array.downcast_ref::<T>().unwrap()
        }

        match residual {
            HeadResidual::None => {
                // Primitives have no body
            }

            HeadResidual::Ptr(ptr) => {
                // Finalize pointer to current position
                ptr.write_cur_len(&mut self.arr_buf);

                // Re-extract data based on array type and write body
                match array.data_type() {
                    ad::DataType::Utf8 => {
                        let string = downcast::<aa::StringArray>(array.as_any()).value(idx);
                        self.arr_buf.put_slice(string.as_bytes());
                    }
                    ad::DataType::LargeUtf8 => {
                        let string = downcast::<aa::LargeStringArray>(array.as_any()).value(idx);
                        self.arr_buf.put_slice(string.as_bytes());
                    }
                    ad::DataType::List(_) => {
                        let values = downcast::<aa::ListArray>(array.as_any()).value(idx);
                        self.write_list_body(values);
                    }
                    ad::DataType::LargeList(_) => {
                        let values = downcast::<aa::LargeListArray>(array.as_any()).value(idx);
                        self.write_list_body(values);
                    }
                    _ => unreachable!("Ptr residual for non-text/list type"),
                }
            }

            HeadResidual::Struct(residuals) => {
                let struct_arr = downcast::<aa::StructArray>(array.as_any());
                for (col_idx, residual) in residuals.into_iter().enumerate() {
                    let field = struct_arr.column(col_idx);
                    self.write_body(residual, field.as_ref(), idx);
                }
            }
        }
    }

    /// Helper to write list body (all item heads, then all item bodies)
    fn write_list_body(&mut self, values: Arc<dyn aa::Array>) {
        let len = values.len();

        // Collect all item head residuals
        let mut item_residuals = Vec::with_capacity(len);
        for i in 0..len {
            item_residuals.push(self.write_head(values.as_ref(), i));
        }

        // Write all item bodies
        for (i, residual) in item_residuals.into_iter().enumerate() {
            self.write_body(residual, values.as_ref(), i);
        }
    }
}
