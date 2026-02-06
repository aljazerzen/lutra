use crate::Error;
use arrow::array::RecordBatch;
use lutra_bin::ir;

/// Convert DuckDB Arrow results to Lutra binary format
pub fn from_arrow(
    arrow_iter: impl Iterator<Item = RecordBatch>,
    output_ty: &ir::Ty,
) -> Result<Vec<u8>, Error> {
    // Collect all batches
    let batches: Vec<RecordBatch> = arrow_iter.collect();

    // For now, handle the simple case: either a single row (tuple) or array of rows
    match &output_ty.kind {
        _ if output_ty.is_unit() => {
            // Unit type - no output
            Ok(Vec::new())
        }
        ir::TyKind::Array(_item_ty) => {
            // Array type - convert all rows to an array
            if batches.is_empty() {
                // Empty array result
                return Ok(vec![8, 0, 0, 0, 0, 0, 0, 0]); // [offset:u32=8][len:u32=0]
            }

            // Use lutra-arrow to convert the batches
            Ok(lutra_arrow::arrow_to_lutra(batches).to_vec())
        }
        _ => {
            // Single row - should be exactly 1 row across all batches
            if batches.is_empty() {
                return Err(Error::BadDatabaseResponse("expected at least one batch"));
            }

            let total_rows: usize = batches.iter().map(|b| b.num_rows()).sum();
            if total_rows != 1 {
                return Err(Error::BadDatabaseResponse(
                    "expected exactly one row for non-array output",
                ));
            }

            // Use lutra-arrow, which will create an array of 1 tuple
            let arr_bytes = lutra_arrow::arrow_to_lutra(batches);

            // Extract the single tuple from the array
            // Format: [offset:u32][len:u32][...tuple_data...]
            // We want just the tuple data, so skip the array header (8 bytes)
            if arr_bytes.len() < 8 {
                return Err(Error::BadDatabaseResponse(
                    "invalid arrow conversion result",
                ));
            }

            Ok(arr_bytes[8..].to_vec())
        }
    }
}
