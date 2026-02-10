use std::collections::HashMap;
use std::sync::Arc;

use arrow::array as aa;
use arrow::datatypes as ad;
use lutra_bin::Encode;
use lutra_bin::bytes::{self, BufMut};
use lutra_bin::ir;
use thiserror::Error;

/// Converts Arrow RecordBatches to Lutra binary format.
///
/// Uses Lutra type information to:
/// - Determine output structure (array vs single value)
/// - Validate Arrow data matches expected types
/// - Wrap nullable values in option enum when declared
///
/// # Arguments
/// - `batches`: Arrow record batches from query result
/// - `ty`: Expected Lutra output type
/// - `ty_defs`: Type definitions for resolving type identifiers
pub fn arrow_to_lutra(
    batches: Vec<aa::RecordBatch>,
    ty: &ir::Ty,
    ty_defs: &[ir::TyDef],
) -> Result<bytes::BytesMut, Error> {
    let ctx = Context::new(ty_defs);
    let ty = ctx.get_ty_mat(ty)?;

    // Case 1: Unit type - return empty
    if ty.is_unit() {
        return Ok(bytes::BytesMut::new());
    }

    // Case 2: Array type
    if let ir::TyKind::Array(item_ty) = &ty.kind {
        return encode_array(&ctx, batches, item_ty);
    }

    // Case 3: Non-array type (single row expected)
    encode_single_value(&ctx, batches, ty)
}

/// Error type for Arrow to Lutra conversion
#[derive(Debug, Error)]
pub enum Error {
    #[error("type mismatch: Arrow has {arrow_ty}, but Lutra expects {lutra_ty}")]
    TypeMismatch { arrow_ty: String, lutra_ty: String },

    #[error("expected single row, got {got}")]
    ExpectedSingleRow { got: usize },

    #[error("expected {expected} columns, got {got}")]
    ColumnCountMismatch { expected: usize, got: usize },

    #[error("unexpected null value for non-option type")]
    UnexpectedNull,

    #[error("provided type is invalid")]
    BadType,
}

/// Context for resolving type identifiers
struct Context<'a> {
    types: HashMap<&'a ir::Path, &'a ir::Ty>,
}

impl<'a> Context<'a> {
    fn new(ty_defs: &'a [ir::TyDef]) -> Self {
        Context {
            types: ty_defs.iter().map(|def| (&def.name, &def.ty)).collect(),
        }
    }

    fn get_ty_mat(&self, ty: &'a ir::Ty) -> Result<&'a ir::Ty, Error> {
        let mut ty = ty;
        while let ir::TyKind::Ident(path) = &ty.kind {
            ty = self.types.get(path).ok_or(Error::BadType)?;
        }
        Ok(ty)
    }
}

/// Checks if enum is option pattern: enum {none, some: T} where T is primitive or array.
/// These can be represented as nullable values (single column).
fn is_option(variants: &[ir::TyEnumVariant]) -> bool {
    if variants.len() != 2 || !variants[0].ty.is_unit() {
        return false;
    }
    let some_ty = &variants[1].ty.kind;
    some_ty.is_primitive() || some_ty.is_array()
}

/// Encode an array of values
fn encode_array(
    ctx: &Context,
    batches: Vec<aa::RecordBatch>,
    item_ty: &ir::Ty,
) -> Result<bytes::BytesMut, Error> {
    let num_rows: usize = batches.iter().map(|b| b.num_rows()).sum();

    // Handle empty array
    if num_rows == 0 {
        return Ok(empty_array_bytes());
    }

    // Validate column count
    let item_ty = ctx.get_ty_mat(item_ty)?;
    let expected_cols = expected_column_count(item_ty);
    validate_column_count(&batches, expected_cols)?;

    // Encode array with all rows
    let mut encoder = Encoder::new(ctx);
    encoder.encode_array(&batches, item_ty, num_rows)
}

/// Encode a single value (non-array output)
fn encode_single_value(
    ctx: &Context,
    batches: Vec<aa::RecordBatch>,
    ty: &ir::Ty,
) -> Result<bytes::BytesMut, Error> {
    // Validate exactly 1 row total
    let total_rows: usize = batches.iter().map(|b| b.num_rows()).sum();
    if total_rows != 1 {
        return Err(Error::ExpectedSingleRow { got: total_rows });
    }

    // Validate column count
    let ty = ctx.get_ty_mat(ty)?;
    let expected_cols = expected_column_count(ty);
    validate_column_count(&batches, expected_cols)?;

    // Find the batch with the row
    let batch = batches.iter().find(|b| b.num_rows() > 0).unwrap();

    // Encode single row (no array wrapper)
    let mut encoder = Encoder::new(ctx);
    encoder.encode_single_row(batch, 0, ty)
}

/// Returns expected column count for a type.
/// With DuckDB arrow repr, nested tuples are STRUCTs (single column), not flattened.
fn expected_column_count(ty: &ir::Ty) -> usize {
    match &ty.kind {
        ir::TyKind::Tuple(fields) => fields.len(),
        _ => 1, // Primitives, arrays, enums: single column
    }
}

/// Validate that all batches have the expected column count
fn validate_column_count(batches: &[aa::RecordBatch], expected: usize) -> Result<(), Error> {
    for batch in batches {
        if batch.num_columns() != expected {
            return Err(Error::ColumnCountMismatch {
                expected,
                got: batch.num_columns(),
            });
        }
    }
    Ok(())
}

/// Returns bytes for an empty array: [offset:u32=8][len:u32=0]
fn empty_array_bytes() -> bytes::BytesMut {
    let mut buf = bytes::BytesMut::new();
    buf.put_u32_le(8); // offset points past header
    buf.put_u32_le(0); // length = 0
    buf
}

/// Validate that Arrow type can represent Lutra type
fn validate_type_match(
    arrow_ty: &ad::DataType,
    lutra_ty: &ir::Ty,
    ctx: &Context,
) -> Result<(), Error> {
    let lutra_ty = ctx.get_ty_mat(lutra_ty)?;

    // option types: validation happens on inner type when we recurse
    if let ir::TyKind::Enum(variants) = &lutra_ty.kind
        && is_option(variants)
    {
        return Ok(());
    }

    let ok = match (arrow_ty, &lutra_ty.kind) {
        // Primitives
        (ad::DataType::Boolean, ir::TyKind::Primitive(ir::TyPrimitive::bool)) => true,
        (ad::DataType::Int8, ir::TyKind::Primitive(ir::TyPrimitive::int8)) => true,
        (ad::DataType::Int16, ir::TyKind::Primitive(ir::TyPrimitive::int16)) => true,
        (ad::DataType::Int32, ir::TyKind::Primitive(ir::TyPrimitive::int32)) => true,
        (ad::DataType::Date32, ir::TyKind::Primitive(ir::TyPrimitive::int32)) => true,
        (ad::DataType::Int64, ir::TyKind::Primitive(ir::TyPrimitive::int64)) => true,
        (ad::DataType::Timestamp(_, _), ir::TyKind::Primitive(ir::TyPrimitive::int64)) => true,
        (ad::DataType::UInt8, ir::TyKind::Primitive(ir::TyPrimitive::uint8)) => true,
        (ad::DataType::UInt16, ir::TyKind::Primitive(ir::TyPrimitive::uint16)) => true,
        (ad::DataType::UInt32, ir::TyKind::Primitive(ir::TyPrimitive::uint32)) => true,
        (ad::DataType::UInt64, ir::TyKind::Primitive(ir::TyPrimitive::uint64)) => true,
        (ad::DataType::Float32, ir::TyKind::Primitive(ir::TyPrimitive::float32)) => true,
        (ad::DataType::Float64, ir::TyKind::Primitive(ir::TyPrimitive::float64)) => true,
        (
            ad::DataType::Utf8 | ad::DataType::LargeUtf8,
            ir::TyKind::Primitive(ir::TyPrimitive::text),
        ) => true,

        // Composite types
        (ad::DataType::Struct(arrow_fields), ir::TyKind::Tuple(lutra_fields)) => {
            arrow_fields.len() == lutra_fields.len()
        }
        (ad::DataType::List(_) | ad::DataType::LargeList(_), ir::TyKind::Array(_)) => true,
        (ad::DataType::Union(arrow_fields, _), ir::TyKind::Enum(lutra_variants)) => {
            arrow_fields.len() == lutra_variants.len()
        }

        _ => false,
    };

    if ok {
        Ok(())
    } else {
        Err(Error::TypeMismatch {
            arrow_ty: format!("{:?}", arrow_ty),
            lutra_ty: format!("{:?}", lutra_ty.kind),
        })
    }
}

/// Residual information from writing a head that needs body processing
#[derive(Debug)]
enum HeadResidual {
    None,                                  // Primitives - no body needed
    Ptr(lutra_bin::ReversePointer),        // Text & List - pointer to finalize
    Struct(Vec<HeadResidual>),             // Struct - collect residuals from fields
    OptionSome(lutra_bin::ReversePointer), // Option some with pointer (body written later)
    Enum {
        // General enum - tag already written, variant residual stored
        tag: usize,
        variant_residual: Box<HeadResidual>,
        has_ptr: bool,
        ptr: Option<lutra_bin::ReversePointer>,
    },
}

/// Encoder for converting Arrow data to Lutra binary format
struct Encoder<'a> {
    arr_buf: bytes::BytesMut,
    ctx: &'a Context<'a>,
}

impl<'a> Encoder<'a> {
    fn new(ctx: &'a Context<'a>) -> Self {
        Self {
            arr_buf: bytes::BytesMut::new(),
            ctx,
        }
    }

    /// Encode an array of rows
    fn encode_array(
        &mut self,
        batches: &[aa::RecordBatch],
        item_ty: &ir::Ty,
        num_rows: usize,
    ) -> Result<bytes::BytesMut, Error> {
        // Array head: [pointer][length]
        let arr_body_ptr = lutra_bin::ReversePointer::new(&mut self.arr_buf);
        self.arr_buf.put_u32_le(num_rows as u32);
        arr_body_ptr.write_cur_len(&mut self.arr_buf);

        // First pass: encode all heads
        let mut residuals = Vec::with_capacity(num_rows);
        for batch in batches {
            for row_idx in 0..batch.num_rows() {
                residuals.push(self.write_row_head(batch, row_idx, item_ty)?);
            }
        }

        // Second pass: encode all bodies
        let mut residuals = residuals.into_iter();
        for batch in batches {
            for row_idx in 0..batch.num_rows() {
                self.write_row_body(residuals.next().unwrap(), batch, row_idx, item_ty)?;
            }
        }

        Ok(std::mem::take(&mut self.arr_buf))
    }

    /// Encode a single row (no array wrapper)
    fn encode_single_row(
        &mut self,
        batch: &aa::RecordBatch,
        row_idx: usize,
        ty: &ir::Ty,
    ) -> Result<bytes::BytesMut, Error> {
        let residual = self.write_row_head(batch, row_idx, ty)?;
        self.write_row_body(residual, batch, row_idx, ty)?;
        Ok(std::mem::take(&mut self.arr_buf))
    }

    /// Write head for a row (may be tuple or single value)
    /// With DuckDB arrow repr, each tuple field is one column (may be STRUCT for nested tuples)
    fn write_row_head(
        &mut self,
        batch: &aa::RecordBatch,
        row_idx: usize,
        ty: &ir::Ty,
    ) -> Result<HeadResidual, Error> {
        let ty = self.ctx.get_ty_mat(ty)?;

        match &ty.kind {
            ir::TyKind::Tuple(fields) => {
                // Each field is one column (could be STRUCT for nested tuples)
                let mut residuals = Vec::with_capacity(fields.len());
                for (col_idx, field) in fields.iter().enumerate() {
                    let array = batch.column(col_idx).as_ref();
                    residuals.push(self.write_head(array, row_idx, &field.ty)?);
                }
                Ok(HeadResidual::Struct(residuals))
            }
            _ => {
                // Single column result
                let array = batch.column(0).as_ref();
                self.write_head(array, row_idx, ty)
            }
        }
    }

    /// Write body for a row
    fn write_row_body(
        &mut self,
        residual: HeadResidual,
        batch: &aa::RecordBatch,
        row_idx: usize,
        ty: &ir::Ty,
    ) -> Result<(), Error> {
        let ty = self.ctx.get_ty_mat(ty)?;

        match &ty.kind {
            ir::TyKind::Tuple(fields) => {
                let HeadResidual::Struct(residuals) = residual else {
                    unreachable!()
                };
                for (col_idx, (field, res)) in fields.iter().zip(residuals).enumerate() {
                    let array = batch.column(col_idx).as_ref();
                    self.write_body(res, array, row_idx, &field.ty)?;
                }
                Ok(())
            }
            _ => {
                let array = batch.column(0).as_ref();
                self.write_body(residual, array, row_idx, ty)
            }
        }
    }

    /// Write the head of an element from an Arrow array
    fn write_head(
        &mut self,
        array: &dyn aa::Array,
        idx: usize,
        ty: &ir::Ty,
    ) -> Result<HeadResidual, Error> {
        let ty = self.ctx.get_ty_mat(ty)?;

        // Handle option enum: {none, some: T}
        if let ir::TyKind::Enum(variants) = &ty.kind
            && is_option(variants)
        {
            let inner_ty = &variants[1].ty;
            let head_format = lutra_bin::layout::enum_head_format(variants, &ty.variants_recursive);

            if array.is_null(idx) {
                return Ok(self.encode_option_none(&head_format));
            } else {
                // Write .some tag
                self.arr_buf.put_u8(1);

                if head_format.has_ptr {
                    // For pointer-style enums, write a pointer that will be filled later
                    // The entire inner value (head + body) will be written in write_body
                    let ptr = lutra_bin::ReversePointer::new(&mut self.arr_buf);
                    return Ok(HeadResidual::OptionSome(ptr));
                } else {
                    // For inline-style enums, write the inner value directly
                    return self.write_head(array, idx, inner_ty);
                }
            }
        }

        // Non-option null is an error
        if array.is_null(idx) {
            return Err(Error::UnexpectedNull);
        }

        // Validate type match
        validate_type_match(array.data_type(), ty, self.ctx)?;

        fn downcast<T: 'static>(array: &dyn std::any::Any) -> &T {
            array.downcast_ref::<T>().unwrap()
        }

        let residual = match array.data_type() {
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
                let ir::TyKind::Tuple(fields) = &ty.kind else {
                    unreachable!("validated above")
                };
                let struct_arr = downcast::<aa::StructArray>(array.as_any());
                let mut residuals = Vec::new();
                for (col_idx, field) in fields.iter().enumerate() {
                    let field_arr = struct_arr.column(col_idx);
                    residuals.push(self.write_head(field_arr.as_ref(), idx, &field.ty)?);
                }
                HeadResidual::Struct(residuals)
            }

            // Union - general enum type
            ad::DataType::Union(_fields, _mode) => {
                let ir::TyKind::Enum(variants) = &ty.kind else {
                    unreachable!("validated above")
                };
                let union_arr = downcast::<aa::UnionArray>(array.as_any());

                // Get the tag (type_id) for this row
                let tag = union_arr.type_id(idx) as usize;
                let variant = &variants[tag];

                // Get enum format for layout info
                let head_format =
                    lutra_bin::layout::enum_head_format(variants, &ty.variants_recursive);

                // Write tag byte
                self.arr_buf.put_u8(tag as u8);

                if head_format.has_ptr {
                    // Pointer-style enum: write pointer, defer inner to body
                    if variant.ty.is_unit() {
                        // Unit variant: just padding (no pointer needed)
                        self.arr_buf.put_bytes(0, head_format.inner_bytes as usize);
                        HeadResidual::Enum {
                            tag,
                            variant_residual: Box::new(HeadResidual::None),
                            has_ptr: true,
                            ptr: None,
                        }
                    } else {
                        // Non-unit variant: write pointer
                        let ptr = lutra_bin::ReversePointer::new(&mut self.arr_buf);
                        HeadResidual::Enum {
                            tag,
                            variant_residual: Box::new(HeadResidual::None),
                            has_ptr: true,
                            ptr: Some(ptr),
                        }
                    }
                } else {
                    // Inline-style enum: write variant head inline
                    let variant_arr = union_arr.child(tag as i8);
                    // For sparse unions, use the same index; for dense, we'd need offset
                    let variant_idx = idx;

                    let variant_residual = if variant.ty.is_unit() {
                        HeadResidual::None
                    } else {
                        self.write_head(variant_arr.as_ref(), variant_idx, &variant.ty)?
                    };

                    // Add padding to match largest variant
                    let variant_format =
                        lutra_bin::layout::enum_variant_format(&head_format, &variant.ty);
                    if variant_format.padding_bytes > 0 {
                        self.arr_buf
                            .put_bytes(0, variant_format.padding_bytes as usize);
                    }

                    HeadResidual::Enum {
                        tag,
                        variant_residual: Box::new(variant_residual),
                        has_ptr: false,
                        ptr: None,
                    }
                }
            }

            _ => panic!("Unsupported Arrow data type: {:?}", array.data_type()),
        };

        Ok(residual)
    }

    /// Write the body of an element based on its head residual
    fn write_body(
        &mut self,
        residual: HeadResidual,
        array: &dyn aa::Array,
        idx: usize,
        ty: &ir::Ty,
    ) -> Result<(), Error> {
        let ty = self.ctx.get_ty_mat(ty)?;

        // Handle option: if null, head already encoded .none with no body
        if let ir::TyKind::Enum(variants) = &ty.kind
            && is_option(variants)
        {
            if array.is_null(idx) {
                return Ok(());
            }
            // For non-null, the residual tells us how to proceed
            // - HeadResidual::OptionSome for pointer-style (inner head+body written here)
            // - Other residuals for inline-style (inner head already written)
            match residual {
                HeadResidual::OptionSome(ptr) => {
                    // Finalize pointer to current position
                    ptr.write_cur_len(&mut self.arr_buf);
                    // Write the full inner value (head + body) at current position
                    let inner_ty = &variants[1].ty;
                    let inner_residual = self.write_head(array, idx, inner_ty)?;
                    return self.write_body(inner_residual, array, idx, inner_ty);
                }
                _ => {
                    // Inline-style: recurse with inner type
                    return self.write_body(residual, array, idx, &variants[1].ty);
                }
            }
        }

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
                        let ir::TyKind::Array(item_ty) = &ty.kind else {
                            unreachable!()
                        };
                        let values = downcast::<aa::ListArray>(array.as_any()).value(idx);
                        self.write_list_body(values, item_ty)?;
                    }
                    ad::DataType::LargeList(_) => {
                        let ir::TyKind::Array(item_ty) = &ty.kind else {
                            unreachable!()
                        };
                        let values = downcast::<aa::LargeListArray>(array.as_any()).value(idx);
                        self.write_list_body(values, item_ty)?;
                    }
                    _ => unreachable!("Ptr residual for non-text/list type"),
                }
            }

            HeadResidual::Struct(residuals) => {
                let ir::TyKind::Tuple(fields) = &ty.kind else {
                    unreachable!()
                };
                let struct_arr = downcast::<aa::StructArray>(array.as_any());
                for (col_idx, (field, res)) in fields.iter().zip(residuals).enumerate() {
                    let field_arr = struct_arr.column(col_idx);
                    self.write_body(res, field_arr.as_ref(), idx, &field.ty)?;
                }
            }

            HeadResidual::OptionSome(_) => {
                // This should be handled above in the option enum branch
                unreachable!("OptionSome residual should be handled in option enum case")
            }

            HeadResidual::Enum {
                tag,
                variant_residual,
                has_ptr,
                ptr,
            } => {
                let ir::TyKind::Enum(variants) = &ty.kind else {
                    unreachable!()
                };
                let variant = &variants[tag];
                let union_arr = downcast::<aa::UnionArray>(array.as_any());

                if has_ptr {
                    // Pointer-style: write full inner value at current position
                    if let Some(ptr) = ptr {
                        ptr.write_cur_len(&mut self.arr_buf);
                        // Write the variant's head and body
                        let variant_arr = union_arr.child(tag as i8);
                        let inner_residual =
                            self.write_head(variant_arr.as_ref(), idx, &variant.ty)?;
                        self.write_body(inner_residual, variant_arr.as_ref(), idx, &variant.ty)?;
                    }
                    // If ptr is None, it's a unit variant - nothing more to write
                } else {
                    // Inline-style: recurse with variant residual
                    let variant_arr = union_arr.child(tag as i8);
                    self.write_body(*variant_residual, variant_arr.as_ref(), idx, &variant.ty)?;
                }
            }
        }

        Ok(())
    }

    /// Helper to write list body (all item heads, then all item bodies)
    fn write_list_body(
        &mut self,
        values: Arc<dyn aa::Array>,
        item_ty: &ir::Ty,
    ) -> Result<(), Error> {
        let len = values.len();

        // Collect all item head residuals
        let mut item_residuals = Vec::with_capacity(len);
        for i in 0..len {
            item_residuals.push(self.write_head(values.as_ref(), i, item_ty)?);
        }

        // Write all item bodies
        for (i, residual) in item_residuals.into_iter().enumerate() {
            self.write_body(residual, values.as_ref(), i, item_ty)?;
        }

        Ok(())
    }

    /// Encode option none variant: [tag=0][padding]
    fn encode_option_none(
        &mut self,
        head_format: &lutra_bin::layout::EnumHeadFormat,
    ) -> HeadResidual {
        // Tag = 0 for none
        self.arr_buf.put_u8(0);

        // Padding: inner_bytes from the enum head format
        self.arr_buf.put_bytes(0, head_format.inner_bytes as usize);

        HeadResidual::None
    }
}
