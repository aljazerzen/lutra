use std::any::Any;
use std::borrow::Cow;
use std::sync::Arc;

use arrow::array as aa;
use arrow::datatypes::{DataType, Field, Schema, SchemaRef};

use lutra_bin::{ArrayReader, bytes, ir};

use crate::context::{Context, Error};

fn decode_prim<T: lutra_bin::Decode>(data: impl bytes::Buf) -> T {
    T::decode(data.chunk()).unwrap()
}

/// Converts Lutra binary data to Arrow RecordBatch representation.
///
/// # Returns
/// - For array types: RecordBatch with N rows (one per array item)
/// - For tuple types: RecordBatch with 1 row and M columns
/// - For primitive types: RecordBatch with 1 row and 1 column
///
/// # Errors
/// - `UnsupportedType` if type contains enums/functions or composite types in fields
pub fn lutra_to_arrow(
    data: impl bytes::Buf + Clone,
    ty: &ir::Ty,
    ty_defs: &[ir::TyDef],
) -> Result<aa::RecordBatch, Error> {
    let ctx = Context::new(ty_defs);
    let ty = ctx.get_ty_mat(ty)?;

    let schema = get_schema(ty, &ctx)?;
    let mut writer = ArrowRowWriter::new(schema, 0);

    match &ty.kind {
        ir::TyKind::Array(ty_item) => {
            let array_reader = lutra_bin::ArrayReader::new_for_ty(data, ty);
            writer.prepare_for_batch(array_reader.remaining())?;

            for item in array_reader {
                encode_row(&mut writer, item, ty_item, &ctx)?;
            }
        }
        ir::TyKind::Primitive(_) | ir::TyKind::Tuple(_) => {
            writer.prepare_for_batch(1)?;
            encode_row(&mut writer, data, ty, &ctx)?;
        }
        ir::TyKind::Enum(_) => {
            return Err(Error::UnsupportedType(
                "enum types not yet supported in lutra_to_arrow".into(),
            ));
        }
        ir::TyKind::Function(_) => {
            return Err(Error::UnsupportedType(
                "cannot serialize functions to Arrow".into(),
            ));
        }
        ir::TyKind::Ident(_) => {
            unreachable!("should have been resolved by get_ty_mat")
        }
    }

    let batches = writer.finish()?;
    assert_eq!(batches.len(), 1);
    Ok(batches.into_iter().next().unwrap())
}

/// Encode a non-top-level value as a RecordBatch
fn encode_row(
    writer: &mut ArrowRowWriter,
    data: impl bytes::Buf + Clone,
    ty: &ir::Ty,
    ctx: &Context,
) -> Result<(), Error> {
    let ty = ctx.get_ty_mat(ty)?;
    match &ty.kind {
        ir::TyKind::Array(_) => Err(Error::UnsupportedType("nested arrays not supported".into())),
        ir::TyKind::Tuple(ty_fields) => {
            let field_offsets = lutra_bin::layout::tuple_field_offsets(ty);
            let tuple_reader = lutra_bin::TupleReader::new(data, Cow::Owned(field_offsets));
            for (i, ty_field) in ty_fields.iter().enumerate() {
                encode_value(writer, tuple_reader.get_field(i), &ty_field.ty, ctx)?;
            }
            Ok(())
        }

        ir::TyKind::Primitive(_) => encode_value(writer, data, ty, ctx),

        ir::TyKind::Enum(_) => Err(Error::UnsupportedType(
            "enum types not yet supported".into(),
        )),
        ir::TyKind::Function(_) => Err(Error::UnsupportedType(
            "cannot serialize functions to Arrow".into(),
        )),
        ir::TyKind::Ident(_) => {
            unreachable!("should have been resolved by get_ty_mat")
        }
    }
}

/// Decode and write a single array item to the builder
fn encode_value(
    writer: &mut ArrowRowWriter,
    data: impl bytes::Buf,
    ty: &ir::Ty,
    ctx: &Context,
) -> Result<(), Error> {
    let ty = ctx.get_ty_mat(ty)?;
    match &ty.kind {
        ir::TyKind::Primitive(ir::TyPrimitive::bool) => {
            let v = decode_prim::<bool>(data);
            writer.next_as::<aa::BooleanBuilder>().append_value(v);
            Ok(())
        }
        ir::TyKind::Primitive(ir::TyPrimitive::int8) => {
            let v = decode_prim::<i8>(data);
            writer.next_as::<aa::Int8Builder>().append_value(v);
            Ok(())
        }
        ir::TyKind::Primitive(ir::TyPrimitive::int16) => {
            let v = decode_prim::<i16>(data);
            writer.next_as::<aa::Int16Builder>().append_value(v);
            Ok(())
        }
        ir::TyKind::Primitive(ir::TyPrimitive::int32) => {
            let v = decode_prim::<i32>(data);
            writer.next_as::<aa::Int32Builder>().append_value(v);
            Ok(())
        }
        ir::TyKind::Primitive(ir::TyPrimitive::int64) => {
            let v = decode_prim::<i64>(data);
            writer.next_as::<aa::Int64Builder>().append_value(v);
            Ok(())
        }
        ir::TyKind::Primitive(ir::TyPrimitive::uint8) => {
            let v = decode_prim::<u8>(data);
            writer.next_as::<aa::UInt8Builder>().append_value(v);
            Ok(())
        }
        ir::TyKind::Primitive(ir::TyPrimitive::uint16) => {
            let v = decode_prim::<u16>(data);
            writer.next_as::<aa::UInt16Builder>().append_value(v);
            Ok(())
        }
        ir::TyKind::Primitive(ir::TyPrimitive::uint32) => {
            let v = decode_prim::<u32>(data);
            writer.next_as::<aa::UInt32Builder>().append_value(v);
            Ok(())
        }
        ir::TyKind::Primitive(ir::TyPrimitive::uint64) => {
            let v = decode_prim::<u64>(data);
            writer.next_as::<aa::UInt64Builder>().append_value(v);
            Ok(())
        }
        ir::TyKind::Primitive(ir::TyPrimitive::float32) => {
            let v = decode_prim::<f32>(data);
            writer.next_as::<aa::Float32Builder>().append_value(v);
            Ok(())
        }
        ir::TyKind::Primitive(ir::TyPrimitive::float64) => {
            let v = decode_prim::<f64>(data);
            writer.next_as::<aa::Float64Builder>().append_value(v);
            Ok(())
        }
        ir::TyKind::Primitive(ir::TyPrimitive::text) => {
            // TODO: this copies content twice, but it could copy it only once

            let mut d = data;
            let (offset, len) = ArrayReader::<&[u8]>::read_head(d.chunk());
            d.advance(offset);
            let mut bytes = vec![0; len];
            d.copy_to_slice(&mut bytes);

            let s = String::from_utf8(bytes).unwrap();
            writer.next_as::<aa::StringBuilder>().append_value(s);
            Ok(())
        }
        ir::TyKind::Tuple(_) => Err(Error::UnsupportedType(
            "nested tuple types not yet supported".into(),
        )),
        ir::TyKind::Array(_) => Err(Error::UnsupportedType(
            "nested array types not yet supported".into(),
        )),
        ir::TyKind::Enum(_) => Err(Error::UnsupportedType(
            "enum types not yet supported".into(),
        )),
        ir::TyKind::Function(_) => Err(Error::UnsupportedType(
            "cannot serialize functions to Arrow".into(),
        )),
        ir::TyKind::Ident(_) => unreachable!(),
    }
}

/// Get arrow schema for a lutra type
fn get_schema(ty: &ir::Ty, ctx: &Context) -> Result<SchemaRef, Error> {
    let ty = ctx.get_ty_mat(ty)?;

    let fields = match &ty.kind {
        ir::TyKind::Array(ty_item) => get_schema_row(ty_item, ctx)?,
        ir::TyKind::Tuple(_) | ir::TyKind::Primitive(_) => get_schema_row(ty, ctx)?,

        ir::TyKind::Enum(_) => {
            return Err(Error::UnsupportedType("enum types not supported".into()));
        }
        ir::TyKind::Function(_) => {
            return Err(Error::UnsupportedType(
                "cannot serialize functions to Arrow".into(),
            ));
        }
        ir::TyKind::Ident(_) => unreachable!(),
    };
    Ok(Arc::new(Schema::new(fields)))
}

/// Get arrow schema for a lutra type
fn get_schema_row(ty: &ir::Ty, ctx: &Context) -> Result<Vec<Field>, Error> {
    let ty = ctx.get_ty_mat(ty)?;

    match &ty.kind {
        ir::TyKind::Array(_) => Err(Error::UnsupportedType(
            "nested array types not supported".into(),
        )),
        ir::TyKind::Tuple(ty_fields) => {
            let mut fields = Vec::new();
            for (i, ty_field) in ty_fields.iter().enumerate() {
                let name = ty_field.name.clone().unwrap_or_else(|| format!("field{i}"));

                let data_type = get_schema_cell(&ty_field.ty, ctx)?;
                fields.push(Field::new(name, data_type, false));
            }
            Ok(fields)
        }
        ir::TyKind::Primitive(_) => {
            // Single column for non-tuple types
            let field = Field::new("value", get_schema_cell(ty, ctx)?, false);
            Ok(vec![field])
        }
        ir::TyKind::Enum(_) => Err(Error::UnsupportedType("enum types not supported".into())),
        ir::TyKind::Function(_) => Err(Error::UnsupportedType(
            "cannot serialize functions to Arrow".into(),
        )),
        ir::TyKind::Ident(_) => unreachable!(),
    }
}

fn get_schema_cell(ty: &ir::Ty, ctx: &Context) -> Result<DataType, Error> {
    let ty = ctx.get_ty_mat(ty)?;

    match &ty.kind {
        // Primitives
        ir::TyKind::Primitive(ir::TyPrimitive::bool) => Ok(DataType::Boolean),
        ir::TyKind::Primitive(ir::TyPrimitive::int8) => Ok(DataType::Int8),
        ir::TyKind::Primitive(ir::TyPrimitive::int16) => Ok(DataType::Int16),
        ir::TyKind::Primitive(ir::TyPrimitive::int32) => Ok(DataType::Int32),
        ir::TyKind::Primitive(ir::TyPrimitive::int64) => Ok(DataType::Int64),
        ir::TyKind::Primitive(ir::TyPrimitive::uint8) => Ok(DataType::UInt8),
        ir::TyKind::Primitive(ir::TyPrimitive::uint16) => Ok(DataType::UInt16),
        ir::TyKind::Primitive(ir::TyPrimitive::uint32) => Ok(DataType::UInt32),
        ir::TyKind::Primitive(ir::TyPrimitive::uint64) => Ok(DataType::UInt64),
        ir::TyKind::Primitive(ir::TyPrimitive::float32) => Ok(DataType::Float32),
        ir::TyKind::Primitive(ir::TyPrimitive::float64) => Ok(DataType::Float64),
        ir::TyKind::Primitive(ir::TyPrimitive::text) => Ok(DataType::Utf8),

        // Arrays
        ir::TyKind::Array(_) => Err(Error::UnsupportedType(
            "nested arrays not supported".to_string(),
        )),

        // Nested Tuples
        ir::TyKind::Tuple(_) => Err(Error::UnsupportedType(
            "nested tuples not supported".to_string(),
        )),

        // Enums
        ir::TyKind::Enum(_) => Err(Error::UnsupportedType(
            "enum types not supported".to_string(),
        )),

        ir::TyKind::Function(_) => Err(Error::UnsupportedType(
            "cannot serialize functions to Arrow".into(),
        )),

        ir::TyKind::Ident(_) => {
            unreachable!("should have been resolved by get_ty_mat")
        }
    }
}

/// Receives values row-by-row and passes them to [ArrayBuilder]s,
/// which construct [RecordBatch]es.
///
/// Copied from connector_arrow crate
pub struct ArrowRowWriter {
    schema: SchemaRef,
    min_batch_size: usize,
    data: Vec<aa::RecordBatch>,

    /// Determines into which column the next stream value should go.
    receiver: Organizer,

    /// Array buffers.
    builders: Option<Vec<Box<dyn aa::ArrayBuilder>>>,
    /// Number of rows reserved to be written in by [ArrowPartitionWriter::prepare_for_batch]
    rows_reserved: usize,
    /// Number of rows allocated within builders.
    rows_capacity: usize,
}

impl ArrowRowWriter {
    pub fn new(schema: SchemaRef, min_batch_size: usize) -> Self {
        ArrowRowWriter {
            receiver: Organizer::new(schema.fields().len()),
            data: Vec::new(),

            builders: None,
            rows_reserved: 0,
            rows_capacity: 0,

            schema,
            min_batch_size,
        }
    }

    pub fn prepare_for_batch(&mut self, row_count: usize) -> Result<(), arrow::error::ArrowError> {
        self.receiver.reset_for_batch(row_count);
        self.allocate(row_count)?;
        Ok(())
    }

    /// Make sure that there is enough memory allocated in builders for the incoming batch.
    /// Might allocate more than needed, for future row reservations.
    fn allocate(&mut self, row_count: usize) -> Result<(), arrow::error::ArrowError> {
        if self.rows_capacity >= row_count + self.rows_reserved {
            // there is enough capacity, no need to allocate
            self.rows_reserved += row_count;
            return Ok(());
        }

        if self.rows_reserved > 0 {
            self.flush()?;
        }

        let to_allocate = usize::max(row_count, self.min_batch_size);

        let builders: Vec<Box<dyn aa::ArrayBuilder>> = self
            .schema
            .fields
            .iter()
            .map(|f| arrow::array::make_builder(f.data_type(), to_allocate))
            .collect();

        self.builders = Some(builders);
        self.rows_reserved = row_count;
        self.rows_capacity = to_allocate;
        Ok(())
    }

    fn flush(&mut self) -> Result<(), arrow::error::ArrowError> {
        let Some(mut builders) = self.builders.take() else {
            return Ok(());
        };
        let columns: Vec<aa::ArrayRef> = builders
            .iter_mut()
            .map(|builder| builder.finish().slice(0, self.rows_reserved))
            .collect();
        let rb = aa::RecordBatch::try_new(self.schema.clone(), columns)?;
        self.data.push(rb);
        Ok(())
    }

    pub fn finish(mut self) -> Result<Vec<aa::RecordBatch>, arrow::error::ArrowError> {
        self.flush()?;
        Ok(self.data)
    }

    fn next_builder(&mut self) -> &mut dyn Any {
        let col = self.receiver.next_col_index();
        // this is safe, because prepare_for_batch must have been called earlier
        let builders = self.builders.as_mut().unwrap();
        builders[col].as_any_mut()
    }

    fn next_as<T: 'static>(&mut self) -> &mut T {
        self.next_builder().downcast_mut().unwrap()
    }
}

/// Determines into which column the next stream value should go.
pub struct Organizer {
    col_count: usize,
    row_count: usize,

    next_row: usize,
    next_col: usize,
}

impl Organizer {
    fn new(col_count: usize) -> Self {
        Organizer {
            col_count,
            row_count: 0,

            next_row: 0,
            next_col: 0,
        }
    }

    fn reset_for_batch(&mut self, row_count: usize) {
        self.row_count = row_count;
        self.next_row = 0;
        self.next_col = 0;
    }

    fn next_col_index(&mut self) -> usize {
        let col = self.next_col;

        self.next_col += 1;
        if self.next_col == self.col_count {
            self.next_col = 0;
            self.next_row += 1;
        }
        col
    }
}
