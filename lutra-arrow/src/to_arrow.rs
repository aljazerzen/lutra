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

pub fn lutra_to_arrow(
    data: impl bytes::Buf + Clone,
    ty_item: &ir::Ty,
    ty_defs: &[ir::TyDef],
) -> Result<aa::RecordBatch, Error> {
    let ctx = Context::new(ty_defs);
    let ty_item = ctx.get_ty_mat(ty_item)?;

    let ir::TyKind::Tuple(ty_fields) = &ty_item.kind else {
        return Err(Error::UnsupportedType(format!(
            "expected tuple type for record batch, got {:?}",
            ty_item.kind
        )));
    };

    let schema = get_schema(ty_fields, &ctx)?;

    let mut writer = ArrowRowWriter::new(schema, 0);

    let item_head_size = ty_item.layout.as_ref().unwrap().head_size as usize;
    let item_field_offsets = lutra_bin::layout::tuple_field_offsets(ty_item);
    let array_reader = lutra_bin::ArrayReader::new(data, item_head_size.div_ceil(8));
    writer.prepare_for_batch(array_reader.remaining())?;
    for item in array_reader {
        let tuple_reader = lutra_bin::TupleReader::new(item, Cow::Borrowed(&item_field_offsets));
        for (i, ty_field) in ty_fields.iter().enumerate() {
            match &ty_field.ty.kind {
                ir::TyKind::Primitive(ir::TyPrimitive::bool) => {
                    let v = decode_prim::<bool>(tuple_reader.get_field(i));
                    writer.next_as::<aa::BooleanBuilder>().append_value(v);
                }
                ir::TyKind::Primitive(ir::TyPrimitive::int8) => {
                    let v = decode_prim::<i8>(tuple_reader.get_field(i));
                    writer.next_as::<aa::Int8Builder>().append_value(v);
                }
                ir::TyKind::Primitive(ir::TyPrimitive::int16) => {
                    let v = decode_prim::<i16>(tuple_reader.get_field(i));
                    writer.next_as::<aa::Int16Builder>().append_value(v);
                }
                ir::TyKind::Primitive(ir::TyPrimitive::int32) => {
                    let v = decode_prim::<i32>(tuple_reader.get_field(i));
                    writer.next_as::<aa::Int32Builder>().append_value(v);
                }
                ir::TyKind::Primitive(ir::TyPrimitive::int64) => {
                    let v = decode_prim::<i64>(tuple_reader.get_field(i));
                    writer.next_as::<aa::Int64Builder>().append_value(v);
                }
                ir::TyKind::Primitive(ir::TyPrimitive::uint8) => {
                    let v = decode_prim::<u8>(tuple_reader.get_field(i));
                    writer.next_as::<aa::UInt8Builder>().append_value(v);
                }
                ir::TyKind::Primitive(ir::TyPrimitive::uint16) => {
                    let v = decode_prim::<u16>(tuple_reader.get_field(i));
                    writer.next_as::<aa::UInt16Builder>().append_value(v);
                }
                ir::TyKind::Primitive(ir::TyPrimitive::uint32) => {
                    let v = decode_prim::<u32>(tuple_reader.get_field(i));
                    writer.next_as::<aa::UInt32Builder>().append_value(v);
                }
                ir::TyKind::Primitive(ir::TyPrimitive::uint64) => {
                    let v = decode_prim::<u64>(tuple_reader.get_field(i));
                    writer.next_as::<aa::UInt64Builder>().append_value(v);
                }
                ir::TyKind::Primitive(ir::TyPrimitive::float32) => {
                    let v = decode_prim::<f32>(tuple_reader.get_field(i));
                    writer.next_as::<aa::Float32Builder>().append_value(v);
                }
                ir::TyKind::Primitive(ir::TyPrimitive::float64) => {
                    let v = decode_prim::<f64>(tuple_reader.get_field(i));
                    writer.next_as::<aa::Float64Builder>().append_value(v);
                }
                ir::TyKind::Primitive(ir::TyPrimitive::text) => {
                    let mut d = tuple_reader.get_field(i);

                    // decode string
                    let (offset, len) = ArrayReader::<&[u8]>::read_head(d.chunk());
                    d.advance(offset);
                    let mut bytes = vec![0; len];
                    d.copy_to_slice(&mut bytes);

                    let s = String::from_utf8(bytes).unwrap();

                    writer.next_as::<aa::StringBuilder>().append_value(s);
                }

                // Composite types not yet supported
                _ => {
                    return Err(Error::UnsupportedType(format!(
                        "composite types not yet supported in to_arrow for field encoding: {:?}",
                        ty_field.ty.kind
                    )));
                }
            }
        }
    }

    let batches = writer.finish()?;
    assert_eq!(batches.len(), 1);
    Ok(batches.into_iter().next().unwrap())
}

fn get_schema(ty_fields: &[ir::TyTupleField], ctx: &Context) -> Result<SchemaRef, Error> {
    let mut fields = Vec::new();
    for (index, ty_field) in ty_fields.iter().enumerate() {
        let name = ty_field.name.clone().unwrap_or_else(|| format!("_{index}"));

        let data_type = get_data_type(&ty_field.ty, ctx)?;
        fields.push(Field::new(name, data_type, false));
    }
    Ok(Arc::new(Schema::new(fields)))
}

fn get_data_type(ty: &ir::Ty, ctx: &Context) -> Result<DataType, Error> {
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
        ir::TyKind::Array(item_ty) => {
            let item_ty = ctx.get_ty_mat(item_ty)?;
            let item_data_type = get_data_type(item_ty, ctx)?;
            let item_field = Arc::new(Field::new("item", item_data_type, false));
            Ok(DataType::List(item_field))
        }

        // Nested Tuples
        ir::TyKind::Tuple(nested_fields) => {
            let struct_fields: Result<Vec<_>, Error> = nested_fields
                .iter()
                .enumerate()
                .map(|(i, f)| {
                    let name = f.name.clone().unwrap_or_else(|| format!("_{i}"));
                    let data_type = get_data_type(&f.ty, ctx)?;
                    Ok(Arc::new(Field::new(name, data_type, false)))
                })
                .collect();
            Ok(DataType::Struct(struct_fields?.into()))
        }

        // Enums
        ir::TyKind::Enum(_variants) => {
            // For now, return error - Union encoding is complex
            Err(Error::UnsupportedType(
                "enum types not yet supported in to_arrow schema generation".to_string(),
            ))
        }

        ir::TyKind::Function(_) => Err(Error::UnsupportedType(
            "cannot serialize functions to Arrow".to_string(),
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
