use std::any::Any;
use std::borrow::Cow;
use std::sync::Arc;

use arrow::array as aa;
use arrow::datatypes::{DataType, Field, Schema, SchemaRef};

use lutra_bin::{ArrayReader, bytes, ir};

fn decode_prim<T: lutra_bin::Decode>(data: impl bytes::Buf) -> T {
    T::decode(data.chunk()).unwrap()
}

pub fn lutra_to_arrow(data: impl bytes::Buf + Clone, ty_item: &ir::Ty) -> aa::RecordBatch {
    let ir::TyKind::Tuple(ty_fields) = &ty_item.kind else {
        panic!()
    };

    let schema = get_schema(ty_fields).unwrap();

    let mut writer = ArrowRowWriter::new(schema, 0);

    let item_head_size = ty_item.layout.as_ref().unwrap().head_size as usize;
    let item_field_offsets = lutra_bin::layout::tuple_field_offsets(ty_item);
    let array_reader = lutra_bin::ArrayReader::new(data, item_head_size.div_ceil(8));
    writer.prepare_for_batch(array_reader.remaining()).unwrap();
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

                _ => unreachable!(),
            }
        }
    }

    let batches = writer.finish().unwrap();
    assert_eq!(batches.len(), 1);
    batches.into_iter().next().unwrap()
}

fn get_schema(ty_fields: &[ir::TyTupleField]) -> Result<SchemaRef, String> {
    let mut fields = Vec::new();
    for (index, ty_field) in ty_fields.iter().enumerate() {
        let name = ty_field
            .name
            .clone()
            .unwrap_or_else(|| format!("field{index}"));

        let data_type = match &ty_field.ty.kind {
            ir::TyKind::Primitive(ir::TyPrimitive::bool) => DataType::Boolean,
            ir::TyKind::Primitive(ir::TyPrimitive::int8) => DataType::Int8,
            ir::TyKind::Primitive(ir::TyPrimitive::int16) => DataType::Int16,
            ir::TyKind::Primitive(ir::TyPrimitive::int32) => DataType::Int32,
            ir::TyKind::Primitive(ir::TyPrimitive::int64) => DataType::Int64,
            ir::TyKind::Primitive(ir::TyPrimitive::uint8) => DataType::UInt8,
            ir::TyKind::Primitive(ir::TyPrimitive::uint16) => DataType::UInt16,
            ir::TyKind::Primitive(ir::TyPrimitive::uint32) => DataType::UInt32,
            ir::TyKind::Primitive(ir::TyPrimitive::uint64) => DataType::UInt64,
            ir::TyKind::Primitive(ir::TyPrimitive::float32) => DataType::Float32,
            ir::TyKind::Primitive(ir::TyPrimitive::float64) => DataType::Float64,
            ir::TyKind::Primitive(ir::TyPrimitive::text) => DataType::Utf8,

            ir::TyKind::Function(_) => {
                return Err(format!(
                    "Cannot serialize functions to arrow (field {name})"
                ));
            }
            ir::TyKind::Ident(_)
            | ir::TyKind::Tuple(_)
            | ir::TyKind::Array(_)
            | ir::TyKind::Enum(_) => {
                todo!()
            }
        };

        fields.push(Field::new(name, data_type, false));
    }
    Ok(Arc::new(Schema::new(fields)))
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
