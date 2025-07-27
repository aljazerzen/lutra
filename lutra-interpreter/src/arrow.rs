use std::any::Any;
use std::borrow::Cow;
use std::sync::Arc;

use arrow::array::{self as arrow_array};
use arrow::datatypes::{self as arrow_datatypes, DataType, Field, Schema, SchemaRef};

use lutra_bin::{Decode, ir};

pub fn validate_schema(schema: &arrow_datatypes::Schema, item_ty: &ir::Ty) -> Result<(), String> {
    let ty_fields = item_ty.kind.as_tuple().unwrap();

    for (index, (ty_f, f)) in std::iter::zip(ty_fields, schema.fields()).enumerate() {
        let field_name = ty_f
            .name
            .clone()
            .unwrap_or_else(|| format!("field {index}"));

        validate_field(ty_f, f).map_err(|m| format!("{field_name}: {m}"))?;
    }
    Ok(())
}

fn validate_field(
    ty_f: &ir::TyTupleField,
    f: &std::sync::Arc<arrow_datatypes::Field>,
) -> Result<(), String> {
    match &ty_f.ty.kind {
        ir::TyKind::Primitive(prim) => {
            let expected: &[arrow_datatypes::DataType] = match prim {
                ir::TyPrimitive::bool => &[arrow_datatypes::DataType::Boolean],
                ir::TyPrimitive::int8 => &[arrow_datatypes::DataType::Int8],
                ir::TyPrimitive::int16 => &[arrow_datatypes::DataType::Int16],
                ir::TyPrimitive::int32 => &[arrow_datatypes::DataType::Int32],
                ir::TyPrimitive::int64 => &[arrow_datatypes::DataType::Int64],
                ir::TyPrimitive::uint8 => &[arrow_datatypes::DataType::UInt8],
                ir::TyPrimitive::uint16 => &[arrow_datatypes::DataType::UInt16],
                ir::TyPrimitive::uint32 => &[arrow_datatypes::DataType::UInt32],
                ir::TyPrimitive::uint64 => &[arrow_datatypes::DataType::UInt64],
                ir::TyPrimitive::float32 => &[arrow_datatypes::DataType::Float32],
                ir::TyPrimitive::float64 => &[arrow_datatypes::DataType::Float64],
                ir::TyPrimitive::text => &[
                    arrow_datatypes::DataType::Utf8,
                    arrow_datatypes::DataType::LargeUtf8,
                ],
            };
            validate_data_type(expected, f)?;
        }

        ir::TyKind::Tuple(_) => unimplemented!(),
        ir::TyKind::Array(_) => unimplemented!(),
        ir::TyKind::Enum(_) => unimplemented!(),

        ir::TyKind::Function(_) | ir::TyKind::Ident(_) => panic!(),
    };
    Ok(())
}

fn validate_data_type(
    expected: &[arrow_datatypes::DataType],
    found: &arrow_datatypes::Field,
) -> Result<(), String> {
    for e in expected {
        if e == found.data_type() {
            return Ok(());
        }
    }
    let nullable = if found.is_nullable() { "nullable " } else { "" };
    let expected: Vec<_> = expected.iter().map(|f| f.to_string()).collect();
    let expected = expected.join(" ,");
    Err(format!(
        "expected {expected}, found {nullable}{}",
        found.data_type()
    ))
}

pub fn arrow_to_lutra(
    reader: impl arrow_array::RecordBatchReader,
    item_ty: &ir::Ty,
) -> Result<lutra_bin::Data, ()> {
    let ty_fields = item_ty.kind.as_tuple().ok_or(())?;

    let mut output = lutra_bin::ArrayWriter::new_for_item_ty(item_ty);
    for batch in reader {
        let batch = batch.unwrap();

        for row_index in 0..batch.num_rows() {
            let mut tuple = lutra_bin::TupleWriter::new_for_ty(item_ty);
            for (col_index, ty_field) in ty_fields.iter().enumerate() {
                let array = batch.column(col_index).as_any();

                let value = match ty_field.ty.kind {
                    ir::TyKind::Primitive(ir::TyPrimitive::bool) => encode(
                        &array
                            .downcast_ref::<arrow_array::BooleanArray>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::int8) => encode(
                        &array
                            .downcast_ref::<arrow_array::Int8Array>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::int16) => encode(
                        &array
                            .downcast_ref::<arrow_array::Int16Array>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::int32) => encode(
                        &array
                            .downcast_ref::<arrow_array::Int32Array>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::int64) => encode(
                        &array
                            .downcast_ref::<arrow_array::Int64Array>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::uint8) => encode(
                        &array
                            .downcast_ref::<arrow_array::UInt8Array>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::uint16) => encode(
                        &array
                            .downcast_ref::<arrow_array::UInt16Array>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::uint32) => encode(
                        &array
                            .downcast_ref::<arrow_array::UInt32Array>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::uint64) => encode(
                        &array
                            .downcast_ref::<arrow_array::UInt64Array>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::float32) => encode(
                        &array
                            .downcast_ref::<arrow_array::Float32Array>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::float64) => encode(
                        &array
                            .downcast_ref::<arrow_array::Float64Array>()
                            .unwrap()
                            .value(row_index),
                    ),
                    ir::TyKind::Primitive(ir::TyPrimitive::text) => encode(
                        Option::or(
                            array
                                .downcast_ref::<arrow_array::StringArray>()
                                .map(|array| array.value(row_index)),
                            array
                                .downcast_ref::<arrow_array::LargeStringArray>()
                                .map(|array| array.value(row_index)),
                        )
                        .unwrap(),
                    ),

                    ir::TyKind::Tuple(_) | ir::TyKind::Array(_) | ir::TyKind::Enum(_) => {
                        todo!()
                    }
                    ir::TyKind::Function(_) | ir::TyKind::Ident(_) => {
                        return Err(());
                    }
                };

                tuple.write_field(value);
            }
            output.write_item(tuple.finish());
        }
    }
    Ok(output.finish())
}

fn encode<T: lutra_bin::Encode + ?Sized>(value: &T) -> lutra_bin::Data {
    lutra_bin::Data::new(value.encode())
}

fn decode<T: lutra_bin::Decode>(data: lutra_bin::Data) -> T {
    T::decode(data.slice(T::head_size().div_ceil(8))).unwrap()
}

pub(crate) fn lutra_to_arrow(data: lutra_bin::Data, ty_item: &ir::Ty) -> arrow_array::RecordBatch {
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
        let tuple_reader = lutra_bin::TupleReader::new(&item, Cow::Borrowed(&item_field_offsets));
        for (i, ty_field) in ty_fields.iter().enumerate() {
            match &ty_field.ty.kind {
                ir::TyKind::Primitive(ir::TyPrimitive::bool) => {
                    writer
                        .next_builder()
                        .downcast_mut::<arrow_array::BooleanBuilder>()
                        .unwrap()
                        .append_value(decode::<bool>(tuple_reader.get_field(i)));
                }
                ir::TyKind::Primitive(ir::TyPrimitive::int8) => {
                    writer
                        .next_builder()
                        .downcast_mut::<arrow_array::Int8Builder>()
                        .unwrap()
                        .append_value(decode::<i8>(tuple_reader.get_field(i)));
                }
                ir::TyKind::Primitive(ir::TyPrimitive::int16) => {
                    writer
                        .next_builder()
                        .downcast_mut::<arrow_array::Int16Builder>()
                        .unwrap()
                        .append_value(decode::<i16>(tuple_reader.get_field(i)));
                }
                ir::TyKind::Primitive(ir::TyPrimitive::int32) => {
                    writer
                        .next_builder()
                        .downcast_mut::<arrow_array::Int32Builder>()
                        .unwrap()
                        .append_value(decode::<i32>(tuple_reader.get_field(i)));
                }
                ir::TyKind::Primitive(ir::TyPrimitive::int64) => {
                    writer
                        .next_builder()
                        .downcast_mut::<arrow_array::Int64Builder>()
                        .unwrap()
                        .append_value(decode::<i64>(tuple_reader.get_field(i)));
                }
                ir::TyKind::Primitive(ir::TyPrimitive::uint8) => {
                    writer
                        .next_builder()
                        .downcast_mut::<arrow_array::UInt8Builder>()
                        .unwrap()
                        .append_value(decode::<u8>(tuple_reader.get_field(i)));
                }
                ir::TyKind::Primitive(ir::TyPrimitive::uint16) => {
                    writer
                        .next_builder()
                        .downcast_mut::<arrow_array::UInt16Builder>()
                        .unwrap()
                        .append_value(decode::<u16>(tuple_reader.get_field(i)));
                }
                ir::TyKind::Primitive(ir::TyPrimitive::uint32) => {
                    writer
                        .next_builder()
                        .downcast_mut::<arrow_array::UInt32Builder>()
                        .unwrap()
                        .append_value(decode::<u32>(tuple_reader.get_field(i)));
                }
                ir::TyKind::Primitive(ir::TyPrimitive::uint64) => {
                    writer
                        .next_builder()
                        .downcast_mut::<arrow_array::UInt64Builder>()
                        .unwrap()
                        .append_value(decode::<u64>(tuple_reader.get_field(i)));
                }
                ir::TyKind::Primitive(ir::TyPrimitive::float32) => {
                    writer
                        .next_builder()
                        .downcast_mut::<arrow_array::Float32Builder>()
                        .unwrap()
                        .append_value(decode::<f32>(tuple_reader.get_field(i)));
                }
                ir::TyKind::Primitive(ir::TyPrimitive::float64) => {
                    writer
                        .next_builder()
                        .downcast_mut::<arrow_array::Float64Builder>()
                        .unwrap()
                        .append_value(decode::<f64>(tuple_reader.get_field(i)));
                }
                ir::TyKind::Primitive(ir::TyPrimitive::text) => {
                    let d = tuple_reader.get_field(i);

                    // TODO: this is potentially very expensive
                    let d = d.flatten();
                    let s = String::decode(&d).unwrap();

                    writer
                        .next_builder()
                        .downcast_mut::<arrow_array::StringBuilder>()
                        .unwrap()
                        .append_value(s);
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
    data: Vec<arrow_array::RecordBatch>,

    /// Determines into which column the next stream value should go.
    receiver: Organizer,

    /// Array buffers.
    builders: Option<Vec<Box<dyn arrow_array::ArrayBuilder>>>,
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

        let builders: Vec<Box<dyn arrow_array::ArrayBuilder>> = self
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
        let columns: Vec<arrow_array::ArrayRef> = builders
            .iter_mut()
            .map(|builder| builder.finish().slice(0, self.rows_reserved))
            .collect();
        let rb = arrow_array::RecordBatch::try_new(self.schema.clone(), columns)?;
        self.data.push(rb);
        Ok(())
    }

    pub fn finish(mut self) -> Result<Vec<arrow_array::RecordBatch>, arrow::error::ArrowError> {
        self.flush()?;
        Ok(self.data)
    }

    fn next_builder(&mut self) -> &mut dyn Any {
        let col = self.receiver.next_col_index();
        // this is safe, because prepare_for_batch must have been called earlier
        let builders = self.builders.as_mut().unwrap();
        builders[col].as_any_mut()
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
