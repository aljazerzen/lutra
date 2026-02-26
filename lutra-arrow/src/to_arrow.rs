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

    let writer = match &ty.kind {
        ir::TyKind::Array(ty_item) => {
            let ty_item = ctx.get_ty_mat(ty_item)?;
            let row_converter = construct_row_converter(ty_item, &ctx)?;

            let array_reader = lutra_bin::ArrayReader::new_for_ty(data, ty);

            let mut writer = ArrowRowWriter::new(schema, array_reader.remaining());
            for item in array_reader {
                row_converter.convert(&mut writer, item.chunk());
            }
            writer
        }
        _ => {
            let mut writer = ArrowRowWriter::new(schema, 1);

            let converter = construct_row_converter(ty, &ctx)?;
            converter.convert(&mut writer, data.chunk());

            writer
        }
    };

    Ok(writer.finish()?)
}

// ---------------------------------------------------------------------------
// Convert trait
// ---------------------------------------------------------------------------

/// Decodes a Lutra value and writes it into [ArrowRowWriter].
trait Convert {
    fn convert(&self, writer: &mut ArrowRowWriter, data: &[u8]);
}

/// Row converter for tuple types: reads each field at its pre-computed offset.
struct TupleConverter {
    field_offsets: Vec<u32>,
    fields: Vec<Box<dyn Convert>>,
}

impl Convert for TupleConverter {
    fn convert(&self, writer: &mut ArrowRowWriter, data: &[u8]) {
        let tuple_reader =
            lutra_bin::TupleReader::new(data, Cow::Borrowed(self.field_offsets.as_slice()));
        for (i, c) in self.fields.iter().enumerate() {
            c.convert(writer, tuple_reader.get_field(i));
        }
    }
}

macro_rules! prim_converter {
    ($name:ident, $rust_ty:ty, $builder_ty:ty) => {
        struct $name;
        impl Convert for $name {
            fn convert(&self, writer: &mut ArrowRowWriter, data: &[u8]) {
                let v = decode_prim::<$rust_ty>(data);
                writer.next_as::<$builder_ty>().append_value(v);
            }
        }
    };
}

prim_converter!(BoolConverter, bool, aa::BooleanBuilder);
prim_converter!(Int8Converter, i8, aa::Int8Builder);
prim_converter!(Int16Converter, i16, aa::Int16Builder);
prim_converter!(Int32Converter, i32, aa::Int32Builder);
prim_converter!(Int64Converter, i64, aa::Int64Builder);
prim_converter!(UInt8Converter, u8, aa::UInt8Builder);
prim_converter!(UInt16Converter, u16, aa::UInt16Builder);
prim_converter!(UInt32Converter, u32, aa::UInt32Builder);
prim_converter!(UInt64Converter, u64, aa::UInt64Builder);
prim_converter!(Float32Converter, f32, aa::Float32Builder);
prim_converter!(Float64Converter, f64, aa::Float64Builder);

struct TextConverter;
impl Convert for TextConverter {
    fn convert(&self, writer: &mut ArrowRowWriter, data: &[u8]) {
        let (offset, len) = ArrayReader::<&[u8]>::read_head(data);
        // Safety: Lutra text is always valid UTF-8
        let s = std::str::from_utf8(&data[offset..offset + len]).unwrap();
        writer.next_as::<aa::StringBuilder>().append_value(s);
    }
}

// ---------------------------------------------------------------------------
// Constructor functions
// ---------------------------------------------------------------------------

/// Build a row converter for the given type.
fn construct_row_converter(ty: &ir::Ty, ctx: &Context) -> Result<Box<dyn Convert>, Error> {
    match &ty.kind {
        ir::TyKind::Tuple(ty_fields) => {
            let field_offsets = lutra_bin::layout::tuple_field_offsets(ty);
            let fields = ty_fields
                .iter()
                .map(|f| construct_cell_converter(&f.ty, ctx))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Box::new(TupleConverter {
                field_offsets,
                fields,
            }))
        }
        ir::TyKind::Primitive(_) => Ok(construct_cell_converter(ty, ctx)?),
        ir::TyKind::Array(_) => Err(Error::UnsupportedType("nested arrays not supported".into())),
        ir::TyKind::Enum(_) => Err(Error::UnsupportedType(
            "enum types not yet supported".into(),
        )),
        ir::TyKind::Function(_) => Err(Error::UnsupportedType(
            "cannot serialize functions to Arrow".into(),
        )),
        ir::TyKind::Ident(_) => unreachable!("should have been resolved by get_ty_mat"),
    }
}

/// Build a converter for a single column and a single row.
fn construct_cell_converter(ty: &ir::Ty, ctx: &Context) -> Result<Box<dyn Convert>, Error> {
    let ty = ctx.get_ty_mat(ty)?;
    match &ty.kind {
        ir::TyKind::Primitive(ir::TyPrimitive::bool) => Ok(Box::new(BoolConverter)),
        ir::TyKind::Primitive(ir::TyPrimitive::int8) => Ok(Box::new(Int8Converter)),
        ir::TyKind::Primitive(ir::TyPrimitive::int16) => Ok(Box::new(Int16Converter)),
        ir::TyKind::Primitive(ir::TyPrimitive::int32) => Ok(Box::new(Int32Converter)),
        ir::TyKind::Primitive(ir::TyPrimitive::int64) => Ok(Box::new(Int64Converter)),
        ir::TyKind::Primitive(ir::TyPrimitive::uint8) => Ok(Box::new(UInt8Converter)),
        ir::TyKind::Primitive(ir::TyPrimitive::uint16) => Ok(Box::new(UInt16Converter)),
        ir::TyKind::Primitive(ir::TyPrimitive::uint32) => Ok(Box::new(UInt32Converter)),
        ir::TyKind::Primitive(ir::TyPrimitive::uint64) => Ok(Box::new(UInt64Converter)),
        ir::TyKind::Primitive(ir::TyPrimitive::float32) => Ok(Box::new(Float32Converter)),
        ir::TyKind::Primitive(ir::TyPrimitive::float64) => Ok(Box::new(Float64Converter)),
        ir::TyKind::Primitive(ir::TyPrimitive::text) => Ok(Box::new(TextConverter)),
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

// ---------------------------------------------------------------------------
// Schema helpers
// ---------------------------------------------------------------------------

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

/// Get arrow schema fields for a row type
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
        ir::TyKind::Array(_) => Err(Error::UnsupportedType(
            "nested arrays not supported".to_string(),
        )),
        ir::TyKind::Tuple(_) => Err(Error::UnsupportedType(
            "nested tuples not supported".to_string(),
        )),
        ir::TyKind::Enum(_) => Err(Error::UnsupportedType(
            "enum types not supported".to_string(),
        )),
        ir::TyKind::Function(_) => Err(Error::UnsupportedType(
            "cannot serialize functions to Arrow".into(),
        )),
        ir::TyKind::Ident(_) => unreachable!("should have been resolved by get_ty_mat"),
    }
}

// ---------------------------------------------------------------------------
// ArrowRowWriter and Organizer
// ---------------------------------------------------------------------------

/// Receives values row-by-row and passes them to [ArrayBuilder]s,
/// which construct [RecordBatch]es.
pub struct ArrowRowWriter {
    schema: SchemaRef,

    /// Determines into which column the next stream value should go.
    receiver: Organizer,

    /// Array buffers.
    builders: Vec<Box<dyn aa::ArrayBuilder>>,
}

impl ArrowRowWriter {
    pub fn new(schema: SchemaRef, capacity: usize) -> Self {
        ArrowRowWriter {
            receiver: Organizer::new(schema.fields().len()),

            builders: Self::allocate(&schema, capacity),
            schema,
        }
    }

    fn allocate(schema: &Schema, capacity: usize) -> Vec<Box<dyn aa::ArrayBuilder>> {
        schema
            .fields
            .iter()
            .map(|f| arrow::array::make_builder(f.data_type(), capacity))
            .collect()
    }

    fn finish(self) -> Result<aa::RecordBatch, arrow::error::ArrowError> {
        let columns: Vec<aa::ArrayRef> = self
            .builders
            .into_iter()
            .map(|mut builder| builder.finish())
            .collect();
        aa::RecordBatch::try_new(self.schema, columns)
    }

    pub fn next_builder(&mut self) -> &mut dyn Any {
        let col = self.receiver.next_col_index();
        self.builders[col].as_any_mut()
    }

    pub fn next_as<T: 'static>(&mut self) -> &mut T {
        self.next_builder().downcast_mut().unwrap()
    }
}

/// Determines into which column the next stream value should go.
pub struct Organizer {
    col_count: usize,

    next_row: usize,
    next_col: usize,
}

impl Organizer {
    fn new(col_count: usize) -> Self {
        Organizer {
            col_count,

            next_row: 0,
            next_col: 0,
        }
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
