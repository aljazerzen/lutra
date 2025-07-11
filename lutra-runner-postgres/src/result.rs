use bytes::{BufMut, BytesMut};
use core::str;
use lutra_bin::{Encode, ReversePointer, ir, rr};
use postgres_types as pg_ty;
use std::collections::HashMap;

#[cfg(not(feature = "tokio-postgres"))]
use postgres::Row;
#[cfg(feature = "tokio-postgres")]
use tokio_postgres::Row;

pub fn from_sql(program: &rr::SqlProgram, rows: &[Row]) -> Vec<u8> {
    // write rows to buffer
    let mut buf = bytes::BytesMut::new();

    let ctx = Context::new(&program.types);

    let encoder = ctx.construct_rows_encoder(&program.output_ty);
    encoder.encode(&mut buf, rows);

    buf.to_vec()
}

// TODO: use get_ty_mat for ident types (or recursive types in JSON)
struct Context<'a> {
    pub types: HashMap<&'a ir::Path, &'a ir::Ty>,
}

impl<'a> Context<'a> {
    fn new(ty_defs: &'a [ir::TyDef]) -> Self {
        Context {
            types: ty_defs.iter().map(|def| (&def.name, &def.ty)).collect(),
        }
    }

    fn get_ty_mat(&self, ty: &'a ir::Ty) -> &'a ir::Ty {
        match &ty.kind {
            ir::TyKind::Ident(path) => self.types.get(path).unwrap(),
            _ => ty,
        }
    }

    /// Constructs an encoder that takes a slice of rows and produces the given type
    #[tracing::instrument(name = "rows", skip_all)]
    fn construct_rows_encoder(&self, ty: &ir::Ty) -> Box<dyn EncodeRows> {
        tracing::debug!("rows for: {}", lutra_bin::ir::print_ty(ty));

        if let ir::TyKind::Array(item_ty) = &ty.kind {
            Box::new(ArrayEncoder {
                inner: self.construct_row_encoder(item_ty),
            })
        } else {
            Box::new(SingleRowEncoder {
                inner: self.construct_row_encoder(ty),
            })
        }
    }

    /// Constructs an encoder that takes a row and produces the given type
    #[tracing::instrument(name = "row", skip_all)]
    fn construct_row_encoder(&self, ty: &ir::Ty) -> Box<dyn EncodeRow> {
        tracing::debug!("row for: {}", lutra_bin::ir::print_ty(ty));

        match &self.get_ty_mat(ty).kind {
            // we expected X but got {X}: just take the first row
            ir::TyKind::Primitive(_) | ir::TyKind::Array(_) => Box::new(RowCellEncoder {
                inner: self.construct_cell_encoder(ty),
            }),

            ir::TyKind::Tuple(fields) => Box::new(TupleEncoder {
                inner: fields
                    .iter()
                    .map(|f| self.construct_row_encoder(&f.ty))
                    .collect(),
            }),

            ir::TyKind::Enum(variants) => Box::new(EnumEncoder {
                ty: variants.clone(),
                inner: variants
                    .iter()
                    .map(|f| self.construct_row_encoder(&f.ty))
                    .collect(),
            }),
            ir::TyKind::Function(_) => todo!(),
            ir::TyKind::Ident(_) => todo!(),
        }
    }

    /// Constructs an encoder that takes cell within a row and produces the given type
    #[tracing::instrument(name = "cell", skip_all)]
    fn construct_cell_encoder(&self, ty: &ir::Ty) -> Box<dyn EncodeCell> {
        tracing::debug!("cell for: {}", lutra_bin::ir::print_ty(ty));

        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Primitive(prim) if *prim == ir::TyPrimitive::text => Box::new(TextEncoder),
            ir::TyKind::Primitive(prim) => Box::new(PrimEncoder { prim: *prim }),

            ir::TyKind::Tuple(_) => unreachable!(),

            ir::TyKind::Array(_) => Box::new(JsonCellEncoder {
                inner: self.construct_json_encoder(ty),
            }),
            ir::TyKind::Enum(_) => todo!(),
            ir::TyKind::Function(_) => todo!(),
            ir::TyKind::Ident(_) => todo!(),
        }
    }

    /// Constructs an encoder that takes JSON and produces the given type
    #[tracing::instrument(name = "json", skip_all)]
    fn construct_json_encoder(&self, ty: &ir::Ty) -> Box<dyn EncodeJson> {
        tracing::debug!("json");

        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Primitive(prim) if *prim == ir::TyPrimitive::text => {
                Box::new(JsonTextEncoder)
            }
            ir::TyKind::Primitive(prim) => Box::new(JsonPrimEncoder { prim: *prim }),

            ir::TyKind::Tuple(fields) => Box::new(JsonTupleEncoder {
                inner: fields
                    .iter()
                    .map(|f| self.construct_json_encoder(&f.ty))
                    .collect(),
            }),

            ir::TyKind::Array(item) => Box::new(JsonArrayEncoder {
                inner: self.construct_json_encoder(item),
            }),
            ir::TyKind::Enum(_) => todo!(),
            ir::TyKind::Function(_) => todo!(),
            ir::TyKind::Ident(_) => todo!(),
        }
    }
}

enum HeadResidual {
    None,
    Offset(ReversePointer),
    Tuple(Vec<HeadResidual>),
    Json(tinyjson::JsonValue, Box<HeadResidual>),
}

trait EncodeRows {
    fn encode(&self, buf: &mut BytesMut, rows: &[Row]);
}

struct ArrayEncoder {
    inner: Box<dyn EncodeRow>,
}

impl EncodeRows for ArrayEncoder {
    fn encode(&self, buf: &mut BytesMut, rows: &[Row]) {
        // write array head
        let body_ptr = lutra_bin::ReversePointer::new(buf);
        buf.put_u32_le(rows.len() as u32);

        // write array body
        body_ptr.write_cur_len(buf);

        let mut residuals = Vec::with_capacity(rows.len());
        for row in rows {
            let mut row_iter = RowIter { row, idx: 0 };
            residuals.push(self.inner.encode_head(buf, &mut row_iter));
        }

        for (row, h) in rows.iter().zip(residuals.into_iter()) {
            let mut row_iter = RowIter { row, idx: 0 };
            self.inner.encode_body(buf, &mut row_iter, h);
        }
    }
}

struct SingleRowEncoder {
    inner: Box<dyn EncodeRow>,
}

impl EncodeRows for SingleRowEncoder {
    fn encode(&self, buf: &mut BytesMut, rows: &[Row]) {
        let row = &rows[0];

        let mut row_iter = RowIter { row, idx: 0 };
        let head = self.inner.encode_head(buf, &mut row_iter);

        let mut row_iter = RowIter { row, idx: 0 };
        self.inner.encode_body(buf, &mut row_iter, head);
    }
}

trait EncodeRow {
    fn encode_head(&self, buf: &mut BytesMut, row: &mut RowIter) -> HeadResidual;

    fn encode_body(&self, buf: &mut BytesMut, row: &mut RowIter, r: HeadResidual);

    fn skip(&self, row: &mut RowIter);
}

#[derive(Clone)]
struct RowIter<'t> {
    row: &'t Row,
    idx: usize,
}

impl<'t> RowIter<'t> {
    #[track_caller]
    fn get<T: pg_ty::FromSql<'t>>(&self) -> T {
        self.row.get::<usize, T>(self.idx)
    }
    fn advance(&mut self) {
        self.idx += 1;
    }
}

trait EncodeCell {
    fn encode_head(&self, buf: &mut BytesMut, cell: &RowIter) -> HeadResidual;

    fn encode_body(&self, buf: &mut BytesMut, cell: &RowIter, r: HeadResidual);
}

struct RowCellEncoder {
    inner: Box<dyn EncodeCell>,
}

impl EncodeRow for RowCellEncoder {
    fn encode_head(&self, buf: &mut BytesMut, row: &mut RowIter) -> HeadResidual {
        let h = self.inner.encode_head(buf, row);
        row.advance();
        h
    }

    fn encode_body(&self, buf: &mut BytesMut, row: &mut RowIter, r: HeadResidual) {
        self.inner.encode_body(buf, row, r);
        row.advance();
    }

    fn skip(&self, row: &mut RowIter) {
        row.advance()
    }
}

struct PrimEncoder {
    prim: ir::TyPrimitive,
}

impl EncodeCell for PrimEncoder {
    fn encode_head(&self, buf: &mut BytesMut, cell: &RowIter) -> HeadResidual {
        match self.prim {
            ir::TyPrimitive::bool => cell.get::<bool>().encode_head(buf),
            ir::TyPrimitive::int8 => cell.get::<i8>().encode_head(buf),
            ir::TyPrimitive::int16 => cell.get::<i16>().encode_head(buf),
            ir::TyPrimitive::int32 => cell.get::<i32>().encode_head(buf),
            ir::TyPrimitive::int64 => cell.get::<i64>().encode_head(buf),
            ir::TyPrimitive::uint8 => (cell.get::<i16>() as u8).encode_head(buf),
            ir::TyPrimitive::uint16 => (cell.get::<i32>() as u16).encode_head(buf),
            ir::TyPrimitive::uint32 => (cell.get::<i64>() as u32).encode_head(buf),
            ir::TyPrimitive::uint64 => todo!(),
            ir::TyPrimitive::float32 => cell.get::<f32>().encode_head(buf),
            ir::TyPrimitive::float64 => cell.get::<f64>().encode_head(buf),
            ir::TyPrimitive::text => unreachable!(),
        }
        HeadResidual::None
    }

    fn encode_body(&self, _buf: &mut BytesMut, _cell: &RowIter, _r: HeadResidual) {}
}

struct TextEncoder;

impl EncodeCell for TextEncoder {
    fn encode_head(&self, buf: &mut BytesMut, cell: &RowIter) -> HeadResidual {
        let value = cell.get::<&str>();
        let ptr = value.encode_head(buf);
        HeadResidual::Offset(ptr)
    }

    fn encode_body(&self, buf: &mut BytesMut, cell: &RowIter, r: HeadResidual) {
        let HeadResidual::Offset(r) = r else { panic!() };
        let value = cell.get::<&str>();
        value.encode_body(r, buf);
    }
}

struct TupleEncoder {
    inner: Vec<Box<dyn EncodeRow>>,
}

impl EncodeRow for TupleEncoder {
    fn encode_head(&self, buf: &mut BytesMut, row: &mut RowIter) -> HeadResidual {
        let mut residuals = Vec::with_capacity(self.inner.len());
        for inner in &self.inner {
            residuals.push(inner.encode_head(buf, row));
        }
        HeadResidual::Tuple(residuals)
    }

    fn encode_body(&self, buf: &mut BytesMut, row: &mut RowIter, r: HeadResidual) {
        let HeadResidual::Tuple(r) = r else { panic!() };

        for (inner, r) in self.inner.iter().zip(r) {
            inner.encode_body(buf, row, r);
        }
    }

    fn skip(&self, row: &mut RowIter) {
        for i in &self.inner {
            i.skip(row)
        }
    }
}
struct EnumEncoder {
    ty: Vec<ir::TyEnumVariant>,
    inner: Vec<Box<dyn EncodeRow>>,
}

impl EncodeRow for EnumEncoder {
    fn encode_head(&self, buf: &mut BytesMut, row: &mut RowIter) -> HeadResidual {
        let tag = row.get::<i8>() as usize;
        row.advance();
        for i in &self.inner[0..tag] {
            i.skip(row)
        }

        let head_format = lutra_bin::layout::enum_head_format(&self.ty);
        let variant_ty = &self.ty[tag];
        let variant_format = lutra_bin::layout::enum_variant_format(&head_format, &variant_ty.ty);

        let tag_bytes = &(tag as u64).to_le_bytes()[0..head_format.tag_bytes as usize];
        buf.put_slice(tag_bytes);

        let r = if head_format.has_ptr {
            self.inner[tag].skip(row);

            if variant_format.is_unit {
                // this is unit variant, no need to encode head
                HeadResidual::None
            } else {
                let offset = ReversePointer::new(buf);

                HeadResidual::Offset(offset)
            }
        } else {
            self.inner[tag].encode_head(buf, row)
        };

        if variant_format.padding_bytes > 0 {
            buf.put_bytes(0, variant_format.padding_bytes as usize);
        }

        for i in &self.inner[(tag + 1)..] {
            i.skip(row)
        }
        r
    }

    fn encode_body(&self, buf: &mut BytesMut, row: &mut RowIter, r: HeadResidual) {
        let tag = row.get::<i8>() as usize;
        row.advance();
        for i in &self.inner[0..tag] {
            i.skip(row)
        }

        let head_format = lutra_bin::layout::enum_head_format(&self.ty);

        if head_format.has_ptr {
            match r {
                HeadResidual::None => {
                    // unit variant, done
                }
                HeadResidual::Offset(offset_ptr) => {
                    offset_ptr.write_cur_len(buf);

                    let mut row2 = row.clone();
                    let residual = self.inner[tag].encode_head(buf, &mut row2);
                    self.inner[tag].encode_body(buf, row, residual);
                }
                _ => unreachable!(),
            }
        } else {
            self.inner[tag].encode_body(buf, row, r);
        }

        for i in &self.inner[(tag + 1)..] {
            i.skip(row)
        }
    }

    fn skip(&self, row: &mut RowIter) {
        row.advance(); // tag
        for i in &self.inner {
            i.skip(row)
        }
    }
}

struct JsonCellEncoder {
    inner: Box<dyn EncodeJson>,
}

impl EncodeCell for JsonCellEncoder {
    fn encode_head(&self, buf: &mut BytesMut, cell: &RowIter) -> HeadResidual {
        let value = cell.get::<Json>();
        let value: tinyjson::JsonValue = value.0.parse().unwrap();
        let r = self.inner.encode_head(buf, &value);
        HeadResidual::Json(value, Box::new(r))
    }

    fn encode_body(&self, buf: &mut BytesMut, _cell: &RowIter, r: HeadResidual) {
        let HeadResidual::Json(value, r) = r else {
            panic!()
        };
        self.inner.encode_body(buf, &value, *r);
    }
}

trait EncodeJson {
    fn encode_head(&self, buf: &mut BytesMut, value: &tinyjson::JsonValue) -> HeadResidual;

    fn encode_body(&self, buf: &mut BytesMut, value: &tinyjson::JsonValue, r: HeadResidual);
}

struct JsonPrimEncoder {
    prim: ir::TyPrimitive,
}

impl EncodeJson for JsonPrimEncoder {
    fn encode_head(&self, buf: &mut BytesMut, value: &tinyjson::JsonValue) -> HeadResidual {
        match (self.prim, value) {
            (ir::TyPrimitive::bool, tinyjson::JsonValue::Boolean(v)) => (*v).encode_head(buf),
            (ir::TyPrimitive::int8, tinyjson::JsonValue::Number(v)) => (*v as i8).encode_head(buf),
            (ir::TyPrimitive::int16, tinyjson::JsonValue::Number(v)) => {
                (*v as i16).encode_head(buf)
            }
            (ir::TyPrimitive::int32, tinyjson::JsonValue::Number(v)) => {
                (*v as i32).encode_head(buf)
            }
            (ir::TyPrimitive::int64, tinyjson::JsonValue::Number(v)) => {
                (*v as i64).encode_head(buf)
            }
            (ir::TyPrimitive::uint8, tinyjson::JsonValue::Number(v)) => (*v as u8).encode_head(buf),
            (ir::TyPrimitive::uint16, tinyjson::JsonValue::Number(v)) => {
                (*v as u16).encode_head(buf)
            }
            (ir::TyPrimitive::uint32, tinyjson::JsonValue::Number(v)) => {
                (*v as u32).encode_head(buf)
            }
            (ir::TyPrimitive::uint64, tinyjson::JsonValue::Number(v)) => {
                (*v as u64).encode_head(buf)
            }
            (ir::TyPrimitive::float32, tinyjson::JsonValue::Number(v)) => {
                (*v as f32).encode_head(buf)
            }
            (ir::TyPrimitive::float64, tinyjson::JsonValue::Number(v)) => {
                (*v as u64).encode_head(buf)
            }
            (ir::TyPrimitive::text, _) => unreachable!(),
            (_, v) => panic!(
                "expected {:?}, found JSON {}",
                self.prim,
                v.stringify().unwrap()
            ),
        };
        HeadResidual::None
    }

    fn encode_body(&self, _buf: &mut BytesMut, _value: &tinyjson::JsonValue, _r: HeadResidual) {}
}

struct JsonTextEncoder;

impl EncodeJson for JsonTextEncoder {
    fn encode_head(&self, buf: &mut BytesMut, value: &tinyjson::JsonValue) -> HeadResidual {
        let tinyjson::JsonValue::String(value) = value else {
            panic!()
        };
        HeadResidual::Offset(value.encode_head(buf))
    }

    fn encode_body(&self, buf: &mut BytesMut, value: &tinyjson::JsonValue, r: HeadResidual) {
        let tinyjson::JsonValue::String(value) = value else {
            panic!()
        };
        let HeadResidual::Offset(head) = r else {
            panic!()
        };
        value.encode_body(head, buf)
    }
}

struct JsonArrayEncoder {
    inner: Box<dyn EncodeJson>,
}

impl EncodeJson for JsonArrayEncoder {
    fn encode_head(&self, buf: &mut BytesMut, value: &tinyjson::JsonValue) -> HeadResidual {
        let tinyjson::JsonValue::Array(value) = value else {
            panic!()
        };

        let offset_ptr = ReversePointer::new(buf);
        buf.put_u32_le(value.len() as u32);
        HeadResidual::Offset(offset_ptr)
    }

    fn encode_body(&self, buf: &mut BytesMut, value: &tinyjson::JsonValue, r: HeadResidual) {
        let tinyjson::JsonValue::Array(value) = value else {
            panic!()
        };
        let HeadResidual::Offset(head) = r else {
            panic!()
        };

        head.write_cur_len(buf);

        let mut head_residuals = Vec::with_capacity(value.len());
        for i in value {
            head_residuals.push(self.inner.encode_head(buf, i));
        }

        for (v, r) in value.iter().zip(head_residuals.into_iter()) {
            self.inner.encode_body(buf, v, r)
        }
    }
}

struct JsonTupleEncoder {
    inner: Vec<Box<dyn EncodeJson>>,
}

impl EncodeJson for JsonTupleEncoder {
    fn encode_head(&self, buf: &mut BytesMut, value: &tinyjson::JsonValue) -> HeadResidual {
        let tinyjson::JsonValue::Array(value) = value else {
            panic!()
        };

        let mut head_residuals = Vec::with_capacity(self.inner.len());
        for (encoder, value) in self.inner.iter().zip(value) {
            head_residuals.push(encoder.encode_head(buf, value));
        }

        HeadResidual::Tuple(head_residuals)
    }

    fn encode_body(&self, buf: &mut BytesMut, value: &tinyjson::JsonValue, r: HeadResidual) {
        let tinyjson::JsonValue::Array(value) = value else {
            panic!()
        };
        let HeadResidual::Tuple(head_residuals) = r else {
            panic!()
        };

        for ((encoder, v), r) in self.inner.iter().zip(value).zip(head_residuals.into_iter()) {
            encoder.encode_body(buf, v, r)
        }
    }
}

struct Json<'a>(&'a str);

impl<'a> pg_ty::FromSql<'a> for Json<'a> {
    fn from_sql(
        ty: &pg_ty::Type,
        raw: &'a [u8],
    ) -> Result<Json<'a>, Box<dyn std::error::Error + Sync + Send>> {
        if let pg_ty::Type::JSONB = *ty {
            // we only support version 1 (currently the only version)
            assert!(raw[0] == 1);
            return Ok(Json(str::from_utf8(&raw[1..])?));
        }
        Ok(Json(str::from_utf8(raw)?))
    }

    fn accepts(ty: &pg_ty::Type) -> bool {
        matches!(*ty, pg_ty::Type::JSON | pg_ty::Type::JSONB)
    }
}
