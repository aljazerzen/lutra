use crate::vec;

use crate::ArrayReader;
use crate::Decode;
use crate::Error;
use crate::TupleReader;
use crate::ir;
use crate::layout;

pub trait Visitor<'t, B>
where
    B: bytes::Buf + Clone,
{
    type Res;

    fn get_ty(&self, name: &ir::Path) -> &'t ir::Ty;

    fn get_mat_ty(&self, ty: &'t ir::Ty) -> &'t ir::Ty {
        let mut ty = ty;
        while let ir::TyKind::Ident(name) = &ty.kind {
            ty = self.get_ty(name);
        }
        ty
    }

    fn visit(&mut self, buf: B, ty: &'t ir::Ty) -> Result<Self::Res, crate::Error> {
        let mut ty = ty;
        while let ir::TyKind::Ident(ident) = &ty.kind {
            if let Some(result) = self.visit_ident(buf.clone(), ident)? {
                return Ok(result);
            }
            ty = self.get_ty(ident);
        }

        self.visit_concrete(buf, ty)
    }

    fn visit_ident(
        &mut self,
        buf: B,
        ident: &'t ir::Path,
    ) -> Result<Option<Self::Res>, crate::Error> {
        if ident.is(&["std", "Bool"]) {
            let v = bool::decode(buf.chunk())?;
            self.visit_bool(v).map(Some)
        } else if ident.is(&["std", "Int8"]) {
            let v = i8::decode(buf.chunk())?;
            self.visit_int8(v).map(Some)
        } else if ident.is(&["std", "Int16"]) {
            let v = i16::decode(buf.chunk())?;
            self.visit_int16(v).map(Some)
        } else if ident.is(&["std", "Int32"]) {
            let v = i32::decode(buf.chunk())?;
            self.visit_int32(v).map(Some)
        } else if ident.is(&["std", "Int64"]) {
            let v = i64::decode(buf.chunk())?;
            self.visit_int64(v).map(Some)
        } else if ident.is(&["std", "Uint8"]) {
            let v = u8::decode(buf.chunk())?;
            self.visit_uint8(v).map(Some)
        } else if ident.is(&["std", "Uint16"]) {
            let v = u16::decode(buf.chunk())?;
            self.visit_uint16(v).map(Some)
        } else if ident.is(&["std", "Uint32"]) {
            let v = u32::decode(buf.chunk())?;
            self.visit_uint32(v).map(Some)
        } else if ident.is(&["std", "Uint64"]) {
            let v = u64::decode(buf.chunk())?;
            self.visit_uint64(v).map(Some)
        } else if ident.is(&["std", "Float32"]) {
            let v = f32::decode(buf.chunk())?;
            self.visit_float32(v).map(Some)
        } else if ident.is(&["std", "Float64"]) {
            let v = f64::decode(buf.chunk())?;
            self.visit_float64(v).map(Some)
        } else if ident.is(&["std", "Text"]) {
            let (offset, len) = ArrayReader::<&[u8]>::read_head(buf.chunk());
            let mut buf = buf;
            buf.advance(offset);
            self.visit_text(buf, len).map(Some)
        } else if ident.is(&["std", "Date"]) {
            let v = i32::decode(buf.chunk())?;
            self.visit_date(v).map(Some)
        } else if ident.is(&["std", "Time"]) {
            let v = i64::decode(buf.chunk())?;
            self.visit_time(v).map(Some)
        } else if ident.is(&["std", "Timestamp"]) {
            let v = i64::decode(buf.chunk())?;
            self.visit_timestamp(v).map(Some)
        } else if ident.is(&["std", "Decimal"]) {
            let v = i64::decode(buf.chunk())?;
            self.visit_decimal(v).map(Some)
        } else {
            Ok(None)
        }
    }

    fn visit_concrete(&mut self, buf: B, ty: &'t ir::Ty) -> Result<Self::Res, crate::Error> {
        match &ty.kind {
            ir::TyKind::Primitive(ir::TyPrimitive::prim8) => {
                self.visit_uint8(u8::decode(buf.chunk())?)
            }
            ir::TyKind::Primitive(ir::TyPrimitive::prim16) => {
                self.visit_uint16(u16::decode(buf.chunk())?)
            }
            ir::TyKind::Primitive(ir::TyPrimitive::prim32) => {
                self.visit_uint32(u32::decode(buf.chunk())?)
            }
            ir::TyKind::Primitive(ir::TyPrimitive::prim64) => {
                self.visit_uint64(u64::decode(buf.chunk())?)
            }
            ir::TyKind::Tuple(ty_fields) => {
                let reader = TupleReader::new_for_ty(buf, ty);

                let fields = ty_fields
                    .iter()
                    .enumerate()
                    .map(|(i, t)| (reader.get_field(i), t));
                self.visit_tuple(fields)
            }
            ir::TyKind::Array(ty_items) => {
                let reader = ArrayReader::new_for_ty(buf, ty);
                self.visit_array(reader, ty_items)
            }
            ir::TyKind::Enum(variants) => {
                let head = layout::enum_head_format(variants, &ty.variants_recursive);

                let mut buf = buf;

                let mut tag_bytes = vec![0; 8];
                buf.copy_to_slice(&mut tag_bytes[0..head.tag_bytes as usize]);
                tag_bytes.resize(8, 0);
                let tag = u64::from_le_bytes(tag_bytes.try_into().unwrap()) as usize;

                let variant = variants.get(tag).unwrap();

                let variant_format = layout::enum_variant_format(&head, &variant.ty);

                let mut buf_inner = buf;
                if head.has_ptr {
                    if variant_format.is_unit {
                        // no need to advance, inner is unit anyway
                    } else {
                        // read ptr without advancing
                        let ptr = buf_inner.clone().get_u32_le() as usize;

                        buf_inner.advance(ptr);
                    }
                } else {
                    // no need to advance, inner is right after tag
                };

                self.visit_enum(tag, buf_inner, variants)
            }

            ir::TyKind::Function(..) => Err(Error::InvalidType),
            ir::TyKind::Ident(..) => Err(Error::Bug),
        }
    }

    fn visit_bool(&mut self, v: bool) -> Result<Self::Res, crate::Error>;
    fn visit_int8(&mut self, v: i8) -> Result<Self::Res, crate::Error>;
    fn visit_int16(&mut self, v: i16) -> Result<Self::Res, crate::Error>;
    fn visit_int32(&mut self, v: i32) -> Result<Self::Res, crate::Error>;
    fn visit_int64(&mut self, v: i64) -> Result<Self::Res, crate::Error>;
    fn visit_uint8(&mut self, v: u8) -> Result<Self::Res, crate::Error>;
    fn visit_uint16(&mut self, v: u16) -> Result<Self::Res, crate::Error>;
    fn visit_uint32(&mut self, v: u32) -> Result<Self::Res, crate::Error>;
    fn visit_uint64(&mut self, v: u64) -> Result<Self::Res, crate::Error>;
    fn visit_float32(&mut self, v: f32) -> Result<Self::Res, crate::Error>;
    fn visit_float64(&mut self, v: f64) -> Result<Self::Res, crate::Error>;
    fn visit_text(&mut self, contents: B, len: usize) -> Result<Self::Res, crate::Error>;
    fn visit_date(&mut self, days: i32) -> Result<Self::Res, crate::Error> {
        self.visit_int32(days)
    }
    fn visit_time(&mut self, micros: i64) -> Result<Self::Res, crate::Error> {
        self.visit_int64(micros)
    }
    fn visit_timestamp(&mut self, micros: i64) -> Result<Self::Res, crate::Error> {
        self.visit_int64(micros)
    }
    fn visit_decimal(&mut self, cents: i64) -> Result<Self::Res, crate::Error> {
        self.visit_int64(cents)
    }

    fn visit_tuple(
        &mut self,
        fields: impl Iterator<Item = (B, &'t ir::TyTupleField)>,
    ) -> Result<Self::Res, crate::Error>;

    fn visit_array(
        &mut self,
        items: impl Iterator<Item = B>,
        ty_items: &'t ir::Ty,
    ) -> Result<Self::Res, crate::Error>;

    fn visit_enum(
        &mut self,
        tag: usize,
        inner: B,
        ty_variants: &'t [ir::TyEnumVariant],
    ) -> Result<Self::Res, crate::Error>;
}
