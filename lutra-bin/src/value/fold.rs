use crate::ir;

use crate::Error;
use crate::Value;

pub trait ValueVisitor<'t> {
    type Res;

    fn get_ty(&self, name: &ir::Path) -> &'t ir::Ty;

    fn get_mat_ty(&self, ty: &'t ir::Ty) -> &'t ir::Ty {
        let mut ty = ty;
        while let ir::TyKind::Ident(name) = &ty.kind {
            ty = self.get_ty(name);
        }
        ty
    }

    fn visit_value(&mut self, value: &Value, ty: &'t ir::Ty) -> Result<Self::Res, crate::Error> {
        let mut ty = ty;
        while let ir::TyKind::Ident(ident) = &ty.kind {
            if let Some(result) = self.visit_ident(value, ident)? {
                return Ok(result);
            }
            ty = self.get_ty(ident);
        }

        match &ty.kind {
            ir::TyKind::Primitive(ir::TyPrimitive::prim8) => {
                self.visit_int8(value.expect_prim8()? as i8)
            }
            ir::TyKind::Primitive(ir::TyPrimitive::prim16) => {
                self.visit_int16(value.expect_prim16()? as i16)
            }
            ir::TyKind::Primitive(ir::TyPrimitive::prim32) => {
                self.visit_int32(value.expect_prim32()? as i32)
            }
            ir::TyKind::Primitive(ir::TyPrimitive::prim64) => {
                self.visit_int64(value.expect_prim64()? as i64)
            }
            ir::TyKind::Tuple(ty_fields) => {
                let fields = value.expect_tuple()?;
                self.visit_tuple(fields, ty_fields)
            }
            ir::TyKind::Array(ty_items) => {
                let items = value.expect_array()?;
                self.visit_array(items, ty_items)
            }
            ir::TyKind::Enum(variants) => {
                let (tag, inner) = value.expect_enum()?;
                self.visit_enum(tag, inner, variants)
            }

            ir::TyKind::Function(..) => Err(Error::InvalidType),
            ir::TyKind::Ident(..) => Err(Error::Bug),
        }
    }

    fn visit_ident(
        &mut self,
        value: &Value,
        ident: &'t ir::Path,
    ) -> Result<Option<Self::Res>, crate::Error> {
        if ident.is(&["std", "Bool"]) {
            let v = value.expect_prim8()? != 0;
            self.visit_bool(v).map(Some)
        } else if ident.is(&["std", "Int8"]) {
            let v = value.expect_prim8()? as i8;
            self.visit_int8(v).map(Some)
        } else if ident.is(&["std", "Int16"]) {
            let v = value.expect_prim16()? as i16;
            self.visit_int16(v).map(Some)
        } else if ident.is(&["std", "Int32"]) {
            let v = value.expect_prim32()? as i32;
            self.visit_int32(v).map(Some)
        } else if ident.is(&["std", "Int64"]) {
            let v = value.expect_prim64()? as i64;
            self.visit_int64(v).map(Some)
        } else if ident.is(&["std", "Uint8"]) {
            let v = value.expect_prim8()?;
            self.visit_uint8(v).map(Some)
        } else if ident.is(&["std", "Uint16"]) {
            let v = value.expect_prim16()?;
            self.visit_uint16(v).map(Some)
        } else if ident.is(&["std", "Uint32"]) {
            let v = value.expect_prim32()?;
            self.visit_uint32(v).map(Some)
        } else if ident.is(&["std", "Uint64"]) {
            let v = value.expect_prim64()?;
            self.visit_uint64(v).map(Some)
        } else if ident.is(&["std", "Float32"]) {
            let v = f32::from_ne_bytes(value.expect_prim32()?.to_ne_bytes());
            self.visit_float32(v).map(Some)
        } else if ident.is(&["std", "Float64"]) {
            let v = f64::from_ne_bytes(value.expect_prim64()?.to_ne_bytes());
            self.visit_float64(v).map(Some)
        } else if ident.is(&["std", "Text"]) {
            let v = value.expect_text_cloned()?;
            self.visit_text(&v).map(Some)
        } else if ident.is(&["std", "Date"]) {
            let v = value.expect_prim32()? as i32;
            self.visit_date(v).map(Some)
        } else if ident.is(&["std", "Time"]) {
            let v = value.expect_prim64()? as i64;
            self.visit_time(v).map(Some)
        } else if ident.is(&["std", "Timestamp"]) {
            let v = value.expect_prim64()? as i64;
            self.visit_timestamp(v).map(Some)
        } else if ident.is(&["std", "Decimal"]) {
            let v = value.expect_prim64()? as i64;
            self.visit_decimal(v).map(Some)
        } else {
            Ok(None)
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
    fn visit_text(&mut self, v: &str) -> Result<Self::Res, crate::Error>;
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
        fields: &[Value],
        ty_fields: &'t [ir::TyTupleField],
    ) -> Result<Self::Res, crate::Error>;

    fn visit_array(
        &mut self,
        items: &[Value],
        ty_items: &'t ir::Ty,
    ) -> Result<Self::Res, crate::Error>;

    fn visit_enum(
        &mut self,
        tag: usize,
        inner: &Value,
        ty_variants: &'t [ir::TyEnumVariant],
    ) -> Result<Self::Res, crate::Error>;
}
