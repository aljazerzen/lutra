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
        let ty = self.get_mat_ty(ty);

        match &ty.kind {
            ir::TyKind::Primitive(ir::TyPrimitive::bool) => {
                self.visit_bool(value.expect_prim8()? != 0)
            }
            ir::TyKind::Primitive(ir::TyPrimitive::int8) => {
                self.visit_int8(value.expect_prim8()? as i8)
            }
            ir::TyKind::Primitive(ir::TyPrimitive::uint8) => {
                self.visit_uint8(value.expect_prim8()?)
            }
            ir::TyKind::Primitive(ir::TyPrimitive::int16) => {
                self.visit_int16(value.expect_prim16()? as i16)
            }
            ir::TyKind::Primitive(ir::TyPrimitive::uint16) => {
                self.visit_uint16(value.expect_prim16()?)
            }
            ir::TyKind::Primitive(ir::TyPrimitive::int32) => {
                self.visit_int32(value.expect_prim32()? as i32)
            }
            ir::TyKind::Primitive(ir::TyPrimitive::uint32) => {
                self.visit_uint32(value.expect_prim32()?)
            }
            ir::TyKind::Primitive(ir::TyPrimitive::float32) => {
                self.visit_float32(f32::from_ne_bytes(value.expect_prim32()?.to_ne_bytes()))
            }
            ir::TyKind::Primitive(ir::TyPrimitive::int64) => {
                self.visit_int64(value.expect_prim64()? as i64)
            }
            ir::TyKind::Primitive(ir::TyPrimitive::uint64) => {
                self.visit_uint64(value.expect_prim64()?)
            }
            ir::TyKind::Primitive(ir::TyPrimitive::float64) => {
                self.visit_float64(f64::from_ne_bytes(value.expect_prim64()?.to_ne_bytes()))
            }
            ir::TyKind::Primitive(ir::TyPrimitive::text) => self.visit_text(value.expect_text()?),
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
