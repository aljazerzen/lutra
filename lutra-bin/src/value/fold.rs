use crate::ir;

use crate::Value;

#[allow(dead_code)]
pub trait ValueVisitor {
    type Res;

    fn visit_value(&mut self, value: &Value, ty: &ir::Ty) -> Result<Self::Res, crate::Error> {
        match value {
            Value::Bool(v) => {
                super::expect_ty_primitive(ty, ir::TyPrimitive::bool)?;
                self.visit_bool(*v)
            }
            Value::Int8(v) => {
                super::expect_ty_primitive(ty, ir::TyPrimitive::int8)?;
                self.visit_int8(*v)
            }
            Value::Int16(v) => {
                super::expect_ty_primitive(ty, ir::TyPrimitive::int16)?;
                self.visit_int16(*v)
            }
            Value::Int32(v) => {
                super::expect_ty_primitive(ty, ir::TyPrimitive::int32)?;
                self.visit_int32(*v)
            }
            Value::Int64(v) => {
                super::expect_ty_primitive(ty, ir::TyPrimitive::int64)?;
                self.visit_int64(*v)
            }
            Value::Uint8(v) => {
                super::expect_ty_primitive(ty, ir::TyPrimitive::uint8)?;
                self.visit_uint8(*v)
            }
            Value::Uint16(v) => {
                super::expect_ty_primitive(ty, ir::TyPrimitive::uint16)?;
                self.visit_uint16(*v)
            }
            Value::Uint32(v) => {
                super::expect_ty_primitive(ty, ir::TyPrimitive::uint32)?;
                self.visit_uint32(*v)
            }
            Value::Uint64(v) => {
                super::expect_ty_primitive(ty, ir::TyPrimitive::uint64)?;
                self.visit_uint64(*v)
            }
            Value::Float32(v) => {
                super::expect_ty_primitive(ty, ir::TyPrimitive::float32)?;
                self.visit_float32(*v)
            }
            Value::Float64(v) => {
                super::expect_ty_primitive(ty, ir::TyPrimitive::float64)?;
                self.visit_float64(*v)
            }
            Value::Text(v) => {
                super::expect_ty_primitive(ty, ir::TyPrimitive::text)?;
                self.visit_text(v)
            }
            Value::Tuple(fields) => {
                let t = super::expect_ty(ty, |k| k.as_tuple(), "tuple")?;
                self.visit_tuple(fields, t)
            }
            Value::Array(items) => {
                let t = super::expect_ty(ty, |k| k.as_array(), "array")?;
                self.visit_array(items, t)
            }
            Value::Enum(tag, inner) => {
                let t = super::expect_ty(ty, |k| k.as_enum(), "enum")?;
                self.visit_enum(*tag, inner, t)
            }
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
        ty_fields: &[ir::TyTupleField],
    ) -> Result<Self::Res, crate::Error>;

    fn visit_array(
        &mut self,
        items: &[Value],
        ty_items: &ir::Ty,
    ) -> Result<Self::Res, crate::Error>;

    fn visit_enum(
        &mut self,
        tag: usize,
        inner: &Value,
        ty_variants: &[ir::TyEnumVariant],
    ) -> Result<Self::Res, crate::Error>;
}
