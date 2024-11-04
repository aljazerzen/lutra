use lutra_parser::pr::{self, TyTupleField};

use crate::Value;

pub trait ValueVisitor {
    type Res;

    fn visit_value(&mut self, value: &Value, ty: &pr::Ty) -> Result<Self::Res, crate::Error> {
        match value {
            Value::Int(v) => {
                super::expect_ty_primitive(ty, pr::PrimitiveSet::Int)?;
                self.visit_int(*v)
            }
            Value::Float(v) => {
                super::expect_ty_primitive(ty, pr::PrimitiveSet::Float)?;
                self.visit_float(*v)
            }
            Value::Bool(v) => {
                super::expect_ty_primitive(ty, pr::PrimitiveSet::Bool)?;
                self.visit_bool(*v)
            }
            Value::Text(v) => {
                super::expect_ty_primitive(ty, pr::PrimitiveSet::Text)?;
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

    fn visit_int(&mut self, v: i64) -> Result<Self::Res, crate::Error>;
    fn visit_float(&mut self, v: f64) -> Result<Self::Res, crate::Error>;
    fn visit_bool(&mut self, v: bool) -> Result<Self::Res, crate::Error>;
    fn visit_text(&mut self, v: &str) -> Result<Self::Res, crate::Error>;

    fn visit_tuple(
        &mut self,
        fields: &[Value],
        ty_fields: &[TyTupleField],
    ) -> Result<Self::Res, crate::Error>;

    fn visit_array(
        &mut self,
        items: &[Value],
        ty_items: &pr::Ty,
    ) -> Result<Self::Res, crate::Error>;

    fn visit_enum(
        &mut self,
        tag: usize,
        inner: &Value,
        ty_variants: &[(String, pr::Ty)],
    ) -> Result<Self::Res, crate::Error>;
}
