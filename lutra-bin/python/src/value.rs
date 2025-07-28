use pyo3::prelude::*;

// TODO: exposing Value to Python is suboptimal, because when accessing nested
// Values, they need to be copied. I cannot think of a way around this, because
// we have no way of representing a partially borrowed Value.

// So maybe the way forward here is to just scrap Value and expose a way of
// converting to dicts and lists.

#[pyclass]
pub struct Value(lutra_bin::Value);

#[pymethods]
impl Value {
    #[staticmethod]
    fn decode(bytes: &[u8], ty: &crate::ir::Ty) -> PyResult<Value> {
        let ty_defs = &[];

        let value = lutra_bin::Value::decode(bytes, &ty.0, ty_defs).unwrap();
        Ok(Value(value))
    }

    fn encode(&self, ty: &crate::ir::Ty) -> PyResult<Vec<u8>> {
        let ty_defs = &[];

        let bytes = self.0.encode(&ty.0, ty_defs).unwrap();
        Ok(bytes)
    }

    fn as_bool(&self) -> PyResult<bool> {
        match &self.0 {
            lutra_bin::Value::Bool(v) => Ok(*v),
            _ => Err(pyo3::exceptions::PyValueError::new_err("expected bool")),
        }
    }
    fn as_int8(&self) -> PyResult<i8> {
        match &self.0 {
            lutra_bin::Value::Int8(v) => Ok(*v),
            _ => Err(pyo3::exceptions::PyValueError::new_err("expected int8")),
        }
    }
    fn as_int16(&self) -> PyResult<i16> {
        match &self.0 {
            lutra_bin::Value::Int16(v) => Ok(*v),
            _ => Err(pyo3::exceptions::PyValueError::new_err("expected int16")),
        }
    }
    fn as_int32(&self) -> PyResult<i32> {
        match &self.0 {
            lutra_bin::Value::Int32(v) => Ok(*v),
            _ => Err(pyo3::exceptions::PyValueError::new_err("expected int32")),
        }
    }
    fn as_int64(&self) -> PyResult<i64> {
        match &self.0 {
            lutra_bin::Value::Int64(v) => Ok(*v),
            _ => Err(pyo3::exceptions::PyValueError::new_err("expected int64")),
        }
    }
    fn as_uint8(&self) -> PyResult<u8> {
        match &self.0 {
            lutra_bin::Value::Uint8(v) => Ok(*v),
            _ => Err(pyo3::exceptions::PyValueError::new_err("expected uint8")),
        }
    }
    fn as_uint16(&self) -> PyResult<u16> {
        match &self.0 {
            lutra_bin::Value::Uint16(v) => Ok(*v),
            _ => Err(pyo3::exceptions::PyValueError::new_err("expected uint16")),
        }
    }
    fn as_uint32(&self) -> PyResult<u32> {
        match &self.0 {
            lutra_bin::Value::Uint32(v) => Ok(*v),
            _ => Err(pyo3::exceptions::PyValueError::new_err("expected uint32")),
        }
    }
    fn as_uint64(&self) -> PyResult<u64> {
        match &self.0 {
            lutra_bin::Value::Uint64(v) => Ok(*v),
            _ => Err(pyo3::exceptions::PyValueError::new_err("expected uint64")),
        }
    }
    fn as_float32(&self) -> PyResult<f32> {
        match &self.0 {
            lutra_bin::Value::Float32(v) => Ok(*v),
            _ => Err(pyo3::exceptions::PyValueError::new_err("expected float32")),
        }
    }
    fn as_float64(&self) -> PyResult<f64> {
        match &self.0 {
            lutra_bin::Value::Float64(v) => Ok(*v),
            _ => Err(pyo3::exceptions::PyValueError::new_err("expected float64")),
        }
    }
    fn as_text(&self) -> PyResult<&str> {
        match &self.0 {
            lutra_bin::Value::Text(text) => Ok(text),
            _ => Err(pyo3::exceptions::PyValueError::new_err("expected text")),
        }
    }
}
