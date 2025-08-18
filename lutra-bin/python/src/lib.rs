mod value;

use std::borrow::Cow;

use pyo3::buffer::PyBuffer;
use pyo3::exceptions::PyValueError;
use pyo3::ffi::c_str;
use pyo3::prelude::*;
use pyo3::types::{PyList, PySlice, PyTuple};

/// Main module declaration
#[pymodule(name = "lutra_bin")]
fn main(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<value::Value>()?;
    m.add_class::<BytesMut>()?;

    m.add_class::<UnitCodec>()?;
    m.add_class::<BoolCodec>()?;
    m.add_class::<Int8Codec>()?;
    m.add_class::<Int16Codec>()?;
    m.add_class::<Int32Codec>()?;
    m.add_class::<Int64Codec>()?;
    m.add_class::<Uint8Codec>()?;
    m.add_class::<Uint16Codec>()?;
    m.add_class::<Uint32Codec>()?;
    m.add_class::<Uint64Codec>()?;
    m.add_class::<Float32Codec>()?;
    m.add_class::<Float64Codec>()?;
    m.add_class::<TextCodec>()?;
    m.add_class::<ArrayCodec>()?;
    m.add_class::<EnumCodecHelper>()?;

    ir::register(m)?;
    Ok(())
}

#[pyclass(module = "lutra_bin")]
struct BytesMut {
    inner: lutra_bin::bytes::BytesMut,
}

#[pymethods]
impl BytesMut {
    #[new]
    fn new() -> Self {
        BytesMut {
            inner: lutra_bin::bytes::BytesMut::new(),
        }
    }

    #[allow(clippy::wrong_self_convention)]
    fn into_bytes(&mut self) -> Cow<'_, [u8]> {
        let bytes_mut = std::mem::take(&mut self.inner);
        Cow::from(bytes_mut.freeze().to_vec())
    }
}

#[pyclass(module = "lutra_bin")]
pub struct UnitCodec;

#[pymethods]
impl UnitCodec {
    #[new]
    fn __init__() -> Self {
        Self
    }

    const fn head_bytes(&self) -> usize {
        0
    }

    fn decode<'py>(&self, py: Python<'py>, _buf: PyBuffer<u8>) -> Bound<'py, PyTuple> {
        PyTuple::empty(py)
    }

    fn encode_head<'py>(
        &self,
        py: Python<'py>,
        _value: Bound<'py, PyTuple>,
        _buf: Bound<crate::BytesMut>,
    ) -> Bound<'py, PyTuple> {
        PyTuple::empty(py)
    }

    fn encode_body(
        &self,
        _value: Bound<PyTuple>,
        _residual: Bound<PyTuple>,
        _buf: Bound<'_, crate::BytesMut>,
    ) {
    }
}

macro_rules! prim_pyclass {
    ($class_name: ident, $primitive: ident, $head_size: literal) => {
        #[pyclass(module = "lutra_bin")]
        pub struct $class_name;

        #[pymethods]
        impl $class_name {
            #[new]
            fn __init__() -> Self {
                Self
            }

            const fn head_bytes(&self) -> usize {
                ($head_size as usize).div_ceil(8)
            }

            fn decode(&self, buf: PyBuffer<u8>) -> PyResult<$primitive> {
                use lutra_bin::Decode;
                let buf = buffer_as_slice(&buf)?;

                Ok($primitive::decode(buf).unwrap())
            }

            fn encode_head<'py>(
                &self,
                py: Python<'py>,
                val: $primitive,
                buf: Bound<'py, crate::BytesMut>,
            ) -> PyResult<Bound<'py, PyTuple>> {
                use lutra_bin::Encode;
                let mut bytes_mut = buf.try_borrow_mut()?;
                val.encode_head(&mut bytes_mut.inner);
                Ok(PyTuple::empty(py))
            }

            fn encode_body(
                &self,
                _val: $primitive,
                _r: Bound<PyTuple>,
                _buf: Bound<crate::BytesMut>,
            ) {
            }
        }
    };
}

prim_pyclass!(BoolCodec, bool, 8);
prim_pyclass!(Int8Codec, i8, 8);
prim_pyclass!(Int16Codec, i16, 16);
prim_pyclass!(Int32Codec, i32, 32);
prim_pyclass!(Int64Codec, i64, 64);
prim_pyclass!(Uint8Codec, u8, 8);
prim_pyclass!(Uint16Codec, u16, 16);
prim_pyclass!(Uint32Codec, u32, 32);
prim_pyclass!(Uint64Codec, u64, 64);
prim_pyclass!(Float32Codec, f32, 32);
prim_pyclass!(Float64Codec, f64, 64);

#[pyclass(module = "lutra_bin")]
pub struct TextCodec;

#[pymethods]
impl TextCodec {
    #[new]
    fn __init__() -> Self {
        Self
    }

    const fn head_bytes(&self) -> usize {
        8
    }

    fn decode(&self, buf: PyBuffer<u8>) -> PyResult<String> {
        use lutra_bin::Decode;
        let buf = buffer_as_slice(&buf)?;

        // TODO: use str instead of String to avoid a copy here
        Ok(String::decode(buf).unwrap())
    }

    fn encode_head(&self, value: &str, buf: Bound<crate::BytesMut>) -> PyResult<usize> {
        use lutra_bin::Encode;
        let mut bytes_mut = buf.try_borrow_mut()?;
        let rptr = value.encode_head(&mut bytes_mut.inner);

        Ok(rptr.unwrap())
    }

    fn encode_body(
        &self,
        value: &str,
        residual: usize,
        buf: Bound<'_, crate::BytesMut>,
    ) -> PyResult<()> {
        let rptr = lutra_bin::ReversePointer::new_at(residual);

        use lutra_bin::Encode;
        let mut bytes_mut = buf.try_borrow_mut()?;
        value.encode_body(rptr, &mut bytes_mut.inner);
        Ok(())
    }
}

#[pyclass(module = "lutra_bin")]
pub struct ArrayCodec {
    item_codec: Py<PyAny>,
}

#[pymethods]
impl ArrayCodec {
    #[new]
    fn __init__(item_codec: Py<PyAny>) -> Self {
        Self { item_codec }
    }

    fn decode(&self, py: Python, buf: Bound<PyAny>) -> PyResult<Vec<Py<PyAny>>> {
        // decode head
        let py_buf = PyBuffer::<u8>::get(&buf)?;
        let buf_slice = buffer_as_slice(&py_buf)?;
        let (offset, len) = lutra_bin::ArrayReader::read_head(buf_slice);

        // prepare memoryview of buf
        let buf = py.eval(c_str!("memoryview"), None, None)?.call1((buf,))?;

        let item_head_bytes = self.item_codec.call_method0(py, "head_bytes")?;
        let item_head_bytes: usize = item_head_bytes.extract(py)?;

        let mut items = Vec::with_capacity(len);
        for i in 0..len {
            let start = offset + i * item_head_bytes;
            let end = buf.len()?;

            // compute buf[start:]
            let slice = PySlice::new(py, start as isize, end as isize, 1);
            let item_buf = buf.get_item(slice)?;

            // call item_type.decode(item_buf)
            items.push(self.item_codec.call_method1(py, "decode", (item_buf,))?);
        }

        Ok(items)
    }

    fn encode_head(&self, list: Bound<PyList>, buf: Bound<BytesMut>) -> PyResult<usize> {
        use lutra_bin::bytes::BufMut;
        let mut bytes_mut = buf.try_borrow_mut()?;
        let buf = &mut bytes_mut.inner;

        let rptr = lutra_bin::ReversePointer::new(buf);
        buf.put_u32_le(list.len() as u32);

        Ok(rptr.unwrap())
    }

    fn encode_body(
        &self,
        py: Python,
        list: Bound<PyList>,
        residual: usize,
        buf: Bound<BytesMut>,
    ) -> PyResult<()> {
        {
            let mut bytes_mut = buf.try_borrow_mut()?;
            let buf = &mut bytes_mut.inner;
            let rptr = lutra_bin::ReversePointer::new_at(residual);
            rptr.write_cur_len(buf);
        }

        let item_c = &self.item_codec;

        let mut residuals = Vec::with_capacity(list.len());
        for item in list.iter() {
            residuals.push(item_c.call_method1(py, "encode_head", (item, buf.clone()))?)
        }

        for (item, residual) in std::iter::zip(list.iter(), residuals) {
            item_c.call_method1(py, "encode_body", (item, residual, buf.clone()))?;
        }
        Ok(())
    }
}

/// EnumCoded does not implement the Codec protocol, but is more of a helper.
#[pyclass(module = "lutra_bin")]
pub struct EnumCodecHelper {
    format: lutra_bin::layout::EnumFormat,
}

#[pymethods]
impl EnumCodecHelper {
    #[new]
    fn new(enum_format: &[u8]) -> Self {
        use lutra_bin::Decode;
        Self {
            format: lutra_bin::layout::EnumFormat::decode(enum_format).unwrap(),
        }
    }

    fn decode_head(&self, buf: Bound<PyAny>) -> PyResult<(u64, usize)> {
        let py_buf = PyBuffer::<u8>::get(&buf)?;
        let buf_slice = buffer_as_slice(&py_buf)?;

        let (tag, inner) =
            lutra_bin::decode_enum_head(buf_slice, self.format.tag_bytes, self.format.has_ptr);
        let inner_offset = buf_slice.len() - inner.len();
        Ok((tag, inner_offset))
    }
    fn encode_head_tag(&self, tag: u64, buf: Bound<BytesMut>) -> PyResult<Option<usize>> {
        let mut bytes_mut = buf.try_borrow_mut()?;
        let buf = &mut bytes_mut.inner;

        let residual = lutra_bin::encode_enum_head_tag(&self.format, tag, buf);
        Ok(residual.map(|r| r.unwrap()))
    }
    fn encode_head_padding(&self, tag: u64, buf: Bound<BytesMut>) -> PyResult<()> {
        let mut bytes_mut = buf.try_borrow_mut()?;
        let buf = &mut bytes_mut.inner;

        lutra_bin::encode_enum_head_padding(&self.format, tag, buf);
        Ok(())
    }
    fn encode_body_ptr(&self, residual: Option<usize>, buf: Bound<BytesMut>) -> PyResult<()> {
        let Some(rev_ptr) = residual else {
            return Ok(());
        };
        let rev_ptr = lutra_bin::ReversePointer::new_at(rev_ptr);

        let mut bytes_mut = buf.try_borrow_mut()?;
        let buf = &mut bytes_mut.inner;

        rev_ptr.write_cur_len(buf);
        Ok(())
    }
}

mod ir {
    use std::borrow::Cow;

    use lutra_bin::{Decode, Encode};
    use pyo3::{prelude::*, types::PyType};

    pub fn register(p: &Bound<'_, PyModule>) -> PyResult<()> {
        let m = PyModule::new(p.py(), "ir")?;
        m.add_class::<Ty>()?;
        p.add_submodule(&m)
    }

    #[pyclass(module = "ir")]
    pub struct Ty(pub(crate) lutra_bin::ir::Ty);

    #[pymethods]
    impl Ty {
        #[classmethod]
        fn decode(_cls: &Bound<'_, PyType>, bytes: &[u8]) -> Ty {
            let ty = lutra_bin::ir::Ty::decode(bytes).unwrap();
            Ty(ty)
        }

        fn encode(&self) -> Cow<'static, [u8]> {
            self.0.encode().into()
        }
    }
}

fn buffer_as_slice(buffer: &PyBuffer<u8>) -> PyResult<&[u8]> {
    if !buffer.readonly() {
        return Err(PyValueError::new_err("Must be read-only byte buffer."));
    }
    if buffer.dimensions() != 1 {
        return Err(PyValueError::new_err("Expected 1-dimensional array."));
    }
    // Note: this is probably superfluous for 1D array
    if !buffer.is_c_contiguous() {
        return Err(PyValueError::new_err("Expected c-contiguous array."));
    }
    if buffer.len_bytes() == 0 {
        return Ok(&[]);
    }

    let len = buffer.item_count();
    let data = buffer.buf_ptr() as *const u8;
    Ok(unsafe { std::slice::from_raw_parts(data, len) })
}
