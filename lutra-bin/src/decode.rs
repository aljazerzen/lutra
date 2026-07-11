use crate::{string, vec};

use crate::{Layout, ReaderExt, Result, layout, reader};

pub trait Decode: Sized + Layout {
    fn decode(buffer: &[u8]) -> Result<Self>;
}
impl Decode for bool {
    fn decode(r: &[u8]) -> Result<Self> {
        let [v] = r.read_const::<1>();
        Ok(v != 0)
    }
}

impl Decode for i8 {
    fn decode(r: &[u8]) -> Result<Self> {
        Ok(i8::from_le_bytes(r.read_const()))
    }
}
impl Decode for i16 {
    fn decode(r: &[u8]) -> Result<Self> {
        Ok(i16::from_le_bytes(r.read_const()))
    }
}
impl Decode for i32 {
    fn decode(r: &[u8]) -> Result<Self> {
        Ok(i32::from_le_bytes(r.read_const()))
    }
}
impl Decode for i64 {
    fn decode(r: &[u8]) -> Result<Self> {
        Ok(i64::from_le_bytes(r.read_const()))
    }
}
impl Decode for u8 {
    fn decode(r: &[u8]) -> Result<Self> {
        Ok(u8::from_le_bytes(r.read_const()))
    }
}
impl Decode for u16 {
    fn decode(r: &[u8]) -> Result<Self> {
        Ok(u16::from_le_bytes(r.read_const()))
    }
}
impl Decode for u32 {
    fn decode(r: &[u8]) -> Result<Self> {
        Ok(u32::from_le_bytes(r.read_const()))
    }
}
impl Decode for u64 {
    fn decode(r: &[u8]) -> Result<Self> {
        Ok(u64::from_le_bytes(r.read_const()))
    }
}
impl Decode for f32 {
    fn decode(r: &[u8]) -> Result<Self> {
        Ok(f32::from_le_bytes(r.read_const()))
    }
}
impl Decode for f64 {
    fn decode(r: &[u8]) -> Result<Self> {
        Ok(f64::from_le_bytes(r.read_const()))
    }
}

impl Decode for string::String {
    fn decode(r: &[u8]) -> Result<Self> {
        let (offset, len) = reader::ArrayReader::<&[u8]>::read_head(r);

        let buf = r.skip(offset).read_n(len).to_vec();
        Ok(string::String::from_utf8(buf).unwrap())
    }
}
impl<E: Decode> Decode for vec::Vec<E> {
    fn decode(r: &[u8]) -> Result<Self> {
        decode_array(r, E::head_size().div_ceil(8), E::decode)
    }
}
impl<E: Decode> Decode for Option<E> {
    fn decode(buf: &[u8]) -> Result<Self> {
        let format = layout::option_head_format::<E>();

        Ok(match format.decode_tag(buf) {
            0 => None,
            1 => Some(E::decode(
                format.variant(1, E::head_size() as u32).decode_inner(buf),
            )?),
            _ => return Err(crate::Error::InvalidData),
        })
    }
}
impl<T: Decode, E: Decode> Decode for core::result::Result<T, E> {
    fn decode(buf: &[u8]) -> Result<Self> {
        let format = layout::result_head_format::<T, E>();

        Ok(match format.decode_tag(buf) {
            0 => Ok(T::decode(
                format.variant(0, T::head_size() as u32).decode_inner(buf),
            )?),
            1 => Err(E::decode(
                format.variant(1, E::head_size() as u32).decode_inner(buf),
            )?),
            _ => return Err(crate::Error::InvalidData),
        })
    }
}
impl Decode for () {
    fn decode(_buf: &[u8]) -> Result<Self> {
        Ok(())
    }
}

/// Decodes an array head and its items, with the item stride given by the caller.
fn decode_array<T>(
    buf: &[u8],
    item_head_bytes: usize,
    decode_item: impl Fn(&[u8]) -> Result<T>,
) -> Result<vec::Vec<T>> {
    let (offset, len) = reader::ArrayReader::<&[u8]>::read_head(buf);
    let mut buf = buf.skip(offset);

    let mut out = vec::Vec::with_capacity(len);
    for _ in 0..len {
        out.push(decode_item(buf)?);
        buf = buf.skip(item_head_bytes);
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Encode;
    use crate::string::String;

    /// Encoded head size must match the layout declared by [Layout],
    /// otherwise containers compute wrong strides and offsets.
    fn assert_head_size<T: Encode + Decode + core::fmt::Debug + PartialEq>(value: T) {
        let mut buf = crate::bytes::BytesMut::new();
        let head = value.encode_head(&mut buf);
        assert_eq!(
            buf.len() * 8,
            T::head_size(),
            "head size mismatch for {value:?}"
        );
        value.encode_body(head, &mut buf);

        assert_eq!(T::decode(&buf).unwrap(), value);
    }

    #[test]
    fn head_size_matches_encoding() {
        assert_head_size(42_u16);
        assert_head_size(String::from("hello"));

        // inline inner
        assert_head_size(Some(42_u16));
        assert_head_size(Option::<u16>::None);
        assert_head_size(Some(()));
        assert_head_size(Option::<u32>::None);
        assert_head_size(core::result::Result::<u16, u8>::Ok(1));
        assert_head_size(core::result::Result::<u16, u8>::Err(2));

        // out-of-line inner
        assert_head_size(Some(String::from("hello")));
        assert_head_size(Option::<String>::None);
        assert_head_size(core::result::Result::<String, u8>::Ok("x".into()));
        assert_head_size(core::result::Result::<String, u8>::Err(2));
        assert_head_size(core::result::Result::<String, ()>::Err(()));
        assert_head_size(Some(Some(String::from("nested"))));
    }

    #[test]
    fn array_of_options_roundtrips() {
        let v: crate::vec::Vec<Option<u16>> = crate::vec![Some(1), Some(2), None, Some(700)];
        assert_eq!(
            <crate::vec::Vec<Option<u16>>>::decode(&v.encode()).unwrap(),
            v
        );

        let v: crate::vec::Vec<Option<String>> =
            crate::vec![Some("a".into()), None, Some("bcd".into())];
        assert_eq!(
            <crate::vec::Vec<Option<String>>>::decode(&v.encode()).unwrap(),
            v
        );

        let v: crate::vec::Vec<core::result::Result<u16, ()>> = crate::vec![Ok(1), Err(()), Ok(3)];
        assert_eq!(
            <crate::vec::Vec<core::result::Result<u16, ()>>>::decode(&v.encode()).unwrap(),
            v
        );
    }

    /// A unit variant is stored without a pointer, in both the generic impls
    /// and the compiler-driven enum format.
    #[test]
    fn unit_variant_has_no_pointer() {
        let format = crate::layout::result_head_format::<String, ()>();
        assert!(format.has_ptr);

        let unit_variant = format.variant(1, 0);
        assert!(!unit_variant.has_ptr);
        assert_eq!(unit_variant.padding_bytes, 4);

        let encoded = core::result::Result::<String, ()>::Err(()).encode();
        assert_eq!(encoded, crate::vec![1, 0, 0, 0, 0]);
    }
}
