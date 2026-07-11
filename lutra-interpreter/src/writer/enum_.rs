use lutra_bin::bytes::BufMut;
use lutra_bin::ir;
use lutra_bin::layout;

use crate::Data;

#[derive(Debug)]
pub struct EnumWriter {
    format: layout::EnumFormat,
}

impl EnumWriter {
    pub fn new_for_ty(ty: &ir::Ty) -> Self {
        let ir::TyKind::Enum(variants) = &ty.kind else {
            panic!()
        };
        Self::new(layout::enum_format(variants, &ty.variants_recursive))
    }

    pub fn new(format: layout::EnumFormat) -> Self {
        EnumWriter { format }
    }

    pub fn write(self, tag: u64, inner: Data) -> Data {
        if tag as usize >= self.format.variants.len() {
            panic!()
        }
        let variant = self.format.variant(tag);

        Self::write_variant(
            &tag.to_le_bytes()[0..variant.tag_bytes as usize],
            self.format.inner_bytes,
            variant.has_ptr,
            variant.padding_bytes as u8,
            inner,
        )
    }

    /// Writes the head of an enum variant, as described by
    /// [layout::EnumVariantHead].
    pub fn write_variant(
        tag: &[u8],
        inner_bytes: u8,
        has_ptr: bool,
        padding_bytes: u8,
        inner: Data,
    ) -> Data {
        // tag
        let mut buf = tag.to_vec();

        if has_ptr {
            // pointer to the body, which follows the head directly
            buf.extend(layout::PTR_BYTES.to_le_bytes());

            // inner
            Data::new(buf).combine(inner)
        } else {
            // inner, which is empty for unit variants
            buf.extend(inner.chunk());

            let expected_len = tag.len() + inner_bytes as usize;

            if buf.len() < expected_len {
                // padding
                buf.put_bytes(0, padding_bytes as usize);
            } else {
                // clip, if inner had trailing data
                buf.truncate(expected_len);
            }

            Data::new(buf)
        }
    }
}
