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
        let variant = &self.format.variants[tag as usize];

        let tag = &tag.to_le_bytes()[0..self.format.tag_bytes as usize];
        Self::write_variant(
            tag,
            self.format.inner_bytes,
            self.format.has_ptr,
            variant.padding_bytes,
            inner,
        )
    }

    pub fn write_variant(
        tag: &[u8],
        inner_bytes: u8,
        has_ptr: bool,
        variant_padding_bytes: u8,
        inner: Data,
    ) -> Data {
        // tag
        let mut buf = tag.to_vec();

        if has_ptr {
            // pointer
            buf.extend(4_u32.to_le_bytes());

            // inner
            Data::new(buf).combine(inner)

            // If variant is unit, instead of padding we write ptr [4 0 0 0].
            // We also combine with inner Data, as it should be empty anyway.
            // This allows us to drop `variant.is_unit` param.
        } else {
            // inner
            buf.extend(inner.chunk());

            let expected_len = tag.len() + inner_bytes as usize;

            if buf.len() < expected_len {
                // padding
                buf.put_bytes(0, variant_padding_bytes as usize);
            } else {
                // clip, if inner had trailing data
                buf.truncate(expected_len);
            }

            Data::new(buf)
        }
    }
}
