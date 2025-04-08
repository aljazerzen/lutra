use crate::vec;

use crate::ir;
use crate::layout;
use crate::Data;

#[derive(Debug)]
pub struct EnumWriter {
    tag_bytes: u32,
    has_ptr: bool,
    variants: vec::Vec<layout::EnumVariantFormat>,
}

impl EnumWriter {
    pub fn new_for_ty(ty: &ir::Ty) -> Self {
        let ir::TyKind::Enum(variants) = &ty.kind else {
            panic!()
        };
        let head = layout::enum_head_format(variants);
        let variant_formats = variants
            .iter()
            .map(|v| layout::enum_variant_format(&head, &v.ty))
            .collect();

        Self::new(head.tag_bytes, head.has_ptr, variant_formats)
    }

    pub fn new(
        tag_bytes: u32,
        has_ptr: bool,
        variants: vec::Vec<layout::EnumVariantFormat>,
    ) -> Self {
        EnumWriter {
            tag_bytes,
            has_ptr,
            variants,
        }
    }

    pub fn write(self, tag: u64, inner: Data) -> Data {
        if tag as usize >= self.variants.len() {
            panic!()
        }
        let variant = &self.variants[tag as usize];

        let tag = &tag.to_le_bytes()[0..self.tag_bytes as usize];
        Self::write_variant(tag, self.has_ptr, variant.padding_bytes, inner)
    }

    pub fn write_variant(
        tag: &[u8],
        has_ptr: bool,
        variant_padding_bytes: u32,
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
            let inner_oversized = inner.len() >= 4;
            let mut data = Data::new(buf).combine(inner);

            // padding
            // optimization: skip if inner was oversized already
            if variant_padding_bytes > 0 && !inner_oversized {
                data = data.combine(Data::new(vec![0; variant_padding_bytes as usize]));
            }

            data
        }
    }
}
