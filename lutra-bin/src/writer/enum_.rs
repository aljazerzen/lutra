use bytes::BufMut;

use crate::vec;

use crate::ir;
use crate::layout;
use crate::Data;

#[derive(Debug)]
pub struct EnumWriter<'t> {
    buf: vec::Vec<u8>,

    tag_bytes: u32,
    variants: vec::Vec<layout::EnumVariantFormat>,
    variants_inner_layouts: vec::Vec<(u32, &'t [u32])>,
}

impl<'t> EnumWriter<'t> {
    pub fn new_for_ty(ty: &'t ir::Ty) -> Self {
        let ir::TyKind::Enum(variants) = &ty.kind else {
            panic!()
        };
        let head = layout::enum_head_format(variants);
        let variant_formats = variants
            .iter()
            .map(|v| layout::enum_variant_format(&head, &v.ty))
            .collect();

        let variants_inner_layouts = variants
            .iter()
            .map(|v| {
                let layout = v.ty.layout.as_ref().unwrap();
                (layout.head_size.div_ceil(8), layout.body_ptrs.as_slice())
            })
            .collect();

        Self::new(head.tag_bytes, variant_formats, variants_inner_layouts)
    }

    pub fn new(
        tag_bytes: u32,
        variants: vec::Vec<layout::EnumVariantFormat>,
        variants_inner_layouts: vec::Vec<(u32, &'t [u32])>,
    ) -> Self {
        EnumWriter {
            buf: vec::Vec::new(),
            tag_bytes,
            variants,
            variants_inner_layouts,
        }
    }

    pub fn write(mut self, tag: u64, inner: Data) -> Data {
        if tag as usize >= self.variants.len() {
            panic!()
        }

        let variant = &self.variants[tag as usize];

        // tag
        self.buf
            .extend(&tag.to_le_bytes()[0..self.tag_bytes as usize]);

        // pointer
        if !variant.is_inline {
            self.buf.extend(4_u32.to_le_bytes());
        }

        // inner head
        let (head_bytes, body_ptrs) = self.variants_inner_layouts[tag as usize];
        let inner_body = super::write_head(&mut self.buf, inner, head_bytes, body_ptrs);
        if let Some(inner_body) = &inner_body {
            let buf_offset = self.buf.len();
            inner_body.write_pointers(&mut self.buf, buf_offset);
        }

        // padding
        if variant.padding_bytes > 0 {
            self.buf.put_bytes(0, variant.padding_bytes as usize);
        }

        let mut data = Data::new(self.buf);

        // inner body
        if let Some(body) = inner_body {
            data = data.combine(body.buf);
        }
        data
    }
}
