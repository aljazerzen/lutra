use crate::ir;

use crate::Data;

use super::SeveredBodies;

pub struct ArrayWriter<'t> {
    item_head_bytes: usize,
    item_body_ptrs: &'t [u32],

    buf: Vec<u8>,

    count: usize,
    item_bodies: Vec<SeveredBodies>,
}

impl<'t> ArrayWriter<'t> {
    pub fn new_for_ty(ty: &'t ir::Ty) -> Self {
        let ir::TyKind::Array(item_ty) = &ty.kind else {
            panic!()
        };
        let layout = item_ty.layout.as_ref().unwrap();

        let head_bytes = layout.head_size.div_ceil(8) as usize;
        let body_ptrs = layout.body_ptrs.as_slice();

        Self::new(head_bytes, body_ptrs)
    }

    pub fn new(item_head_bytes: usize, item_body_ptrs: &'t [u32]) -> Self {
        ArrayWriter {
            item_head_bytes,
            item_body_ptrs,
            count: 0,
            buf: vec![
                8, 0, 0, 0, // offset
                0, 0, 0, 0, // len
            ],
            item_bodies: vec![],
        }
    }

    pub fn write_item(&mut self, item: Data) {
        self.count += 1;

        let body = super::write_head(
            &mut self.buf,
            item,
            self.item_head_bytes,
            self.item_body_ptrs,
        );
        if let Some(body) = body {
            self.item_bodies.push(body);
        }
    }

    pub fn finish(mut self) -> Data {
        // write len
        self.buf[4..8].copy_from_slice(&(self.count as u32).to_le_bytes());

        // write body offsets of each item
        let mut total_len = self.buf.len();
        for body in &self.item_bodies {
            body.write_pointers(&mut self.buf, total_len);
            total_len += body.buf.len();
        }

        // construct data
        let mut data = Data::new(self.buf);
        for body in self.item_bodies {
            data = data.combine(body.buf);
        }
        data
    }
}
