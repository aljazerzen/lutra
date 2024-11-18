use lutra_frontend::pr;

use crate::{Data, ReversePointer};

pub struct ArrayWriter<'t> {
    item_ty: &'t pr::Ty,

    buf: Vec<u8>,

    count: usize,
    item_bodies: Vec<(ReversePointer, Data)>,
}

impl<'t> ArrayWriter<'t> {
    pub fn new(ty: &'t pr::Ty) -> Self {
        let pr::TyKind::Array(item_ty) = &ty.kind else {
            panic!()
        };

        ArrayWriter {
            item_ty: item_ty.as_ref(),
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

        let layout = self.item_ty.layout.as_ref().unwrap();

        let body = super::write_head(&mut self.buf, item, layout);
        if let Some(body) = body {
            self.item_bodies.push(body);
        }
    }

    pub fn finish(mut self) -> Data {
        // write len
        self.buf[4..8].copy_from_slice(&(self.count as u32).to_le_bytes());

        // write body offsets of each item
        let mut total_len = self.buf.len();
        for (ptr, d) in &self.item_bodies {
            ptr.write(&mut self.buf, total_len);
            total_len += d.len();
        }

        // construct data
        let mut data = Data::new(self.buf);
        for (_, d) in self.item_bodies {
            data = data.combine(d);
        }
        data
    }
}
