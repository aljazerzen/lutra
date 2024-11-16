use lutra_frontend::pr;

use crate::ReversePointer;

pub struct ArrayWriter<'t> {
    item_ty: &'t pr::Ty,

    buf: Vec<u8>,

    count: usize,
    item_bodies: Vec<(ReversePointer, Vec<u8>)>,
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

    pub fn write_item(&mut self, item: Vec<u8>) {
        self.count += 1;

        let layout = self.item_ty.layout.as_ref().unwrap();

        let body = super::write_head(&mut self.buf, &item, layout);
        if let Some((ptr, body)) = body {
            self.item_bodies.push((ptr, body.to_owned()));
        }
    }

    pub fn finish(mut self) -> Vec<u8> {
        // write len
        self.buf[4..8].copy_from_slice(&(self.count as u32).to_le_bytes());

        // write item bodies
        for (ptr, bytes) in self.item_bodies {
            ptr.write(&mut self.buf);
            self.buf.extend(bytes);
        }

        self.buf
    }
}
