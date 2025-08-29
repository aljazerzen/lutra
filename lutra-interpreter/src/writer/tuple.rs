use std::{borrow, vec};

use lutra_bin::ir;

use crate::Data;

use super::SeveredBodies;

#[derive(Debug)]
pub struct TupleWriter<'t> {
    fields_layouts: borrow::Cow<'t, [(u32, &'t [u32])]>,

    buf: vec::Vec<u8>,

    next: usize,
    fields_bodies: vec::Vec<SeveredBodies>,
}

impl<'t> TupleWriter<'t> {
    pub fn new_for_ty(ty: &'t ir::Ty) -> Self {
        let ir::TyKind::Tuple(field_tys) = &ty.kind else {
            panic!()
        };
        Self::new_for_tys(field_tys.iter().map(|f| &f.ty))
    }

    pub fn new_for_tys(field_tys: impl Iterator<Item = &'t ir::Ty>) -> Self {
        let fields_layouts = field_tys
            .map(|f| {
                let layout = f.layout.as_ref().unwrap();

                (layout.head_size.div_ceil(8), layout.body_ptrs.as_slice())
            })
            .collect();

        Self::new(fields_layouts)
    }

    pub fn new(fields_layouts: borrow::Cow<'t, [(u32, &'t [u32])]>) -> Self {
        TupleWriter {
            fields_layouts,
            next: 0,
            buf: vec::Vec::new(),
            fields_bodies: vec![],
        }
    }

    pub fn write_field(&mut self, field: Data) {
        if self.next >= self.fields_layouts.len() {
            panic!()
        }
        let (head_bytes, body_ptrs) = self.fields_layouts[self.next];

        let body = super::write_head(&mut self.buf, field, head_bytes, body_ptrs);
        if let Some(body) = body {
            self.fields_bodies.push(body);
        }

        self.next += 1;
    }

    pub fn finish(mut self) -> Data {
        // write body offsets for each field
        let mut total_len = self.buf.len();
        for body in &self.fields_bodies {
            body.write_pointers(&mut self.buf, total_len);
            total_len += body.buf.remaining();
        }

        // construct data
        let mut data = Data::new(self.buf);
        for body in self.fields_bodies {
            data = data.combine(body.buf);
        }
        data
    }
}
