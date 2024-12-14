use crate::ir;
use crate::Data;

use super::SeveredBodies;

#[derive(Debug)]
pub struct TupleWriter<'t> {
    fields_ty: &'t [ir::TyTupleField],

    buf: Vec<u8>,

    next: usize,
    fields_bodies: Vec<SeveredBodies>,
}

impl<'t> TupleWriter<'t> {
    pub fn new(ty: &'t ir::Ty) -> Self {
        let ir::TyKind::Tuple(fields_ty) = &ty.kind else {
            panic!()
        };

        TupleWriter {
            fields_ty: fields_ty.as_slice(),
            next: 0,
            buf: Vec::new(),
            fields_bodies: vec![],
        }
    }

    pub fn write_field(&mut self, field: Data) {
        if self.next >= self.fields_ty.len() {
            panic!()
        }

        let layout = self.fields_ty[self.next].ty.layout.as_ref().unwrap();

        let body = super::write_head(&mut self.buf, field, layout);
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
            total_len += body.buf.len();
        }

        // construct data
        let mut data = Data::new(self.buf);
        for body in self.fields_bodies {
            data = data.combine(body.buf);
        }
        data
    }
}
