use lutra_frontend::pr;

use crate::{Data, ReversePointer};

pub struct TupleWriter<'t> {
    fields_ty: &'t [pr::TyTupleField],

    buf: Vec<u8>,

    next: usize,
    fields_bodies: Vec<(ReversePointer, Data)>,
}

impl<'t> TupleWriter<'t> {
    pub fn new(ty: &'t pr::Ty) -> Self {
        let pr::TyKind::Tuple(fields_ty) = &ty.kind else {
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
        if let Some((ptr, body)) = body {
            self.fields_bodies.push((ptr, body.to_owned()));
        }

        self.next += 1;
    }

    pub fn finish(mut self) -> Data {
        // write body offsets for each field
        let mut total_len = self.buf.len();
        for (ptr, d) in &self.fields_bodies {
            ptr.write(&mut self.buf, total_len);
            total_len += d.len();
        }

        // construct data
        let mut data = Data::new(self.buf);
        for (_, d) in self.fields_bodies {
            data = data.combine(d);
        }
        data
    }
}
