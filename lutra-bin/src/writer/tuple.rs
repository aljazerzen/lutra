use lutra_frontend::pr;

use crate::ReversePointer;

pub struct TupleWriter<'t> {
    fields_ty: &'t [pr::TyTupleField],

    buf: Vec<u8>,

    next: usize,
    fields_bodies: Vec<(ReversePointer, Vec<u8>)>,
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

    pub fn write_field(&mut self, field: Vec<u8>) {
        if self.next >= self.fields_ty.len() {
            panic!()
        }

        let layout = self.fields_ty[self.next].ty.layout.as_ref().unwrap();

        let body = super::write_head(&mut self.buf, &field, layout);
        if let Some((ptr, body)) = body {
            self.fields_bodies.push((ptr, body.to_owned()));
        }

        self.next += 1;
    }

    pub fn finish(mut self) -> Vec<u8> {
        // write field bodies
        for (ptr, bytes) in self.fields_bodies {
            ptr.write(&mut self.buf);
            self.buf.extend(bytes);
        }

        self.buf
    }
}
