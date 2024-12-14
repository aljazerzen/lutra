mod array;
mod tuple;

use std::iter::zip;

pub use array::ArrayWriter;
pub use tuple::TupleWriter;

use crate::ir;

use crate::Data;
use crate::ReversePointer;

#[derive(Debug)]
struct SeveredBodies {
    // Buffer that contains the bodies of the type
    buf: Data,

    // Locations of pointers to bodies within the buffer of the head, in bytes.
    body_ptr_offsets: Vec<u32>,

    // Locations of starts of bodies (except the first one) within the buf, in bytes.
    // The first body is skipped, because it is guaranteed to be 0.
    // This is because we skip the buf so the first body is always at the beginning of the buf.
    body_offsets: Vec<u32>,
}

impl SeveredBodies {
    fn offset_head_buf(&mut self, offset: u32) {
        for o in &mut self.body_ptr_offsets {
            *o += offset;
        }
    }

    fn write_pointers(&self, head: &mut [u8], buf_offset: usize) {
        let body_offsets = Some(0).iter().chain(self.body_offsets.iter());

        for (body_ptr_offset, body_offset) in zip(self.body_ptr_offsets.iter(), body_offsets) {
            let from_head_to_body = buf_offset + *body_offset as usize;

            ReversePointer::new_at(*body_ptr_offset as usize).write(head, from_head_to_body);
        }
    }
}

fn extract_head_and_body<'b>(
    buf: &'b Data,
    layout: &ir::TyLayout,
) -> (&'b [u8], Option<SeveredBodies>) {
    let head_bytes = layout.head_size / 8;

    let head = buf.slice(head_bytes as usize);

    let body = if !layout.body_ptrs.is_empty() {
        let mut body_ptrs = layout.body_ptrs.iter();

        let first_body_ptr = body_ptrs.next().unwrap();
        let mut buf = buf.clone();

        let first_body_offset = read_ptr(head, *first_body_ptr);

        // read body offsets
        let mut body_offsets = Vec::new();
        for body_ptr in body_ptrs {
            let body_offset = read_ptr(head, *body_ptr);
            body_offsets.push(body_offset - first_body_offset);
        }

        buf.skip(first_body_offset as usize);
        Some(SeveredBodies {
            buf,
            body_ptr_offsets: layout.body_ptrs.clone(),
            body_offsets,
        })
    } else {
        None
    };
    (head, body)
}

/// Reads a pointers from buf, at given offset.
/// Returns the pointers, *relative to the start of the buf*.
fn read_ptr(buf: &[u8], offset: u32) -> u32 {
    let mut r = crate::Reader::new(buf);
    r.skip(offset as usize);
    u32::from_le_bytes(r.read_const()) + offset
}

fn write_head(out: &mut Vec<u8>, data: Data, layout: &ir::TyLayout) -> Option<SeveredBodies> {
    let (head, mut body) = extract_head_and_body(&data, layout);

    // offset body pointers to the out buffer
    if let Some(body) = body.as_mut() {
        body.offset_head_buf(out.len() as u32);
    }
    // write head into out
    out.extend(head);

    body
}
