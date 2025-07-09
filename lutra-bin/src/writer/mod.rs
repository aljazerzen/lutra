//! Writers of the binary format for generic container types (array, tuple, enum).
//!
//! These functions operate on [Data] and are able to "patch" together multiple encoded
//! data structure into one container type. For example, two buffers of `[int64]` can be
//! patched into one buffer of `{[int64], [int64]}`.
//!
//! This is complicated because the relative pointers of the arrays need to be rewritten
//! to point to new locations of the array contents.
//!
//! If you can looking for encoding of full data structure at once, use [crate::encode].
//!
//! TODO: term "writer" is not conveying that this is an "advanced" object for patching
//! values together. Also, usage for this is quite limited and mostly aimed at the
//! interpreter. Maybe these should be moved there?

mod array;
mod enum_;
mod tuple;

pub use array::ArrayWriter;
pub use enum_::EnumWriter;
pub use tuple::TupleWriter;

use crate::vec;
use core::iter::zip;

use crate::Data;
use crate::ReaderExt;
use crate::ReversePointer;

#[derive(Debug)]
struct SeveredBodies {
    // Buffer that contains the bodies of the type
    buf: Data,

    // Locations of pointers to bodies within the buffer of the head, in bytes.
    body_ptr_offsets: vec::Vec<u32>,

    // Locations of starts of bodies (except the first one) within the buf, in bytes.
    // The first body is skipped, because it is guaranteed to be 0.
    // This is because we skip the buf so the first body is always at the beginning of the buf.
    body_offsets: vec::Vec<u32>,
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
    head_bytes: u32,
    body_ptrs: &[u32],
) -> (&'b [u8], Option<SeveredBodies>) {
    let head = buf.slice(head_bytes as usize);

    let body = if !body_ptrs.is_empty() {
        let mut ptrs = body_ptrs.iter();

        let first_body_ptr = ptrs.next().unwrap();
        let mut buf = buf.clone();

        let first_body_offset = read_ptr(head, *first_body_ptr);

        // read body offsets
        let mut body_offsets = vec::Vec::new();
        for ptr in ptrs {
            let body_offset = read_ptr(head, *ptr);
            body_offsets.push(body_offset - first_body_offset);
        }

        buf.skip(first_body_offset as usize);
        Some(SeveredBodies {
            buf,
            body_ptr_offsets: body_ptrs.to_vec(),
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
    let buf = buf.skip(offset as usize);
    u32::from_le_bytes(buf.read_const()) + offset
}

fn write_head(
    out: &mut vec::Vec<u8>,
    data: Data,
    head_bytes: u32,
    body_ptrs: &[u32],
) -> Option<SeveredBodies> {
    if head_bytes == 0 {
        return None;
    }

    let (head, mut body) = extract_head_and_body(&data, head_bytes, body_ptrs);

    // offset body pointers to the out buffer
    if let Some(body) = body.as_mut() {
        body.offset_head_buf(out.len() as u32);
    }
    // write head into out
    out.extend(head);

    body
}
