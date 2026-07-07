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

use core::iter::zip;
use std::vec;

use crate::Data;
use lutra_bin::ReaderExt;
use lutra_bin::ReversePointer;

#[derive(Debug)]
struct SeveredBodies {
    // Buffer that contains the bodies of the type
    buf: Data,

    // Locations of pointers to *live* bodies within the buffer of the head, in
    // bytes. Slots for unit enum variants (whose raw pointer value is zero and
    // which carry no body) are filtered out during extraction.
    body_ptr_offsets: Vec<u32>,

    // Locations of starts of subsequent live bodies within `buf`, in bytes,
    // relative to the first live body. The first live body's offset is
    // implicit (0) because `buf` is advanced to point at it during extraction.
    // Invariant: `body_ptr_offsets.len() == body_offsets.len() + 1`, and both
    // index only live pointer slots.
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

fn extract_head_and_body(
    buf: Data,
    head_bytes: u32,
    body_ptrs: &[u32],
) -> (Vec<u8>, Option<SeveredBodies>) {
    let head = buf.flatten_n(head_bytes as usize);

    // Collect only the *live* pointer slots — those whose raw stored offset is
    // non-zero. Enum-typed fields whose layout says `has_ptr` still contribute a
    // `body_ptrs` entry at compile time, but at runtime a unit variant leaves
    // that slot zeroed and has no body. Treating such slots as real body pointers
    // would misread padding as a body offset and underflow the subtraction below.
    let mut live: vec::Vec<(u32, u32)> = vec::Vec::with_capacity(body_ptrs.len());
    for ptr in body_ptrs {
        let raw = read_raw_ptr(&head, *ptr);
        if raw == 0 {
            // Unit-variant / no-body slot. Skip; leave the zero in place so
            // downstream pointer rewriting does not clobber it.
            continue;
        }
        // Absolute offset of this body from the start of `buf`.
        live.push((*ptr, raw + *ptr));
    }

    let body = if let Some((_, first_body_offset)) = live.first().copied() {
        let mut buf = buf.clone();

        // Offsets of subsequent bodies, relative to the first body's start.
        let mut body_offsets = vec::Vec::with_capacity(live.len() - 1);
        for &(_, body_offset) in &live[1..] {
            body_offsets.push(body_offset - first_body_offset);
        }

        let body_ptr_offsets: vec::Vec<u32> = live.iter().map(|(p, _)| *p).collect();

        buf.advance(first_body_offset as usize);
        Some(SeveredBodies {
            buf,
            body_ptr_offsets,
            body_offsets,
        })
    } else {
        None
    };
    (head, body)
}

/// Reads the raw 4-byte pointer value stored at `offset` in `buf`, without
/// adding `offset`.
fn read_raw_ptr(buf: &[u8], offset: u32) -> u32 {
    let buf = buf.skip(offset as usize);
    u32::from_le_bytes(buf.read_const())
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

    let (head, mut body) = extract_head_and_body(data, head_bytes, body_ptrs);

    // offset body pointers to the out buffer
    if let Some(body) = body.as_mut() {
        body.offset_head_buf(out.len() as u32);
    }
    // write head into out
    out.extend(head);

    body
}
