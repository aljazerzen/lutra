mod array;
mod test;
mod tuple;

pub use array::ArrayWriter;
pub use tuple::TupleWriter;

use lutra_frontend::pr;

fn extract_head_and_body<'b>(buf: &'b [u8], layout: &pr::TyLayout) -> (&'b [u8], Option<&'b [u8]>) {
    let head_bytes = layout.head_size / 8;

    let (head, remaining) = buf.split_at(head_bytes);

    let body = if let Some(body_ptr_offset) = layout.body_ptr_offset {
        let mut head = crate::Reader::new(head);
        head.skip(body_ptr_offset);
        let body_offset = u32::from_le_bytes(head.copy_const()) as usize - head_bytes;

        Some(&remaining[body_offset..])
    } else {
        None
    };
    (head, body)
}

fn write_head<'b>(
    out: &mut Vec<u8>,
    data: &'b [u8],
    layout: &pr::TyLayout,
) -> Option<(crate::ReversePointer, &'b [u8])> {
    let (head, body) = extract_head_and_body(data, layout);

    let res = if let Some(body) = body {
        let o = layout.body_ptr_offset.unwrap();
        let ptr_loc = crate::ReversePointer::new_at(out.len() + o);

        Some((ptr_loc, body))
    } else {
        None
    };

    out.extend(head);
    res
}
