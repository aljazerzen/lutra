mod array;
mod test;
mod tuple;

pub use array::ArrayWriter;
pub use tuple::TupleWriter;

use lutra_frontend::pr;

use crate::Data;

fn extract_head_and_body<'b>(buf: &'b Data, layout: &pr::TyLayout) -> (&'b [u8], Option<Data>) {
    let head_bytes = layout.head_size / 8;

    let head = buf.slice(head_bytes);

    let body = if let Some(body_ptr_offset) = layout.body_ptr_offset {
        let mut buf = buf.clone();

        let mut head = crate::Reader::new(buf.slice(body_ptr_offset + 4));
        head.skip(body_ptr_offset);
        let body_offset = u32::from_le_bytes(head.read_const()) as usize;

        buf.skip(body_ptr_offset + body_offset);
        Some(buf)
    } else {
        None
    };
    (head, body)
}

fn write_head(
    out: &mut Vec<u8>,
    data: Data,
    layout: &pr::TyLayout,
) -> Option<(crate::ReversePointer, Data)> {
    let (head, body) = extract_head_and_body(&data, layout);

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
