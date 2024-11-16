#![cfg(test)]

use super::{ArrayWriter, TupleWriter};

use lutra_frontend::pr;

#[track_caller]
pub(crate) fn _test_array_writer(items: Vec<Vec<u8>>, output_ty: pr::Ty) -> String {
    let mut output = ArrayWriter::new(&output_ty);
    for item in items {
        output.write_item(item);
    }

    let output_buf = output.finish();
    let output_val = crate::Value::decode(&output_buf, &output_ty).unwrap();

    String::new()
        + &output_val.print_source(&output_ty).unwrap()
        + "\n"
        + &pretty_hex::pretty_hex(&output_buf)
}

#[track_caller]
pub(crate) fn _test_tuple_writer(fields: Vec<Vec<u8>>, output_ty: pr::Ty) -> String {
    let mut output = TupleWriter::new(&output_ty);
    for field in fields {
        output.write_field(field);
    }

    let output_buf = output.finish();
    let output_val = crate::Value::decode(&output_buf, &output_ty).unwrap();

    String::new()
        + &output_val.print_source(&output_ty).unwrap()
        + "\n"
        + &pretty_hex::pretty_hex(&output_buf)
}

#[test]
pub(crate) fn test_01() {
    let items = vec![
        vec![0, 0, 0, 0, 0, 0, 0, 0],
        vec![1, 0, 0, 0, 0, 0, 0, 0],
        vec![2, 0, 0, 0, 0, 0, 0, 0],
        vec![3, 0, 0, 0, 0, 0, 0, 0],
        vec![4, 0, 0, 0, 0, 0, 0, 0],
    ];

    let output_ty = lutra_frontend::_test_compile_ty("[int]");

    insta::assert_snapshot!(_test_array_writer(items, output_ty), @r#"
    [
      0,
      1,
      2,
      3,
      4,
    ]
    Length: 48 (0x30) bytes
    0000:   08 00 00 00  05 00 00 00  00 00 00 00  00 00 00 00   ................
    0010:   01 00 00 00  00 00 00 00  02 00 00 00  00 00 00 00   ................
    0020:   03 00 00 00  00 00 00 00  04 00 00 00  00 00 00 00   ................
    "#);
}

#[test]
pub(crate) fn test_02() {
    let items = vec![
        vec![8, 0, 0, 0, 1, 0, 0, 0, 0x30],
        vec![8, 0, 0, 0, 1, 0, 0, 0, 0x31],
        vec![
            0x10, 0, 0, 0, 1, 0, 0, 0, 8, 0, 0, 0, 1, 0, 0, 0, 0x37, 0x38, 0x39,
        ],
        vec![9, 0, 0, 0, 1, 0, 0, 0, 0x37, 0x38, 0x39],
    ];

    let output_ty = lutra_frontend::_test_compile_ty("[text]");

    insta::assert_snapshot!(_test_array_writer(items, output_ty), @r#"
    [
      "0",
      "1",
      "7",
      "8",
    ]
    Length: 47 (0x2f) bytes
    0000:   08 00 00 00  04 00 00 00  20 00 00 00  01 00 00 00   ........ .......
    0010:   19 00 00 00  01 00 00 00  12 00 00 00  01 00 00 00   ................
    0020:   0d 00 00 00  01 00 00 00  30 31 37 38  39 38 39      ........0178989
    "#);
}

#[test]
pub(crate) fn test_04() {
    let fields = vec![
        vec![42, 0, 0, 0, 0, 0, 0, 0], // int
        vec![
            0x10, 0, 0, 0, 4, 0, 0, 0, 42, 0, 0, 0, 0, 0, 0, 0, 65, 66, 65, 66,
        ], // text (contains int between its head and body)
        vec![1, 1, 0, 0, 0, 0, 0, 0, 65, 66, 65, 66], // int (followed by the body of text)
    ];

    let output_ty = lutra_frontend::_test_compile_ty("{int, text, int}");

    insta::assert_snapshot!(_test_tuple_writer(fields, output_ty), @r#"
    {
      42,
      "ABAB",
      257,
    }
    Length: 28 (0x1c) bytes
    0000:   2a 00 00 00  00 00 00 00  10 00 00 00  04 00 00 00   *...............
    0010:   01 01 00 00  00 00 00 00  41 42 41 42                ........ABAB
    "#);
}
