use lutra_bin::ir;
use lutra_interpreter::{ArrayWriter, Data, EnumWriter, TupleWriter};

#[track_caller]
pub(crate) fn _test_array_writer(items: Vec<Data>, output_ty: &ir::Ty) -> String {
    let mut output = ArrayWriter::new_for_ty(output_ty);
    for item in items {
        output.write_item(item);
    }

    let output_buf = output.finish().flatten();

    String::new()
        + &lutra_bin::print_source(&output_buf, output_ty, &[]).unwrap()
        + "\n"
        + &pretty_hex::pretty_hex(&output_buf)
}

#[track_caller]
pub(crate) fn _test_tuple_writer(fields: Vec<Data>, output_ty: &ir::Ty) -> String {
    let mut output = TupleWriter::new_for_ty(output_ty);
    for field in fields {
        output.write_field(field);
    }

    let output_buf = output.finish().flatten();

    String::new()
        + &lutra_bin::print_source(&output_buf, output_ty, &[]).unwrap()
        + "\n"
        + &pretty_hex::pretty_hex(&output_buf)
}

#[track_caller]
pub(crate) fn _test_enum_writer(tag: u64, inner: Data, output_ty: &ir::Ty) -> String {
    let writer = EnumWriter::new_for_ty(output_ty);
    let output = writer.write(tag, inner);

    let output_buf = output.flatten();

    String::new()
        + &lutra_bin::print_source(&output_buf, output_ty, &[]).unwrap()
        + "\n"
        + &pretty_hex::pretty_hex(&output_buf)
}

#[test]
fn array_01() {
    let items = vec![
        Data::new(vec![0, 0, 0, 0, 0, 0, 0, 0]),
        Data::new(vec![1, 0, 0, 0, 0, 0, 0, 0]),
        Data::new(vec![2, 0, 0, 0, 0, 0, 0, 0]),
        Data::new(vec![3, 0, 0, 0, 0, 0, 0, 0]),
        Data::new(vec![4, 0, 0, 0, 0, 0, 0, 0]),
    ];

    let output_ty = lutra_compiler::_test_compile_ty("[int64]");

    insta::assert_snapshot!(_test_array_writer(items, &output_ty), @r#"
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
fn array_02() {
    let data1 = Data::new(vec![8, 0, 0, 0, 1, 0, 0, 0, 0x30]);
    let data2 = Data::new(vec![8, 0, 0, 0, 1, 0, 0, 0, 0x31]);

    let data3 = Data::new(vec![
        0x10, 0, 0, 0, 1, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0, 0, 0x37, 0x38, 0x39,
    ]);

    let mut data4 = data3.clone();
    data4.advance(8);

    let items = vec![data1, data2, data3, data4];

    let output_ty = lutra_compiler::_test_compile_ty("[text]");

    insta::assert_snapshot!(_test_array_writer(items, &output_ty), @r#"
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
fn tuple_01() {
    let fields = vec![
        Data::new(vec![42, 0, 0, 0, 0, 0, 0, 0]), // int
        Data::new(vec![
            0x10, 0, 0, 0, 4, 0, 0, 0, 42, 0, 0, 0, 0, 0, 0, 0, 65, 66, 65, 66,
        ]), // text (contains int between its head and body)
        Data::new(vec![1, 1, 0, 0, 0, 0, 0, 0, 65, 66, 65, 66]), // int (followed by the body of text)
    ];

    let output_ty = lutra_compiler::_test_compile_ty("{int64, text, int64}");

    insta::assert_snapshot!(_test_tuple_writer(fields, &output_ty), @r#"
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

#[test]
fn enum_01() {
    let output_ty =
        lutra_compiler::_test_compile_ty("enum {Done, Pending: int16, Cancelled: text}");

    insta::assert_snapshot!(_test_enum_writer(0, Data::new(vec![]), &output_ty), @r#"
    Done
    Length: 5 (0x5) bytes
    0000:   00 04 00 00  00                                      .....
    "#);

    // int
    let inner = Data::new(vec![42, 0]);
    insta::assert_snapshot!(_test_enum_writer(1, inner, &output_ty), @r"
    Pending(42)
    Length: 7 (0x7) bytes
    0000:   01 04 00 00  00 2a 00                                .....*.
    ");

    // int (followed by the body of text)
    let inner = Data::new(vec![1, 1, 65, 66, 65, 66]);
    insta::assert_snapshot!(_test_enum_writer(1, inner, &output_ty), @r"
    Pending(257)
    Length: 11 (0xb) bytes
    0000:   01 04 00 00  00 01 01 41  42 41 42                   .......ABAB
    ");

    // text
    let inner = Data::new(vec![8, 0, 0, 0, 4, 0, 0, 0, 65, 66, 65, 66]);
    insta::assert_snapshot!(_test_enum_writer(2, inner, &output_ty), @r#"
    Cancelled("ABAB")
    Length: 17 (0x11) bytes
    0000:   02 04 00 00  00 08 00 00  00 04 00 00  00 41 42 41   .............ABA
    0010:   42                                                   B
    "#);

    // text (contains int between its head and body)
    let inner = Data::new(vec![
        0x10, 0, 0, 0, 4, 0, 0, 0, 42, 0, 0, 0, 0, 0, 0, 0, 65, 66, 65, 66,
    ]);
    insta::assert_snapshot!(_test_enum_writer(2, inner, &output_ty), @r#"
    Cancelled("ABAB")
    Length: 25 (0x19) bytes
    0000:   02 04 00 00  00 10 00 00  00 04 00 00  00 2a 00 00   .............*..
    0010:   00 00 00 00  00 41 42 41  42                         .....ABAB
    "#);
}

#[test]
fn enum_02() {
    // put an enum into an array, to test that body_ptrs are correct

    let output_ty =
        lutra_compiler::_test_compile_ty("[enum {Done, Pending: int16, Cancelled: text}]");

    // done
    let item_0 = Data::new(vec![0, 0, 0, 0, 0]);

    // int16
    let item_1 = Data::new(vec![1, 4, 0, 0, 0, 42, 0, 0, 0]);

    // int16 (followed by the body of text)
    let item_2 = Data::new(vec![1, 4, 0, 0, 0, 1, 1, 0, 0, 65, 66, 65, 66]);

    // text
    let item_3 = Data::new(vec![2, 4, 0, 0, 0, 8, 0, 0, 0, 4, 0, 0, 0, 65, 66, 65, 66]);

    // text (contains int between its head and body)
    let item_4 = Data::new(vec![
        2, 4, 0, 0, 0, 0x10, 0, 0, 0, 3, 0, 0, 0, 42, 0, 0, 0, 0, 0, 0, 0, 65, 66, 65,
    ]);

    let items = vec![item_0, item_1, item_2, item_3, item_4];

    insta::assert_snapshot!(_test_array_writer(items, &output_ty), @r#"
    [
      Done,
      Pending(42),
      Pending(257),
      Cancelled("ABAB"),
      Cancelled("ABA"),
    ]
    Length: 80 (0x50) bytes
    0000:   08 00 00 00  05 00 00 00  00 18 00 00  00 01 17 00   ................
    0010:   00 00 01 16  00 00 00 02  19 00 00 00  02 20 00 00   ............. ..
    0020:   00 00 00 00  00 2a 00 00  00 01 01 00  00 41 42 41   .....*.......ABA
    0030:   42 08 00 00  00 04 00 00  00 41 42 41  42 10 00 00   B........ABAB...
    0040:   00 03 00 00  00 2a 00 00  00 00 00 00  00 41 42 41   .....*.......ABA
    "#);
}

#[test]
fn enum_03() {
    // put an enum into an array, to test that body_ptrs are correct

    let output_ty =
        lutra_compiler::_test_compile_ty("enum {Done, Pending: int16, Cancelled: bool}");

    // done
    let inner = Data::new(vec![]);
    insta::assert_snapshot!(_test_enum_writer(0, inner, &output_ty), @r#"
    Done
    Length: 3 (0x3) bytes
    0000:   00 00 00                                             ...
    "#);

    let inner = Data::new(vec![1, 2]);
    insta::assert_snapshot!(_test_enum_writer(1, inner, &output_ty), @r"
    Pending(513)
    Length: 3 (0x3) bytes
    0000:   01 01 02                                             ...
    ");

    let inner = Data::new(vec![1, 2, 3, 4, 5, 6, 7, 8, 9]);
    insta::assert_snapshot!(_test_enum_writer(2, inner, &output_ty), @r"
    Cancelled(true)
    Length: 3 (0x3) bytes
    0000:   02 01 02                                             ...
    ");
}
