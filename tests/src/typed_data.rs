use lutra_bin::Value;

#[track_caller]
fn _test_typed_data_roundtrip(value: Value, ty: &str) -> String {
    let ty = lutra_frontend::_test_compile_ty(ty);
    let ty = lutra_bin::ir::Ty::from(ty);

    let mut ltd_buf = Vec::new();
    lutra_typed_data::encode_typed_data(&mut ltd_buf, value.clone(), &ty)
        .unwrap()
        .unwrap();

    let (value_decoded, ty_decoded) = lutra_typed_data::decode_typed_data(&ltd_buf).unwrap();

    assert_eq!(value, value_decoded);
    assert_eq!(ty, ty_decoded);

    pretty_hex::pretty_hex(&ltd_buf)
}

#[test]
fn test_typed_data_01() {
    insta::assert_snapshot!(_test_typed_data_roundtrip(
        Value::Tuple(vec![
            Value::Int64(42),
            Value::Text("Hello world!".to_string()),
            Value::Array(vec![Value::Bool(true), Value::Bool(false)]),
        ]),
        "{int64, text, [bool]}",
    ), @r#"
    Length: 194 (0xc2) bytes
    0000:   01 11 00 00  00 01 8a 00  00 00 92 00  00 00 26 00   ..............&.
    0010:   00 00 08 00  00 00 03 00  00 00 00 00  00 00 00 00   ................
    0020:   27 00 00 00  01 27 00 00  00 00 00 00  00 00 00 29   '....'.........)
    0030:   00 00 00 01  29 00 00 00  00 00 00 00  00 02 2b 00   ....).........+.
    0040:   00 00 01 41  00 00 00 04  00 00 00 00  40 00 00 00   ...A........@...
    0050:   08 00 00 00  00 00 00 00  0b 00 00 00  00 40 00 00   .............@..
    0060:   00 08 00 00  00 00 00 00  00 00 09 00  00 00 01 09   ................
    0070:   00 00 00 00  00 00 00 00  08 00 00 00  08 00 00 00   ................
    0080:   00 00 00 00  40 00 00 00  08 00 00 00  00 00 00 00   ....@...........
    0090:   c0 00 00 00  08 00 00 00  00 00 00 00  2a 00 00 00   ............*...
    00a0:   00 00 00 00  10 00 00 00  0c 00 00 00  14 00 00 00   ................
    00b0:   02 00 00 00  48 65 6c 6c  6f 20 77 6f  72 6c 64 21   ....Hello world!
    00c0:   01 00                                                ..
    "#);
}

#[test]
fn test_typed_data_02() {
    insta::assert_snapshot!(_test_typed_data_roundtrip(Value::Bool(true), "bool"), @r#"
    Length: 36 (0x24) bytes
    0000:   00 11 00 00  00 01 11 00  00 00 19 00  00 00 01 00   ................
    0010:   00 00 00 00  00 00 00 08  00 00 00 08  00 00 00 00   ................
    0020:   00 00 00 01                                          ....
    "#);
}
