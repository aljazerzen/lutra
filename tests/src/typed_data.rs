use lutra_bin::Value;

#[track_caller]
fn _test_typed_data_roundtrip(value: Value, ty: &str) -> String {
    let ty = lutra_compiler::_test_compile_ty(ty);

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
            Value::Prim64(42),
            Value::Text("Hello world!".to_string()),
            Value::Array(vec![Value::Prim8(1), Value::Prim8(0)]),
        ]),
        "{int64, text, [bool]}",
    ), @r##"
    Length: 182 (0xb6) bytes
    0000:   01 11 00 00  00 01 7e 00  00 00 86 00  00 00 26 00   ......~.......&.
    0010:   00 00 08 00  00 00 03 00  00 00 00 00  00 00 00 00   ................
    0020:   27 00 00 00  01 23 00 00  00 00 00 00  00 00 00 25   '....#.........%
    0030:   00 00 00 01  21 00 00 00  00 00 00 00  00 02 23 00   ....!.........#.
    0040:   00 00 01 35  00 00 00 04  40 00 00 00  08 00 00 00   ...5....@.......
    0050:   00 00 00 00  0b 40 00 00  00 08 00 00  00 00 00 00   .....@..........
    0060:   00 00 09 00  00 00 01 05  00 00 00 00  08 00 00 00   ................
    0070:   08 00 00 00  00 00 00 00  40 00 00 00  08 00 00 00   ........@.......
    0080:   00 00 00 00  c0 00 00 00  08 00 00 00  00 00 00 00   ................
    0090:   2a 00 00 00  00 00 00 00  10 00 00 00  0c 00 00 00   *...............
    00a0:   14 00 00 00  02 00 00 00  48 65 6c 6c  6f 20 77 6f   ........Hello wo
    00b0:   72 6c 64 21  01 00                                   rld!..
    "##);
}

#[test]
fn test_typed_data_02() {
    insta::assert_snapshot!(_test_typed_data_roundtrip(Value::Prim8(1), "bool"), @r#"
    Length: 32 (0x20) bytes
    0000:   00 11 00 00  00 01 0d 00  00 00 15 00  00 00 01 00   ................
    0010:   00 00 00 08  00 00 00 08  00 00 00 00  00 00 00 01   ................
    "#);
}
