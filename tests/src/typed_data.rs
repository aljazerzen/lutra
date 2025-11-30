use lutra_bin::{Value, ir, typed_data};

#[track_caller]
fn _test_typed_data_roundtrip(value: Value, ty: &str) -> String {
    let ty = lutra_compiler::_test_compile_ty(ty);
    let ty_defs: Vec<ir::TyDef> = vec![];
    let data = value.encode(&ty, &[]).unwrap();

    let mut ltd_buf = lutra_bin::bytes::BytesMut::new();
    typed_data::encode(&mut ltd_buf, &data, &ty, &ty_defs).unwrap();

    let (data_decoded, ty_decoded, ty_defs) = typed_data::decode(&ltd_buf).unwrap();

    let value_decoded = Value::decode(data_decoded, &ty, &ty_defs).unwrap();
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
    ), @r"
    Length: 271 (0x10f) bytes
    0000:   01 26 00 00  00 01 cf 00  00 00 00 00  00 00 00 da   .&..............
    0010:   00 00 00 00  00 00 00 d2  00 00 00 00  00 00 00 ca   ................
    0020:   00 00 00 26  00 00 00 08  00 00 00 03  00 00 00 00   ...&............
    0030:   00 00 00 00  00 4e 00 00  00 01 4a 00  00 00 00 00   .....N....J.....
    0040:   00 00 00 4d  00 00 00 00  00 00 00 00  00 00 00 00   ...M............
    0050:   00 3f 00 00  00 01 3b 00  00 00 00 00  00 00 00 42   .?....;........B
    0060:   00 00 00 00  00 00 00 00  00 00 00 00  02 34 00 00   .............4..
    0070:   00 01 53 00  00 00 00 00  00 00 00 5a  00 00 00 00   ..S........Z....
    0080:   00 00 00 04  40 00 00 00  08 00 00 00  00 00 00 00   ....@...........
    0090:   0b 40 00 00  00 08 00 00  00 01 00 00  00 00 00 00   .@..............
    00a0:   00 00 16 00  00 00 01 12  00 00 00 00  00 00 00 00   ................
    00b0:   15 00 00 00  00 00 00 00  00 08 00 00  00 08 00 00   ................
    00c0:   00 00 00 00  00 40 00 00  00 08 00 00  00 01 00 00   .....@..........
    00d0:   00 00 00 00  00 c0 00 00  00 08 00 00  00 02 00 00   ................
    00e0:   00 08 00 00  00 10 00 00  00 2a 00 00  00 00 00 00   .........*......
    00f0:   00 10 00 00  00 0c 00 00  00 14 00 00  00 02 00 00   ................
    0100:   00 48 65 6c  6c 6f 20 77  6f 72 6c 64  21 01 00      .Hello world!..
    ");
}

#[test]
fn test_typed_data_02() {
    insta::assert_snapshot!(_test_typed_data_roundtrip(Value::Prim8(1), "bool"), @r#"
    Length: 53 (0x35) bytes
    0000:   00 26 00 00  00 01 22 00  00 00 00 00  00 00 00 25   .&...."........%
    0010:   00 00 00 00  00 00 00 1d  00 00 00 00  00 00 00 15   ................
    0020:   00 00 00 01  00 00 00 00  08 00 00 00  08 00 00 00   ................
    0030:   00 00 00 00  01                                      .....
    "#);
}
