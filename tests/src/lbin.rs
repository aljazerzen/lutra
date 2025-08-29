mod printer;
mod writer;

use crate::lutra::bin as types;

use std::sync::OnceLock;

use insta::assert_snapshot;
use lutra_bin::{Decode, Encode, Value, ir};

#[track_caller]
fn _test_encode_decode<T: Encode + Decode + std::fmt::Debug>(value: Value, ty: &ir::Ty) -> String {
    crate::init_logger();

    let ty_defs = SCHEMA.get().unwrap();

    // Value::encode
    let buf = value.encode(ty, ty_defs).unwrap();

    tracing::debug!("Value::encode -> {buf:?}");

    // Value::decode
    let value_decoded = Value::decode(&buf, ty, ty_defs).unwrap();
    assert_eq!(value, value_decoded);

    // native decode
    let native = T::decode(&buf).unwrap();

    // native encode
    let buf2 = native.encode();
    assert_eq!(buf, buf2.to_vec(), "Value::encode == native::encode");

    pretty_hex::pretty_hex(&buf)
}

static SCHEMA: OnceLock<Vec<ir::TyDef>> = OnceLock::new();

#[track_caller]
fn _test_get_type(name: &'static str) -> &'static ir::Ty {
    let ty_defs = SCHEMA.get_or_init(|| {
        let source = include_str!("../lutra/bin.lt");
        let source = lutra_compiler::SourceTree::single("".into(), source.into());

        let project = lutra_compiler::check(source, lutra_compiler::CheckParams {})
            .unwrap_or_else(|e| panic!("{e}"));

        let module = lutra_compiler::lower_type_defs(&project);
        let module = lutra_compiler::layouter::on_root_module(module);
        module
            .iter_types_re()
            .map(|(name, ty)| ir::TyDef {
                name,
                ty: ty.clone(),
            })
            .collect()
    });

    let def = ty_defs.iter().find(|d| d.name.0 == [name]).unwrap();
    &def.ty
}

#[test]
fn test_x() {
    let ty = _test_get_type("x");

    let value = Value::Tuple(vec![
        Value::Prim64(42),
        Value::Text("Hello world!".to_string()),
        Value::Array(vec![Value::Prim8(1), Value::Prim8(0)]),
    ]);

    assert_snapshot!(_test_encode_decode::<types::x>(value, ty), @r#"
    Length: 38 (0x26) bytes
    0000:   2a 00 00 00  00 00 00 00  10 00 00 00  0c 00 00 00   *...............
    0010:   14 00 00 00  02 00 00 00  48 65 6c 6c  6f 20 77 6f   ........Hello wo
    0020:   72 6c 64 21  01 00                                   rld!..
    "#
    );
}

#[test]
fn test_y() {
    let ty = _test_get_type("y");
    let value = Value::Array(vec![Value::Prim64(12), Value::Prim64(55), Value::Prim64(2)]);
    assert_snapshot!(_test_encode_decode::<types::y>(value, ty), @r#"
    Length: 32 (0x20) bytes
    0000:   08 00 00 00  03 00 00 00  0c 00 00 00  00 00 00 00   ................
    0010:   37 00 00 00  00 00 00 00  02 00 00 00  00 00 00 00   7...............
    "#
    );
}

#[test]
fn test_z() {
    let ty = _test_get_type("z");
    let value = Value::Prim8(1);

    assert_snapshot!(_test_encode_decode::<types::z>(value, ty), @r#"
    Length: 1 (0x1) bytes
    0000:   01                                                   .
    "#
    );
}

#[test]
fn test_u_01() {
    let ty = _test_get_type("u");
    let value = Value::Enum(0, Box::new(Value::Prim8(1)));

    assert_snapshot!(_test_encode_decode::<types::u>(value, ty), @r#"
    Length: 6 (0x6) bytes
    0000:   00 04 00 00  00 01                                   ......
    "#
    );
}

#[test]
fn test_u_02() {
    let ty = _test_get_type("u");
    let value = Value::Enum(1, Box::new(Value::Tuple(vec![])));

    assert_snapshot!(_test_encode_decode::<types::u>(value, ty), @r#"
    Length: 5 (0x5) bytes
    0000:   01 00 00 00  00                                      .....
    "#
    );
}

#[test]
fn test_u_03() {
    let ty = _test_get_type("u");
    let value = Value::Enum(
        2,
        Box::new(Value::Tuple(vec![
            Value::Prim64(-12_i64 as u64),
            Value::Prim64(u64::from_ne_bytes((3.16_f64).to_ne_bytes())),
        ])),
    );

    assert_snapshot!(_test_encode_decode::<types::u>(value, ty), @r#"
    Length: 21 (0x15) bytes
    0000:   02 04 00 00  00 f4 ff ff  ff ff ff ff  ff 48 e1 7a   .............H.z
    0010:   14 ae 47 09  40                                      ..G.@
    "#
    );
}

#[test]
fn test_v() {
    let ty = _test_get_type("v");
    let value = Value::Enum(1, Box::new(Value::Tuple(vec![])));

    assert_snapshot!(_test_encode_decode::<types::v>(value, ty), @r#"
    Length: 1 (0x1) bytes
    0000:   01                                                   .
    "#
    );
}

#[test]
fn test_t_01() {
    let ty = _test_get_type("t");
    let value = Value::Enum(0, Box::new(Value::Prim8(2)));

    assert_snapshot!(_test_encode_decode::<types::t>(value, ty), @r#"
    Length: 3 (0x3) bytes
    0000:   00 02 00                                             ...
    "#
    );
}

#[test]
fn test_t_02() {
    let ty = _test_get_type("t");
    let value = Value::Enum(1, Box::new(Value::Prim16(2342)));

    assert_snapshot!(_test_encode_decode::<types::t>(value, ty), @r#"
    Length: 3 (0x3) bytes
    0000:   01 26 09                                             .&.
    "#
    );
}

#[test]
fn test_p() {
    let ty = _test_get_type("p");
    let value = Value::Tuple(vec![
        Value::Array(vec![Value::Prim64(2), Value::Prim64(4)]),
        Value::Array(vec![Value::Prim64(5), Value::Prim64(6), Value::Prim64(7)]),
    ]);

    assert_snapshot!(_test_encode_decode::<types::p>(value, ty), @r#"
    Length: 56 (0x38) bytes
    0000:   10 00 00 00  02 00 00 00  18 00 00 00  03 00 00 00   ................
    0010:   02 00 00 00  00 00 00 00  04 00 00 00  00 00 00 00   ................
    0020:   05 00 00 00  00 00 00 00  06 00 00 00  00 00 00 00   ................
    0030:   07 00 00 00  00 00 00 00                             ........
    "#
    );
}

#[test]
fn test_i() {
    let ty = _test_get_type("i");
    let value = Value::Tuple(vec![
        Value::Enum(1, Box::new(Value::Tuple(vec![]))),
        Value::Prim8(0),
    ]);

    assert_snapshot!(_test_encode_decode::<types::i>(value, ty), @r"
    Length: 2 (0x2) bytes
    0000:   01 00                                                ..
    "
    );
}

#[test]
fn test_tree() {
    let ty = _test_get_type("Tree");
    let value = Value::Enum(
        1, // Node
        Box::new(Value::Tuple(vec![
            Value::Enum(0, Box::new(Value::Prim8(4))), // Leaf
            Value::Enum(
                1, // Node
                Box::new(Value::Tuple(vec![
                    Value::Enum(0, Box::new(Value::Prim8(7))),  // Leaf
                    Value::Enum(0, Box::new(Value::Prim8(10))), // Leaf
                ])),
            ),
        ])),
    );
    assert_snapshot!(_test_encode_decode::<types::Tree>(value, ty), @r"
    Length: 28 (0x1c) bytes
    0000:   01 04 00 00  00 00 09 00  00 00 01 05  00 00 00 04   ................
    0010:   00 09 00 00  00 00 05 00  00 00 07 0a                ............
    "
    );
}

#[test]
fn test_opt_01() {
    let ty = _test_get_type("opt");
    let value = Value::Enum(
        0, // None
        Box::new(Value::Tuple(vec![])),
    );
    assert_snapshot!(_test_encode_decode::<types::opt>(value, ty), @r#"
    Length: 5 (0x5) bytes
    0000:   00 00 00 00  00                                      .....
    "#
    );
}

#[test]
fn test_opt_02() {
    let ty = _test_get_type("opt");
    let value = Value::Enum(
        1, // Some
        Box::new(Value::Text("text".into())),
    );
    assert_snapshot!(_test_encode_decode::<types::opt>(value, ty), @r#"
    Length: 17 (0x11) bytes
    0000:   01 04 00 00  00 08 00 00  00 04 00 00  00 74 65 78   .............tex
    0010:   74                                                   t
    "#
    );
}

#[test]
fn test_opt2_01() {
    let ty = _test_get_type("opt2");
    let value = Value::Enum(
        0, // None
        Box::new(Value::Tuple(vec![])),
    );
    assert_snapshot!(_test_encode_decode::<types::opt2>(value, ty), @r#"
    Length: 3 (0x3) bytes
    0000:   00 00 00                                             ...
    "#
    );
}

#[test]
fn test_opt2_02() {
    let ty = _test_get_type("opt2");
    let value = Value::Enum(
        1, // Some
        Box::new(Value::Prim16(65)),
    );
    assert_snapshot!(_test_encode_decode::<types::opt2>(value, ty), @r#"
    Length: 3 (0x3) bytes
    0000:   01 41 00                                             .A.
    "#
    );
}
