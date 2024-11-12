use crate::lutra::bin as types;

use std::sync::OnceLock;

use insta::assert_snapshot;
use lutra_bin::{Decode, Encode, Value};
use lutra_frontend::{pr, Project};

#[track_caller]
fn _test_encode_decode<T: Encode + Decode>(value: Value, ty: &pr::Ty) -> String {
    // Value::encode
    let mut buf = Vec::new();
    value.encode(&mut buf, ty).unwrap();

    // Value::decode
    let value_decoded = Value::decode(&buf, ty).unwrap();
    assert_eq!(value, value_decoded);

    // native decode
    let x = T::decode_buffer(&buf).unwrap();

    // native encode
    let mut buf2 = Vec::new();
    x.encode(&mut buf2).unwrap();
    assert_eq!(buf, buf2);

    pretty_hex::pretty_hex(&buf)
}

static SCHEMA: OnceLock<Project> = OnceLock::new();

#[track_caller]
fn _test_get_type(name: &'static str) -> &'static pr::Ty {
    let project = SCHEMA.get_or_init(|| {
        let source = include_str!("../lutra/bin.lt");
        let source = lutra_frontend::SourceTree::single("".into(), source.into());

        lutra_frontend::compile(source, lutra_frontend::CompileParams {}).unwrap()
    });

    let name = pr::Path::from_name(name);
    let decl = project.root_module.module.get(&name).unwrap();
    decl.kind.as_ty().unwrap()
}

#[test]
fn test_x() {
    let ty = _test_get_type("x");

    let value = Value::Tuple(vec![
        Value::Int(42),
        Value::Text("Hello world!".to_string()),
        Value::Array(vec![Value::Bool(true), Value::Bool(false)]),
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
    let value = Value::Array(vec![Value::Int(12), Value::Int(55), Value::Int(2)]);
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
    let value = Value::Bool(true);

    assert_snapshot!(_test_encode_decode::<types::z>(value, ty), @r#"
    Length: 1 (0x1) bytes
    0000:   01                                                   .
    "#
    );
}

#[test]
fn test_u_01() {
    let ty = _test_get_type("u");
    let value = Value::Enum(0, Box::new(Value::Bool(true)));

    assert_snapshot!(_test_encode_decode::<types::u>(value, ty), @r#"
    Length: 5 (0x5) bytes
    0000:   00 01 00 00  00                                      .....
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
        Box::new(Value::Tuple(vec![Value::Int(-12), Value::Float(3.16)])),
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
    Length: 5 (0x5) bytes
    0000:   01 00 00 00  00                                      .....
    "#
    );
}

#[test]
fn test_tree() {
    let ty = _test_get_type("Tree");
    let value = Value::Enum(
        1, // Node
        Box::new(Value::Tuple(vec![
            Value::Enum(0, Box::new(Value::Int(4))), // Leaf
            Value::Enum(
                1, // Node
                Box::new(Value::Tuple(vec![
                    Value::Enum(0, Box::new(Value::Int(7))),  // Leaf
                    Value::Enum(0, Box::new(Value::Int(10))), // Leaf
                ])),
            ),
        ])),
    );
    assert_snapshot!(_test_encode_decode::<types::Tree>(value, ty), @r#"
    Length: 49 (0x31) bytes
    0000:   01 04 00 00  00 00 09 00  00 00 01 0c  00 00 00 04   ................
    0010:   00 00 00 00  00 00 00 00  09 00 00 00  00 0c 00 00   ................
    0020:   00 07 00 00  00 00 00 00  00 0a 00 00  00 00 00 00   ................
    0030:   00                                                   .
    "#
    );
}
