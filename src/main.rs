mod hello {
    include!(concat!(env!("OUT_DIR"), "/hello.rs"));
}

use lutra_bin::Value;
use lutra_bin::{Decode, Encode};

fn main() {
    let source = r#"
        type x = {int, a = text, b = [bool]}
    "#;

    let lr = lutra_parser::lexer::lex_source(source).unwrap();
    let (stmts, _errs) = lutra_parser::parser::parse_lr_to_pr(0, lr.0);
    let mut stmts = stmts.unwrap().into_iter();

    let ty_stmt = stmts.next().unwrap();
    let ty = ty_stmt.kind.into_type_def().unwrap().value.unwrap();

    let value = Value::Tuple(vec![
        (None, Value::Integer(42)),
        (Some("a"), Value::String("Hello world!".to_string())),
        (
            Some("b"),
            Value::Array(vec![Value::Boolean(true), Value::Boolean(false)]),
        ),
    ]);

    let mut buf = Vec::new();
    value.encode(&mut buf).unwrap();
    dbg!(&buf);

    let value = Value::decode(&buf, &ty).unwrap();
    dbg!(value);

    let x = hello::x::decode_bytes(&buf).unwrap();
    dbg!(&x);

    let mut buf2 = Vec::new();
    x.encode(&mut buf2).unwrap();

    assert_eq!(buf, buf2);
}
