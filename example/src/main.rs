mod schema {
    include!(concat!(env!("OUT_DIR"), "/schema.rs"));
}

use std::collections::HashMap;

use lutra_bin::Value;
use lutra_bin::{Decode, Encode};
use lutra_parser::parser::pr;

fn main() {
    let schema_source = include_str!("schema.lt");
    let schema_types = parse_types(schema_source);

    {
        let x_ty = schema_types.get("x").unwrap();
        let x_value = Value::Tuple(vec![
            (None, Value::Integer(42)),
            (Some("a"), Value::String("Hello world!".to_string())),
            (
                Some("b"),
                Value::Array(vec![Value::Boolean(true), Value::Boolean(false)]),
            ),
        ]);

        let mut buf = Vec::new();
        x_value.encode(&mut buf, x_ty).unwrap();

        let x_value = Value::decode(&buf, &x_ty).unwrap();
        dbg!(x_value);

        let x = schema::x::decode_buffer(&buf).unwrap();
        dbg!(&x);

        let mut buf2 = Vec::new();
        x.encode(&mut buf2).unwrap();

        assert_eq!(buf, buf2);
    }

    {
        let y_ty = schema_types.get("y").unwrap();
        let y_value = Value::Array(vec![
            Value::Integer(12),
            Value::Integer(55),
            Value::Integer(2),
        ]);

        let mut buf = Vec::new();
        y_value.encode(&mut buf, y_ty).unwrap();

        let y_value = Value::decode(&buf, &y_ty).unwrap();
        dbg!(y_value);

        let y = schema::y::decode_buffer(&buf).unwrap();
        dbg!(&y);

        let mut buf2 = Vec::new();
        y.encode(&mut buf2).unwrap();

        assert_eq!(buf, buf2);
    }

    {
        let z_ty = schema_types.get("z").unwrap();
        let z_value = Value::Boolean(true);

        let mut buf = Vec::new();
        z_value.encode(&mut buf, z_ty).unwrap();

        let z_value = Value::decode(&buf, &z_ty).unwrap();
        dbg!(z_value);

        let z = schema::z::decode_buffer(&buf).unwrap();
        dbg!(&z);

        let mut buf2 = Vec::new();
        z.encode(&mut buf2).unwrap();

        assert_eq!(buf, buf2);
    }

    {
        let u_ty = schema_types.get("u").unwrap();
        let u_value = Value::Enum("f", Box::new(Value::Boolean(true)));

        let mut buf = Vec::new();
        u_value.encode(&mut buf, u_ty).unwrap();

        let u_value = Value::decode(&buf, &u_ty).unwrap();
        dbg!(u_value);

        let u = schema::u::decode_buffer(&buf).unwrap();
        dbg!(&u);

        let mut buf2 = Vec::new();
        u.encode(&mut buf2).unwrap();

        assert_eq!(buf, buf2);
    }

    {
        let u_ty = schema_types.get("u").unwrap();
        let u_value = Value::Enum("g", Box::new(Value::Tuple(vec![])));

        let mut buf = Vec::new();
        u_value.encode(&mut buf, u_ty).unwrap();

        let u_value = Value::decode(&buf, &u_ty).unwrap();
        dbg!(u_value);

        let u = schema::u::decode_buffer(&buf).unwrap();
        dbg!(&u);

        let mut buf2 = Vec::new();
        u.encode(&mut buf2).unwrap();

        assert_eq!(buf, buf2);
    }

    {
        let u_ty = schema_types.get("u").unwrap();
        let u_value = Value::Enum(
            "h",
            Box::new(Value::Tuple(vec![
                (Some("a"), Value::Integer(-12)),
                (Some("b"), Value::Float(3.14)),
            ])),
        );

        let mut buf = Vec::new();
        u_value.encode(&mut buf, u_ty).unwrap();

        let u_value = Value::decode(&buf, &u_ty).unwrap();
        dbg!(u_value);

        let u = schema::u::decode_buffer(&buf).unwrap();
        dbg!(&u);

        let mut buf2 = Vec::new();
        u.encode(&mut buf2).unwrap();

        assert_eq!(buf, buf2);
    }

    {
        let v_ty = schema_types.get("v").unwrap();
        let v_value = Value::Enum("No", Box::new(Value::Tuple(vec![])));

        let mut buf = Vec::new();
        v_value.encode(&mut buf, v_ty).unwrap();

        let v_value = Value::decode(&buf, &v_ty).unwrap();
        dbg!(v_value);

        let v = schema::v::decode_buffer(&buf).unwrap();
        dbg!(&v);

        let mut buf2 = Vec::new();
        v.encode(&mut buf2).unwrap();

        assert_eq!(buf, buf2);
    }
}

fn parse_types(source: &str) -> HashMap<String, pr::Ty> {
    let lr = lutra_parser::lexer::lex_source(source).unwrap();

    let (stmts, _errs) = lutra_parser::parser::parse_lr_to_pr(0, lr.0);

    let mut res = HashMap::new();
    for stmt in stmts.unwrap() {
        if let pr::StmtKind::TypeDef(ty_def) = stmt.kind {
            res.insert(ty_def.name, ty_def.value.unwrap());
        }
    }
    res
}
