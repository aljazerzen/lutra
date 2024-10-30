mod schema {
    include!(concat!(env!("OUT_DIR"), "/schema.rs"));
}

use std::collections::HashMap;

use lutra_bin::{Decode, Encode, Value};
use lutra_parser::parser::pr;

fn main() {
    let schema_source = include_str!("schema.lt");
    let schema_types = parse_types(schema_source);

    {
        let x_ty = schema_types.get("x").unwrap();
        let x_value = Value::Tuple(vec![
            Value::Integer(42),
            Value::String("Hello world!".to_string()),
            Value::Array(vec![Value::Boolean(true), Value::Boolean(false)]),
        ]);

        let mut buf = Vec::new();
        x_value.encode(&mut buf, x_ty).unwrap();

        let x_value = Value::decode(&buf, x_ty).unwrap();
        dbg!(&x_value);

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

        let y_value = Value::decode(&buf, y_ty).unwrap();
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

        let z_value = Value::decode(&buf, z_ty).unwrap();
        dbg!(z_value);

        let z = schema::z::decode_buffer(&buf).unwrap();
        dbg!(&z);

        let mut buf2 = Vec::new();
        z.encode(&mut buf2).unwrap();

        assert_eq!(buf, buf2);
    }

    {
        let u_ty = schema_types.get("u").unwrap();
        let u_value = Value::Enum(0, Box::new(Value::Boolean(true)));

        let mut buf = Vec::new();
        u_value.encode(&mut buf, u_ty).unwrap();

        let u_value = Value::decode(&buf, u_ty).unwrap();
        dbg!(u_value);

        let u = schema::u::decode_buffer(&buf).unwrap();
        dbg!(&u);

        let mut buf2 = Vec::new();
        u.encode(&mut buf2).unwrap();

        assert_eq!(buf, buf2);
    }

    {
        let u_ty = schema_types.get("u").unwrap();
        let u_value = Value::Enum(1, Box::new(Value::Tuple(vec![])));

        let mut buf = Vec::new();
        u_value.encode(&mut buf, u_ty).unwrap();

        let u_value = Value::decode(&buf, u_ty).unwrap();
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
            1,
            Box::new(Value::Tuple(vec![Value::Integer(-12), Value::Float(3.16)])),
        );

        let mut buf = Vec::new();
        u_value.encode(&mut buf, u_ty).unwrap();

        let u_value = Value::decode(&buf, u_ty).unwrap();
        dbg!(u_value);

        let u = schema::u::decode_buffer(&buf).unwrap();
        dbg!(&u);

        let mut buf2 = Vec::new();
        u.encode(&mut buf2).unwrap();

        assert_eq!(buf, buf2);
    }

    {
        let v_ty = schema_types.get("v").unwrap();
        let v_value = Value::Enum(1, Box::new(Value::Tuple(vec![])));

        let mut buf = Vec::new();
        v_value.encode(&mut buf, v_ty).unwrap();

        let v_value = Value::decode(&buf, v_ty).unwrap();
        dbg!(v_value);

        let v = schema::v::decode_buffer(&buf).unwrap();
        dbg!(&v);

        let mut buf2 = Vec::new();
        v.encode(&mut buf2).unwrap();

        assert_eq!(buf, buf2);
    }

    {
        let tree_ty = schema_types.get("Tree").unwrap();
        let tree_value = Value::Enum(
            1, // Node
            Box::new(Value::Tuple(vec![
                Value::Enum(0, Box::new(Value::Integer(4))), // Leaf
                Value::Enum(
                    1, // Node
                    Box::new(Value::Tuple(vec![
                        Value::Enum(0, Box::new(Value::Integer(7))),  // Leaf
                        Value::Enum(0, Box::new(Value::Integer(10))), // Leaf
                    ])),
                ),
            ])),
        );

        let mut buf = Vec::new();
        tree_value.encode(&mut buf, tree_ty).unwrap();

        let tree_value = Value::decode(&buf, tree_ty).unwrap();
        dbg!(tree_value);

        let tree = schema::Tree::decode_buffer(&buf).unwrap();
        dbg!(&tree);

        let mut buf2 = Vec::new();
        tree.encode(&mut buf2).unwrap();

        assert_eq!(buf, buf2);
    }

    {
        let x_ty = schema_types.get("x").unwrap();
        let x_value = Value::Tuple(vec![
            Value::Integer(42),
            Value::String("Hello world!".to_string()),
            Value::Array(vec![Value::Boolean(true), Value::Boolean(false)]),
        ]);

        let mut ltd_buf = Vec::new();
        lutra_typed_data::encode_typed_data(&mut ltd_buf, x_value, x_ty).unwrap();

        let (x_value, _x_ty) = lutra_typed_data::decode_typed_data(&ltd_buf).unwrap();
        dbg!(x_value);
    }

    {
        let z_ty = schema_types.get("z").unwrap();
        let z_value = Value::Boolean(true);

        let mut ltd_buf = Vec::new();
        lutra_typed_data::encode_typed_data(&mut ltd_buf, z_value, z_ty).unwrap();

        let (z_value, _z_ty) = lutra_typed_data::decode_typed_data(&ltd_buf).unwrap();
        dbg!(z_value);
    }
}

fn parse_types(source: &str) -> HashMap<String, pr::Ty> {
    let lr = lutra_parser::lexer::lex_source(source).unwrap();

    let (stmts, _errs) = lutra_parser::parser::parse_lr_to_pr(0, lr.0);

    let mut res = HashMap::new();
    for stmt in stmts.unwrap() {
        if let pr::StmtKind::TypeDef(ty_def) = stmt.kind {
            let mut ty = ty_def.value.unwrap();
            ty.name = Some(ty_def.name.clone());
            res.insert(ty_def.name, ty);
        }
    }
    res
}
