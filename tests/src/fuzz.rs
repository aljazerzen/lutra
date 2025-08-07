use rand::{Rng, SeedableRng};

use crate::POSTGRES_URL;

#[allow(dead_code)]
struct Construct {
    name: &'static str,
    val_source: &'static str,
    val_builder: Box<dyn Fn(Vec<lutra_bin::Value>) -> lutra_bin::Value>,
    ty_source: &'static str,
    param_count: usize,
}

#[derive(Debug)]
struct Case {
    val: lutra_bin::Value,
    val_source: String,
    ty_source: String,
}

#[test]
#[ignore]
fn fuzz() {
    let constructs = [
        Construct {
            name: "int32",
            val_source: "5: int32",
            val_builder: Box::new(|_| lutra_bin::Value::Int32(5)),
            ty_source: "int32",
            param_count: 0,
        },
        Construct {
            name: "text",
            val_source: "\"hello\"",
            val_builder: Box::new(|_| lutra_bin::Value::Text("hello".into())),
            ty_source: "text",
            param_count: 0,
        },
        Construct {
            name: "bool",
            val_source: "true",
            val_builder: Box::new(|_| lutra_bin::Value::Bool(true)),
            ty_source: "bool",
            param_count: 0,
        },
        Construct {
            name: "tuple_1",
            val_source: "{1: int32, $0, 2: int16}",
            val_builder: Box::new(|args| {
                lutra_bin::Value::Tuple(vec![
                    lutra_bin::Value::Int32(1),
                    args.into_iter().next().unwrap(),
                    lutra_bin::Value::Int16(2),
                ])
            }),
            ty_source: "{int32, $0, int16}",
            param_count: 1,
        },
        Construct {
            name: "array_1",
            val_source: "[$0]",
            val_builder: Box::new(|args| {
                lutra_bin::Value::Array(vec![args.into_iter().next().unwrap()])
            }),
            ty_source: "[$0]",
            param_count: 1,
        },
        Construct {
            name: "tuple_2",
            val_source: "{1: int32, $0, 2: int16, $1, 3: int16}",
            val_builder: Box::new(|args| {
                let mut args = args.into_iter();
                lutra_bin::Value::Tuple(vec![
                    lutra_bin::Value::Int32(1),
                    args.next().unwrap(),
                    lutra_bin::Value::Int16(2),
                    args.next().unwrap(),
                    lutra_bin::Value::Int16(3),
                ])
            }),
            ty_source: "{int32, $0, int16, $1, int16}",
            param_count: 2,
        },
        Construct {
            name: "tuple_lookup",
            val_source: "{\"hello\", $0}.1",
            val_builder: Box::new(|args| args.into_iter().next().unwrap()),
            ty_source: "$0",
            param_count: 1,
        },
        Construct {
            name: "array_lookup",
            val_source: "std::index([$0], 0)",
            val_builder: Box::new(|args| args.into_iter().next().unwrap()),
            ty_source: "$0",
            param_count: 1,
        },
    ];

    let source = lutra_compiler::SourceTree::empty();
    let project = lutra_compiler::check(source, Default::default()).unwrap();

    let mut client = postgres::Client::connect(POSTGRES_URL, postgres::NoTls).unwrap();

    for i in tqdm::tqdm(0..10000) {
        generate_and_test(&constructs, i, &project, &mut client);
    }
}

fn generate_and_test(
    constructs: &[Construct],
    seed: u64,
    project: &lutra_compiler::Project,
    client: &mut postgres::Client,
) {
    let mut rng = rand::rngs::SmallRng::seed_from_u64(seed);
    let case = generate(constructs, &mut rng, 0);

    // dbg!(&case);

    // fetch the value from source
    let (program, ty) = lutra_compiler::compile(
        project,
        &case.val_source,
        None,
        lutra_compiler::ProgramFormat::SqlPg,
    )
    .unwrap();
    let program = program.into_sql_pg().unwrap();
    let result = lutra_runner_postgres::execute(client, &program, &[]).unwrap();
    let case_val = case.val.encode(&ty.output, &[]).unwrap();
    assert_eq!(result, case_val);

    // roundtrip
    let (program, _ty) = lutra_compiler::compile(
        project,
        &format!("func (x: {}) -> x", case.ty_source),
        None,
        lutra_compiler::ProgramFormat::SqlPg,
    )
    .unwrap();
    let program = program.into_sql_pg().unwrap();
    let result = lutra_runner_postgres::execute(client, &program, &case_val).unwrap();
    assert_eq!(result, case_val);
}

fn generate(constructs: &[Construct], rng: &mut impl Rng, depth: usize) -> Case {
    if depth > 7 {
        return Case {
            val_source: "false".into(),
            ty_source: "bool".into(),
            val: lutra_bin::Value::Bool(false),
        };
    }

    let construct = &constructs[rng.random_range(0..constructs.len())];

    let mut val_source = construct.val_source.to_string();
    let mut ty_source = construct.ty_source.to_string();
    let mut args = Vec::with_capacity(construct.param_count);
    for i in 0..construct.param_count {
        let sub = generate(constructs, rng, depth + 1);

        val_source = val_source.replace(&format!("${i}"), &sub.val_source);
        ty_source = ty_source.replace(&format!("${i}"), &sub.ty_source);
        args.push(sub.val);
    }
    Case {
        val: (*construct.val_builder)(args),
        val_source,
        ty_source,
    }
}
