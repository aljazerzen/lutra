use rand::{Rng, SeedableRng};

use crate::POSTGRES_URL;

#[allow(dead_code)]
struct Construct {
    name: String,
    text: String,
    sub_places: usize,
}

#[test]
#[ignore]
fn fuzz() {
    let constructs = [
        Construct {
            name: "int32".into(),
            text: "5: int32".into(),
            sub_places: 0,
        },
        Construct {
            name: "text".into(),
            text: "\"hello\"".into(),
            sub_places: 0,
        },
        Construct {
            name: "bool".into(),
            text: "true".into(),
            sub_places: 0,
        },
        Construct {
            name: "tuple_1".into(),
            text: "{1: int32, $0, 2: int16}".into(),
            sub_places: 1,
        },
        Construct {
            name: "array_1".into(),
            text: "[$0]".into(),
            sub_places: 1,
        },
        Construct {
            name: "tuple_2".into(),
            text: "{1: int32, $0, 2: int16, $1, 3: int16}".into(),
            sub_places: 2,
        },
        Construct {
            name: "tuple_lookup".into(),
            text: "{\"hello\", $0}.1".into(),
            sub_places: 1,
        },
        Construct {
            name: "array_lookup".into(),
            text: "std::index([$0], 0)".into(),
            sub_places: 1,
        },
    ];

    let source = lutra_compiler::SourceTree::empty();
    let project = lutra_compiler::check(source, Default::default()).unwrap();

    let mut client = postgres::Client::connect(POSTGRES_URL, postgres::NoTls).unwrap();

    for i in tqdm::tqdm(0..10000) {
        generate_compile_and_run(&constructs, i, &project, &mut client);
    }
}

fn generate_compile_and_run(
    constructs: &[Construct],
    seed: u64,
    project: &lutra_compiler::Project,
    client: &mut postgres::Client,
) {
    let mut rng = rand::rngs::SmallRng::seed_from_u64(seed);
    let expr = generate(constructs, &mut rng, 0);
    // println!("{expr}");

    let (program, _ty) =
        lutra_compiler::compile(project, &expr, None, lutra_compiler::ProgramFormat::SqlPg)
            .unwrap();

    let program = program.into_sql_pg().unwrap();
    lutra_runner_postgres::execute(client, &program, &[]).unwrap();
}

fn generate(constructs: &[Construct], rng: &mut impl Rng, depth: usize) -> String {
    if depth > 7 {
        return "false".into();
    }

    let construct = &constructs[rng.random_range(0..constructs.len())];

    let mut text = construct.text.clone();
    for i in 0..construct.sub_places {
        let sub = generate(constructs, rng, depth + 1);
        text = text.replace(&format!("${i}"), &sub);
    }
    text
}
