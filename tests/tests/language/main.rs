use std::path::PathBuf;

use libtest_mimic::{Arguments, Trial};
use lutra_compiler::ProgramFormat;

fn init_logger() {
    tracing_subscriber::fmt::Subscriber::builder()
        .without_time()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .try_init()
        .ok();

    // this is for making similar-asserts use colors
    console::set_colors_enabled(true);
}

fn main() {
    // Parse command line arguments
    let mut args = Arguments::from_args();
    args.color = Some(libtest_mimic::ColorSetting::Always);

    let test_file = std::fs::read_to_string("tests/language/language.lt").unwrap();
    let cases = parse_file(&test_file);

    let mut trails = Vec::new();
    for case in cases {
        let c = case.clone();
        trails.push(
            Trial::test(format!("{}::interpreter", c.name), || run_on_interpreter(c))
                .with_ignored_flag(case.ignored_interpreter),
        );

        let c = case.clone();
        trails.push(
            Trial::test(format!("{}::postgres", c.name), || run_on_pg(c))
                .with_ignored_flag(case.ignored_postgres),
        );
    }

    libtest_mimic::run(&args, trails).exit();
}

#[derive(Clone)]
struct TestCase {
    name: String,
    program: String,
    input: String,
    output: String,
    ignored_interpreter: bool,
    ignored_postgres: bool,
}

fn parse_file(contents: &str) -> Vec<TestCase> {
    let mut cases = Vec::new();
    for case in contents.split("\n# ===") {
        if case.trim().is_empty() {
            continue;
        }
        let (first_line, remaining) = case.split_once("\n").unwrap_or((case, ""));

        let (remaining, output) = remaining.split_once("\n# >").unwrap_or((remaining, ""));

        let (program, input) = remaining.split_once("\n# <").unwrap_or((remaining, ""));

        cases.push(TestCase {
            name: first_line.trim().into(),
            ignored_interpreter: program.contains("# skip: interpreter"),
            ignored_postgres: program.contains("# skip: postgres"),
            program: program.trim().into(),
            input: input.trim().into(),
            output: output.trim().into(),
        })
    }
    cases
}

#[tokio::main(flavor = "current_thread")]
async fn run_on_interpreter(case: TestCase) -> Result<(), libtest_mimic::Failed> {
    let runner = lutra_interpreter::InterpreterRunner::default();
    run_program("interpreter", ProgramFormat::BytecodeLt, runner, case).await
}

#[tokio::main(flavor = "current_thread")]
async fn run_on_pg(case: TestCase) -> Result<(), libtest_mimic::Failed> {
    const POSTGRES_URL: &str = "postgresql://postgres:pass@localhost:5416";
    let (client, connection) = tokio_postgres::connect(POSTGRES_URL, postgres::NoTls)
        .await
        .unwrap();

    tokio::spawn(async move {
        if let Err(e) = connection.await {
            eprintln!("connection error: {e}");
        }
    });

    let runner = lutra_runner_postgres::RunnerAsync::new(client);
    run_program("pg", ProgramFormat::SqlPg, runner, case).await
}

async fn run_program(
    runner_name: &'static str,
    program_format: ProgramFormat,
    runner: impl lutra_runner::Run,
    case: TestCase,
) -> Result<(), libtest_mimic::Failed> {
    crate::init_logger();

    // check an empty project
    let source_tree = lutra_compiler::SourceTree::single(PathBuf::from(""), case.program);
    let project = lutra_compiler::check(source_tree, Default::default())?;

    // compile the program
    let (program, ty) = lutra_compiler::compile(&project, "main", None, program_format)?;

    let input = if case.input.is_empty() {
        Vec::new()
    } else {
        todo!("parse lutra const expr: {}", case.input)
    };

    let res = runner.prepare(program).await;
    let handle = res.map_err(|e| format!("Run::prepare: {e:?}"))?;

    let res = runner.execute(&handle, &input).await;
    let output = res.map_err(|e| format!("Run::execute: {e:?}"))?;

    let output = lutra_bin::Value::decode(&output, &ty.output, &ty.defs)?;
    let output_source = output.print_source(&ty.output, &ty.defs)?;

    if case.output == output_source {
        Ok(())
    } else {
        Err(similar_asserts::SimpleDiff::from_str(
            &case.output,
            &output_source,
            "expected",
            runner_name,
        )
        .into())
    }
}
