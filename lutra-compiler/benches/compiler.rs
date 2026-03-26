/// Benchmarks for the Lutra compiler pipeline.
///
/// Three phases are measured independently so that regressions can be
/// pinpointed:
///
/// * `check` – lex + parse + resolve the user source *and* std.lt.
/// * `compile`– lower -> inline -> layout -> SQL/bytecode backend,
///   given an already-checked `Project`.
/// * `full` - `check` + `compile` end-to-end (what a CLI user pays).
///
/// The program under test is intentionally tiny so that the fixed cost of
/// compiling std.lt dominates, which is what we care about here.
use criterion::{Criterion, black_box, criterion_group, criterion_main};
use lutra_compiler::{_bench, _lexer, CheckParams, ProgramFormat, SourceTree, check, compile};

const PROG: &str = "func main() -> std::add(1, 2): int32";
const EXPR_PROG: &str = "std::add(1, 2 * 3 ?? 4)";
const SOURCE_PROG: &str = r#"
func main() -> std::add(1, 2 * 3 ?? 4): int32
"#;

fn bench_check(c: &mut Criterion) {
    let mut group = c.benchmark_group("check");

    group.bench_function("std_lib", |b| {
        b.iter(|| _bench::check_std_lib());
    });

    let source = SourceTree::single("".into(), PROG.to_string());
    group.bench_function("source", |b| {
        b.iter(|| check(black_box(source.clone()), CheckParams::default()).unwrap());
    });
    group.finish();
}

fn bench_parser(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser");

    group.bench_function("lexer", |b| {
        b.iter(|| {
            let (tokens, errors) = _lexer::lex(black_box(SOURCE_PROG), 0);
            black_box(tokens);
            assert!(errors.is_empty(), "lexer produced errors: {errors:?}");
        });
    });

    group.bench_function("lexer+parse_expr", |b| {
        b.iter(|| {
            let (ast, errors) = _bench::parse_expr(black_box(EXPR_PROG), 0);
            black_box(ast);
            assert!(errors.is_empty(), "parse_expr produced errors: {errors:?}");
        });
    });

    group.bench_function("lexer+parse_source", |b| {
        b.iter(|| {
            let (ast, errors, trivia) = _bench::parse_source(black_box(SOURCE_PROG), 0);
            black_box(ast);
            black_box(trivia);
            assert!(
                errors.is_empty(),
                "parse_source produced errors: {errors:?}"
            );
        });
    });

    group.finish();
}

fn bench_compile(c: &mut Criterion) {
    // Build the project once; we only want to measure the compile step.
    let source = SourceTree::single("".into(), PROG.to_string());
    let project = check(source, CheckParams::default()).unwrap();

    let mut group = c.benchmark_group("compile");
    group.bench_function("bytecode", |b| {
        b.iter(|| compile(black_box(&project), "main", None, ProgramFormat::BytecodeLt).unwrap());
    });
    group.bench_function("sql_duckdb", |b| {
        b.iter(|| compile(black_box(&project), "main", None, ProgramFormat::SqlDuckdb).unwrap());
    });
    group.finish();
}

criterion_group!(benches, bench_parser, bench_check, bench_compile);
criterion_main!(benches);
