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
use lutra_compiler::{CheckParams, ProgramFormat, SourceTree, check, compile};

const PROG: &str = "func main() -> std::add(1, 2): int32";

fn bench_check(c: &mut Criterion) {
    c.bench_function("check", |b| {
        b.iter(|| {
            let source = SourceTree::single("".into(), black_box(PROG).to_string());
            check(source, CheckParams::default()).unwrap()
        });
    });
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

fn bench_full(c: &mut Criterion) {
    let mut group = c.benchmark_group("full");
    group.bench_function("bytecode", |b| {
        b.iter(|| {
            let source = SourceTree::single("".into(), black_box(PROG).to_string());
            let project = check(source, CheckParams::default()).unwrap();
            compile(&project, "main", None, ProgramFormat::BytecodeLt).unwrap()
        });
    });
    group.bench_function("sql_duckdb", |b| {
        b.iter(|| {
            let source = SourceTree::single("".into(), black_box(PROG).to_string());
            let project = check(source, CheckParams::default()).unwrap();
            compile(&project, "main", None, ProgramFormat::SqlDuckdb).unwrap()
        });
    });
    group.finish();
}

criterion_group!(benches, bench_check, bench_compile, bench_full);
criterion_main!(benches);
