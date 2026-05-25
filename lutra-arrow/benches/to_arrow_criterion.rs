use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use lutra_bin::{Value, ir};
use rand::rngs::SmallRng;
use rand::{RngExt, SeedableRng};

fn generate_complex_tuple_array(num_rows: usize) -> (Vec<u8>, ir::Ty, Vec<ir::TyDef>) {
    let (ty, ty_defs) =
        lutra_compiler::_test_compile_ty("[{id: int32, name: text, score: float64, active: bool}]");

    let mut rng = SmallRng::seed_from_u64(42); // Reproducible seed

    let mut values = Vec::with_capacity(num_rows);
    for _ in 0..num_rows {
        let id = rng.random();
        let name_idx = rng.random_range(0..5);
        let names = ["Alice", "Bob", "Charlie", "Diana", "Eve"];
        let name = names[name_idx];
        let score: f64 = rng.random();
        let active = rng.random_bool(0.5);

        values.push(Value::Tuple(vec![
            Value::Prim32(id),
            Value::new_text(name),
            Value::Prim64(score.to_bits()),
            Value::Prim8(if active { 1 } else { 0 }),
        ]));
    }

    let value = Value::Array(values);
    let data = value.encode(&ty, &ty_defs).unwrap();

    (data, ty, ty_defs)
}

fn bench_to_arrow_complex_tuple_array(c: &mut Criterion) {
    let mut group = c.benchmark_group("to_arrow_complex_tuple_array");

    for num_rows in [100, 1000, 10000].iter() {
        group.bench_with_input(
            BenchmarkId::new("complex_tuple", num_rows),
            num_rows,
            |b, &num_rows| {
                let (data, ty, ty_defs) = generate_complex_tuple_array(num_rows);

                b.iter_with_large_drop(|| {
                    lutra_arrow::lutra_to_arrow(
                        std::hint::black_box(data.as_slice()),
                        &ty,
                        &ty_defs,
                    )
                    .unwrap()
                });
            },
        );
    }

    group.finish();
}

criterion_group!(benches, bench_to_arrow_complex_tuple_array);
criterion_main!(benches);
