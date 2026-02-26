use criterion::{BenchmarkId, Criterion, black_box, criterion_group, criterion_main};
use lutra_bin::{Value, ir};
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};

fn generate_complex_tuple_array(num_rows: usize) -> (Vec<u8>, ir::Ty) {
    let ty =
        lutra_compiler::_test_compile_ty("[{id: int32, name: text, score: float64, active: bool}]");

    let mut rng = SmallRng::seed_from_u64(42); // Reproducible seed

    let mut values = Vec::with_capacity(num_rows);
    for _ in 0..num_rows {
        let id: u32 = rng.r#gen();
        let name_idx = rng.gen_range(0..5);
        let names = ["Alice", "Bob", "Charlie", "Diana", "Eve"];
        let name = names[name_idx];
        let score: f64 = rng.r#gen();
        let active = rng.gen_bool(0.5);

        values.push(Value::Tuple(vec![
            Value::Prim32(id),
            Value::Text(name.into()),
            Value::Prim64(score.to_bits()),
            Value::Prim8(if active { 1 } else { 0 }),
        ]));
    }

    let value = Value::Array(values);
    let data = value.encode(&ty, &[]).unwrap();

    (data, ty)
}

fn bench_to_arrow_complex_tuple_array(c: &mut Criterion) {
    let mut group = c.benchmark_group("to_arrow_complex_tuple_array");

    for num_rows in [100, 1000, 10000].iter() {
        group.bench_with_input(
            BenchmarkId::new("complex_tuple", num_rows),
            num_rows,
            |b, &num_rows| {
                let (data, ty) = generate_complex_tuple_array(num_rows);

                b.iter_with_large_drop(|| {
                    lutra_arrow::lutra_to_arrow(black_box(data.as_slice()), &ty, &[]).unwrap()
                });
            },
        );
    }

    group.finish();
}

criterion_group!(benches, bench_to_arrow_complex_tuple_array);
criterion_main!(benches);
