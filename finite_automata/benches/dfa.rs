use criterion::{black_box, criterion_group, criterion_main, Criterion};

pub fn dfa_benchmark(c: &mut Criterion) {
    // c.bench_function("10 length input", |b| {
    //     b.iter(|| dfa.check_input(black_box("ababababab")))
    // });
}

criterion_group!(benches, dfa_benchmark);
criterion_main!(benches);
