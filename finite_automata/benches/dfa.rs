use criterion::{black_box, criterion_group, criterion_main, Criterion};
use finite_automata::dfa::{Dfa, DfaState};

pub fn dfa_benchmark(c: &mut Criterion) {
    let mut dfa = Dfa::builder()
        .alphabet(&['a', 'b'])
        .initial_state("q1")
        .accept_states(&["q3"])
        .states(&[
            DfaState::new("q1")
                .transition('a', "q2")
                .transition('b', "q1"),
            DfaState::new("q2")
                .transition('a', "q2")
                .transition('b', "q3"),
            DfaState::new("q3")
                .transition('a', "q2")
                .transition('b', "q1"),
        ])
        .build()
        .expect("failed to build dfa");

    c.bench_function("10 length input", |b| {
        b.iter(|| dfa.check_input(black_box("ababababab")))
    });
}

criterion_group!(benches, dfa_benchmark);
criterion_main!(benches);
