use finite_automata::dfa::{Dfa, DfaState};

fn main() {
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

    let res = dfa.check_input("ababababab").unwrap();
    println!("{}", res);

    println!("this is my very last thing")
}
