use std::collections::HashMap;

/// A deterministic finite automata.
pub struct Dfa<'a> {
    initial_state: State<'a>,
    accept_states: Vec<State<'a>>,
    transition_table: HashMap<Transition<'a>, State<'a>>,
    current_state: State<'a>,
    state_order: Vec<State<'a>>,
}

impl<'a> Dfa<'a> {
    pub fn builder() -> DfaBuilder<'a> {
        DfaBuilder::default()
    }

    fn new(
        initial_state: State<'a>,
        accept_states: Vec<State<'a>>,
        transition_table: HashMap<Transition<'a>, State<'a>>,
        current_state: State<'a>,
    ) -> Self {
        Self {
            initial_state,
            accept_states,
            transition_table,
            current_state,
            state_order: Vec::new(),
        }
    }

    /// Returns true if the current state of the finite automaton is an accept state.
    ///
    /// # Examples
    /// In the following example the Dfa is initially not in an accept state,
    /// advancing the state machine with the input `a` brings the Dfa into an accept state.
    /// ```
    /// # use fa::dfa::{Dfa, DfaBuilder};
    /// # let mut dfa = Dfa::builder()
    /// #     .initial_state("q0")
    /// #     .accept_states(["q1"])
    /// #     .transitions([
    /// #         ('a', "q0", "q1"),
    /// #         ('b', "q0", "Ø"),
    /// #         ('a', "q1", "Ø"),
    /// #         ('b', "q1", "q1"),
    /// #     ])
    /// #     .build();
    /// assert!(!dfa.in_accept_state());
    /// dfa.advance_with('a');
    /// assert!(dfa.in_accept_state());
    /// ```
    #[inline(always)]
    pub fn in_accept_state(&self) -> bool {
        self.accept_states.contains(&self.current_state)
    }

    /// Advances the finite automata with the provided input.
    #[inline(always)]
    pub fn advance_with(&mut self, input: char) {
        let transition = Transition::new_with_state(input, self.current_state.clone());
        match self.transition_table.get(&transition) {
            Some(next_state) => {
                self.current_state = next_state.clone();
                self.state_order.push(self.current_state.clone());
            }
            None => todo!(),
        }
    }

    /// Returns the route the finite automata has taken through its states.
    pub fn state_order(&self) -> Vec<&str> {
        let mut state_order = Vec::new();
        for state in self.state_order.iter() {
            state_order.push(state.state_name);
        }
        state_order
    }

    /// Checks the specified input against the defined finite automata.
    ///
    /// The function returns true if the input is accepted.
    pub fn check_input(&mut self, input_str: &str) -> bool {
        self.current_state = self.initial_state.clone();
        for input in input_str.chars() {
            self.advance_with(input);
        }

        self.in_accept_state()
    }
}

#[derive(Default)]
pub struct DfaBuilder<'a> {
    initial_state: Option<&'a str>,
    accept_states: Option<Vec<&'a str>>,
    transition_table: Option<Vec<(char, &'a str, &'a str)>>,
}

impl<'a> DfaBuilder<'a> {
    pub fn initial_state(mut self, state: &'a str) -> Self {
        self.initial_state = Some(state);
        self
    }

    pub fn accept_states<I>(mut self, accept_states: I) -> Self
    where
        I: IntoIterator<Item = &'a str>,
    {
        self.accept_states = Some(accept_states.into_iter().collect());
        self
    }

    /// The input slice consists of a three-tuple of the format `(input, current_state, next_state)`
    ///
    /// As such, for a `Dfa` in `current_state`, `input` will move it to `next_state`.
    pub fn transitions<I>(mut self, transitions: I) -> Self
    where
        I: IntoIterator<Item = (char, &'a str, &'a str)>,
    {
        self.transition_table = Some(transitions.into_iter().collect());
        self
    }

    pub fn build(self) -> Dfa<'a> {
        let initial_state = match self.initial_state {
            Some(state) => State::new(state),
            None => todo!(),
        };

        let transitions = match self.transition_table {
            // The some arm should check and ensure that duplicates are not made (i.e. that they same key is not used twice).
            Some(transition_table) => {
                let mut transitions: HashMap<Transition<'a>, State<'a>> = HashMap::new();
                for (input, current, next) in transition_table {
                    let transition = Transition::new(input, current);
                    let new_state = State::new(next);
                    transitions.insert(transition, new_state);
                }
                transitions
            }
            None => todo!(),
        };

        let accept_states = match self.accept_states {
            Some(slice) => {
                let acccept_states_vec: Vec<State<'a>> =
                    slice.iter().map(|s| State::new(s)).collect();
                acccept_states_vec
            }
            None => todo!(),
        };

        let current_state = initial_state.clone();
        Dfa::new(initial_state, accept_states, transitions, current_state)
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct Transition<'a> {
    input: char,
    current_state: State<'a>,
}

impl<'a> Transition<'a> {
    fn new(input: char, state: &'a str) -> Self {
        Self {
            input,
            current_state: State::new(state),
        }
    }

    fn new_with_state(input: char, state: State<'a>) -> Self {
        Self {
            input,
            current_state: state,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct State<'a> {
    state_name: &'a str,
}

impl<'a> State<'a> {
    fn new(state_name: &'a str) -> Self {
        Self { state_name }
    }
}

#[cfg(test)]
mod test {
    use super::Dfa;

    #[test]
    fn valid_input() {
        let mut dfa = Dfa::builder()
            .initial_state("q1")
            .accept_states(["q2"])
            .transitions([
                ('b', "q1", "q1"),
                ('a', "q1", "q2"),
                ('a', "q2", "q3"),
                ('b', "q2", "q3"),
                ('a', "q3", "q3"),
                ('b', "q3", "q3"),
            ])
            .build();

        assert!(dfa.check_input("bbbbbbbbbba"));
    }

    #[test]
    fn invalid_input() {
        let mut dfa = Dfa::builder()
            .initial_state("q1")
            .accept_states(["q2"])
            .transitions([
                ('b', "q1", "q1"),
                ('a', "q1", "q2"),
                ('a', "q2", "q3"),
                ('b', "q2", "q3"),
                ('a', "q3", "q3"),
                ('b', "q3", "q3"),
            ])
            .build();

        assert!(!dfa.check_input("bbbbbbbbbbaaa"));
    }
}
