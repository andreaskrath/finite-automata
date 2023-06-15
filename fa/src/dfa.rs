use std::collections::HashMap;

/// A deterministic finite automata.
pub struct Dfa<'a> {
    initial_state: State<'a>,
    accept_states: Vec<&'a str>,
    state_map: HashMap<&'a str, State<'a>>,
    current_state: State<'a>,
    state_order: Vec<&'a str>,
}

impl<'a> Dfa<'a> {
    pub fn builder() -> DfaBuilder<'a> {
        DfaBuilder::default()
    }

    fn new(
        initial_state: State<'a>,
        accept_states: Vec<&'a str>,
        state_map: HashMap<&'a str, State<'a>>,
        current_state: State<'a>,
    ) -> Self {
        Self {
            initial_state,
            accept_states,
            state_map,
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
    /// # use fa::dfa::{Dfa, State};
    /// # let mut dfa = Dfa::builder()
    /// #     .initial_state("q0")
    /// #     .accept_states(["q1"])
    /// #     .states([
    /// #         State::new("q0")
    /// #             .add_transition('a', "q1")
    /// #             .add_transition('b', "Ø"),
    /// #         State::new("q1")
    /// #             .add_transition('a', "Ø")
    /// #             .add_transition('b', "q1"),
    /// #         State::new("Ø")
    /// #             .add_transition('a', "Ø")
    /// #             .add_transition('b', "Ø"),
    /// #     ])
    /// #     .build();
    /// assert!(!dfa.in_accept_state());
    /// dfa.advance_with('a');
    /// assert!(dfa.in_accept_state());
    /// ```
    #[inline(always)]
    pub fn in_accept_state(&self) -> bool {
        self.accept_states.contains(&self.current_state.state_name)
    }

    /// Advances the finite automata with the provided input.
    #[inline(always)]
    pub fn advance_with(&mut self, input: char) {
        match self.current_state.transitions.get(&input) {
            Some(next_state_name) => {
                self.current_state = match self.state_map.get(next_state_name) {
                    Some(next_state) => next_state.clone(),
                    None => todo!(),
                };
                self.state_order.push(self.current_state.state_name);
            }
            None => {
                println!("input: {}, state: {:#?}", input, self.current_state);
                todo!();
            }
        }
    }

    /// Returns the route the finite automata has taken through its states.
    pub fn state_order(&self) -> &Vec<&str> {
        &self.state_order
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
    states: Option<Vec<State<'a>>>,
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
    pub fn states<I>(mut self, states: I) -> Self
    where
        I: IntoIterator<Item = State<'a>>,
    {
        self.states = Some(states.into_iter().collect());
        self
    }

    pub fn build(mut self) -> Dfa<'a> {
        let state_map = match self.states {
            // The some arm should check and ensure that duplicates are not made (i.e. that they same key is not used twice).
            Some(states) => {
                let mut state_map: HashMap<&'a str, State<'a>> = HashMap::new();
                for state in states {
                    state_map.insert(state.state_name, state);
                }
                state_map
            }
            None => todo!(),
        };

        let initial_state = match self.initial_state {
            Some(state_name) => match state_map.get(state_name) {
                Some(state) => state.clone(),
                None => todo!(),
            },
            None => todo!(),
        };

        let current_state = initial_state.clone();
        Dfa::new(
            initial_state,
            self.accept_states.take().unwrap(),
            state_map,
            current_state,
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct State<'a> {
    state_name: &'a str,
    transitions: HashMap<char, &'a str>,
}

impl<'a> State<'a> {
    pub fn new(state_name: &'a str) -> Self {
        Self {
            state_name,
            transitions: HashMap::new(),
        }
    }

    pub fn add_transition(mut self, input_char: char, next_state: &'a str) -> Self {
        // This should be checked to ensure that double booking of paths does not happen.
        self.transitions.insert(input_char, next_state);
        self
    }
}

#[cfg(test)]
mod test {
    use crate::dfa::{Dfa, State};

    #[test]
    fn valid_input() {
        let mut dfa = Dfa::builder()
            .initial_state("q1")
            .accept_states(["q2"])
            .states([
                State::new("q1")
                    .add_transition('a', "q2")
                    .add_transition('b', "q1"),
                State::new("q2")
                    .add_transition('a', "q3")
                    .add_transition('b', "q3"),
                State::new("q3")
                    .add_transition('a', "q3")
                    .add_transition('b', "q3"),
            ])
            .build();

        assert!(dfa.check_input("bbbbbbbbbba"));
    }

    #[test]
    fn invalid_input() {
        let mut dfa = Dfa::builder()
            .initial_state("q1")
            .accept_states(["q2"])
            .states([
                State::new("q1")
                    .add_transition('a', "q2")
                    .add_transition('b', "q1"),
                State::new("q2")
                    .add_transition('a', "q3")
                    .add_transition('b', "q3"),
                State::new("q3")
                    .add_transition('a', "q3")
                    .add_transition('b', "q3"),
            ])
            .build();

        assert!(!dfa.check_input("bbbbbbbbbbaaa"));
    }
}
