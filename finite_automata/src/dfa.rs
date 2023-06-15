use std::collections::HashMap;

pub trait DfaBuilder<'a> {
    fn initial_state(self, initial_state: &'a str) -> Self;

    fn alphabet<I>(self, itr: I) -> Self
    where
        I: IntoIterator<Item = char>;

    fn accept_states<I>(self, itr: I) -> Self
    where
        I: IntoIterator<Item = &'a str>;

    fn states<I>(self, itr: I) -> Self
    where
        I: IntoIterator<Item = DfaState<'a>>;

    fn build(self) -> Result<Self, DfaError<'a>>
    where
        Self: Sized;
}

/// A deterministic finite automata.
#[derive(Debug, Default)]
pub struct Dfa<'a> {
    current_state: Option<DfaState<'a>>,
    initial_state: &'a str,
    accept_states: Vec<&'a str>,
    alphabet: Vec<char>,
    state_order: Vec<&'a str>,
    state_map: HashMap<&'a str, DfaState<'a>>,
}

impl<'a> Dfa<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn advance_with(&mut self, input_char: char) {
        let state = self.current_state.take().unwrap();
        let new_state_name = state.transitions.get(&input_char).unwrap();
        let new_state = self.state_map.get(new_state_name).unwrap().clone();
        self.current_state = Some(new_state);
    }

    #[inline]
    pub fn check_input(&mut self, input_str: &str) -> bool {
        for input in input_str.chars() {
            self.advance_with(input);
        }

        self.in_accept_state()
    }

    #[inline]
    pub fn in_accept_state(&mut self) -> bool {
        let state = self.current_state.take().unwrap();
        let accepted = self.accept_states.contains(&state.name);
        self.current_state = Some(state);
        accepted
    }

    #[inline]
    pub fn state_order(&self) -> &Vec<&'a str> {
        &self.state_order
    }
}

impl<'a> DfaBuilder<'a> for Dfa<'a> {
    fn initial_state(mut self, initial_state: &'a str) -> Self {
        self.initial_state = initial_state;
        self
    }

    fn alphabet<I>(mut self, itr: I) -> Self
    where
        I: IntoIterator<Item = char>,
    {
        self.alphabet = itr.into_iter().collect();
        self
    }

    fn accept_states<I>(mut self, itr: I) -> Self
    where
        I: IntoIterator<Item = &'a str>,
    {
        self.accept_states = itr.into_iter().collect();
        self
    }

    fn states<I>(mut self, itr: I) -> Self
    where
        I: IntoIterator<Item = DfaState<'a>>,
    {
        for state in itr.into_iter() {
            self.state_map.insert(state.name, state);
        }

        self
    }

    /// Ensures that the DFA has been correctly specified, and returns errors when relevant.
    ///
    /// **If this method is not called, then the behaviour of the Dfa cannot be guarenteed.**
    fn build(mut self) -> Result<Self, DfaError<'a>> {
        if self.alphabet.is_empty() {
            return Err(DfaError::UndefinedAlphabet);
        }

        if self.initial_state.is_empty() {
            return Err(DfaError::UndefinedInitialState);
        }

        if self.state_map.is_empty() {
            return Err(DfaError::UndefinedStates);
        } else {
            let state_names: Vec<&str> = self.state_map.values().map(|s| s.name).collect();
            // Ensuring that a transition exists for each possible input
            // for each defined state of the Dfa.
            for (_, state) in self.state_map.iter() {
                for c in self.alphabet.iter() {
                    match state.transitions.get(c) {
                        Some(transition_target) => {
                            if !state_names.contains(transition_target) {
                                return Err(DfaError::InvalidTransition(
                                    state.name,
                                    transition_target,
                                ));
                            }
                        }
                        None => return Err(DfaError::MissingTransition(state.name, *c)),
                    }
                }
            }
        }

        self.current_state = match self.state_map.get(self.initial_state) {
            Some(start_state) => Some(start_state.clone()),
            None => return Err(DfaError::InvalidInitialState),
        };

        Ok(self)
    }
}

#[derive(Debug, Clone)]
pub struct DfaState<'a> {
    name: &'a str,
    transitions: HashMap<char, &'a str>,
}

impl<'a> DfaState<'a> {
    fn new(state_name: &'a str) -> Self {
        Self {
            name: state_name,
            transitions: HashMap::new(),
        }
    }

    /// Adds a transition from the state the method is called on, via the `input_char` to the `next_state`.
    ///
    /// **If multiple transitions are added of the same input, then the last defined transition is the active one.**
    fn add_transition(mut self, input_char: char, next_state: &'a str) -> Self {
        self.transitions.insert(input_char, next_state);
        self
    }
}

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum DfaError<'a> {
    #[error("an alphabet was not defined")]
    UndefinedAlphabet,
    #[error("the initial state was not defined")]
    UndefinedInitialState,
    #[error("the states were not defined")]
    UndefinedStates,
    #[error("the initial state is not one of the defined states")]
    InvalidInitialState,
    #[error("the state, '{0}', is missing a transition for the input '{1}'")]
    MissingTransition(&'a str, char),
    #[error("a transition is defined from state '{0}' to state '{1}', however the destination is an undefined state")]
    InvalidTransition(&'a str, &'a str),
}

#[cfg(test)]
mod dfa_builder {
    use crate::dfa::{Dfa, DfaBuilder, DfaError, DfaState};

    #[test]
    fn undefined_alphabet() {
        let res = Dfa::new()
            .initial_state("q0")
            .states([DfaState::new("q0")])
            .build();
        assert!(res.is_err_and(|e| e == DfaError::UndefinedAlphabet))
    }

    #[test]
    fn undefined_initial_state() {
        let res = Dfa::new()
            .alphabet(['a', 'b'])
            .states([DfaState::new("q0")])
            .build();
        assert!(res.is_err_and(|e| e == DfaError::UndefinedInitialState))
    }

    #[test]
    fn undefined_states() {
        let res = Dfa::new().alphabet(['a', 'b']).initial_state("q0").build();
        assert!(res.is_err_and(|e| e == DfaError::UndefinedStates))
    }

    #[test]
    fn missing_transition() {
        let res = Dfa::new()
            .alphabet(['a', 'b'])
            .initial_state("q0")
            .states([
                DfaState::new("q0")
                    .add_transition('a', "q1")
                    .add_transition('b', "q1"),
                DfaState::new("q1").add_transition('a', "q1"),
            ])
            .build();

        assert!(res.is_err_and(|e| e == DfaError::MissingTransition("q1", 'b')));
    }

    #[test]
    fn invalid_initial_state() {
        let res = Dfa::new()
            .alphabet(['a', 'b'])
            .initial_state("q3")
            .states([
                DfaState::new("q0")
                    .add_transition('a', "q1")
                    .add_transition('b', "q1"),
                DfaState::new("q1")
                    .add_transition('a', "q1")
                    .add_transition('b', "q1"),
            ])
            .build();

        assert!(res.is_err_and(|e| e == DfaError::InvalidInitialState));
    }

    #[test]
    fn invalid_transition() {
        let res = Dfa::new()
            .alphabet(['a', 'b'])
            .initial_state("q0")
            .states([
                DfaState::new("q0")
                    .add_transition('a', "q1")
                    .add_transition('b', "q1"),
                DfaState::new("q1")
                    .add_transition('a', "q1")
                    .add_transition('b', "q3"),
            ])
            .build();

        assert!(res.is_err_and(|e| e == DfaError::InvalidTransition("q1", "q3")));
    }
}
