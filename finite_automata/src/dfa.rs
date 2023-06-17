use std::{collections::HashMap, rc::Rc};

// region: Types

type TransitionTable = HashMap<(char, Rc<str>), Rc<str>>;

// endregion

// region: Deterministic Finite Automata

pub struct Dfa {
    alphabet: Box<[char]>,
    transitions: TransitionTable,
    initial_state: Rc<str>,
    accept_states: Box<[Rc<str>]>,
    current_state: Rc<str>,
    state_order: Vec<Rc<str>>,
}

impl<'a> Dfa {
    // region: Public API

    /// Returns a `DfaBuilder`, which is used to create a Dfa.
    ///
    /// This method exists for convenience, and simply calls the [`DfaBuilder::new`] function.
    ///
    /// **For information about the builder and examples of its usage, see the documentation of the [`DfaBuilder`]**
    pub fn builder() -> DfaBuilder<'a> {
        DfaBuilder::new()
    }

    /// Advances the Dfa with the provided input char.
    #[inline]
    pub fn advance(&mut self, input: char) -> Result<&str, DfaError<'a>> {
        if !self.alphabet.contains(&input) {
            return Err(DfaError::InvalidInputChar(input));
        }
        self.state_order.push(Rc::clone(&self.current_state));

        // all possible transitions are checked during the build,
        // as such it should be impossible to have an invalid transition at this point
        let new_state_rc = self
            .transitions
            .get(&(input, Rc::clone(&self.current_state)))
            .unwrap();
        self.current_state = Rc::clone(new_state_rc);
        Ok(new_state_rc.as_ref())
    }

    /// Checks if the provided input is accepted by the defined Dfa.
    ///
    /// **This method is self-contained, meaning it starts in the `initial state`, and resets to the `initial state` after computing the input.**
    ///
    /// Should you wish to check an input without this behaviour, then use [`check_input_without_reset`] method instead.
    pub fn check_input(&mut self, input_str: &str) -> Result<bool, DfaError<'a>> {
        self.current_state = Rc::clone(&self.initial_state);

        for c in input_str.chars() {
            self.advance(c)?;
        }

        self.current_state = Rc::clone(&self.initial_state);

        Ok(self.in_accept_state())
    }

    pub fn check_input_without_reset(&mut self, input_str: &str) -> Result<bool, DfaError<'a>> {
        for c in input_str.chars() {
            self.advance(c)?;
        }

        Ok(self.in_accept_state())
    }

    /// Returns the order in which the Dfa visited its states.
    ///
    /// This method starts by appending the current state of the Dfa to the state order,
    /// meaning the returned slice will always contain at least one state.
    #[inline]
    pub fn state_order(&mut self) -> Box<[&str]> {
        self.state_order.push(Rc::clone(&self.current_state));
        self.state_order.iter().map(|s| s.as_ref()).collect()
    }

    /// Resets the recorded state order of the Dfa.
    #[inline]
    pub fn reset_state_order(&mut self) {
        self.state_order.clear();
    }

    /// Checks if the current state of the Dfa is an accept state.
    #[inline]
    pub fn in_accept_state(&self) -> bool {
        self.accept_states.contains(&self.current_state)
    }

    /// Resets the current state of the Dfa to its initial state.
    #[inline]
    pub fn reset_state(&mut self) {
        self.current_state = Rc::clone(&self.initial_state);
    }

    // endregion

    // region: Internal Implementation

    // endregion
}

// endregion

// region: Deterministic Finite Automata Builder

#[derive(Debug, Default)]
pub struct DfaBuilder<'a> {
    alphabet: Option<&'a [char]>,
    initial_state: Option<&'a str>,
    accept_states: Option<&'a [&'a str]>,
    states: Option<&'a [DfaState<'a>]>,
}

impl<'a> DfaBuilder<'a> {
    // region: Builder API

    pub fn new() -> Self {
        Self::default()
    }

    pub fn alphabet(mut self, slice: &'a [char]) -> Self {
        self.alphabet = Some(slice);
        self
    }

    pub fn initial_state(mut self, name: &'a str) -> Self {
        self.initial_state = Some(name);
        self
    }

    pub fn accept_states(mut self, states: &'a [&'a str]) -> Self {
        self.accept_states = Some(states);
        self
    }

    pub fn states(mut self, states: &'a [DfaState<'a>]) -> Self {
        self.states = Some(states);
        self
    }

    pub fn build(self) -> Result<Dfa, DfaError<'a>> {
        use DfaError::*;

        let state_map: HashMap<&str, Rc<str>> = match self.states {
            Some(slice) => {
                let mut map = HashMap::new();
                for state in slice.iter() {
                    map.insert(state.name.as_ref(), Rc::clone(&state.name));
                }

                map
            }
            None => return Err(StatesNotDefined),
        };

        let start_state = self.generate_initial_state(&state_map)?;
        Ok(Dfa {
            alphabet: self.generate_alphabet()?,
            transitions: self.generate_transitions(&state_map)?,
            initial_state: Rc::clone(&start_state),
            accept_states: self.generate_accept_states(&state_map)?,
            current_state: start_state,
            state_order: Vec::new(),
        })
    }

    // endregion

    // region: Internal Builder Methods

    #[inline]
    fn generate_transitions(
        &self,
        state_map: &HashMap<&str, Rc<str>>,
    ) -> Result<TransitionTable, DfaError<'a>> {
        use DfaError::*;

        let slice = self.states.unwrap();
        let alphabet_slice = self.alphabet.unwrap();

        // The final transition table used in the Dfa is a HashMap of the following type
        // as such, a tuple of an input char and a current state is provided to yield the
        // next state the Dfa will transition to.
        let mut map: TransitionTable = HashMap::new();

        for state in slice.iter() {
            for c in alphabet_slice.iter() {
                // a Dfa requires that each state has a valid transition for every possible input
                if state.transitions.get(c).is_none() {
                    return Err(StateMissingTransition(state.name.as_ref(), *c));
                }
            }

            for (trans_input, trans_dest) in state.transitions.iter() {
                // all transitions must be defined with symbols that are a part of the defined alphabet
                if !alphabet_slice.contains(trans_input) {
                    return Err(InvalidCharTransition(*trans_input));
                }

                match state_map.get(trans_dest) {
                    Some(state_name_rc) => {
                        map.insert(
                            (*trans_input, Rc::clone(&state.name)),
                            Rc::clone(state_name_rc),
                        );
                    }
                    // transition destinations are only allowed within the defined states
                    None => return Err(InvalidTransitionDestination(trans_dest)),
                }
            }
        }

        Ok(map)
    }

    #[inline]
    fn generate_alphabet(&self) -> Result<Box<[char]>, DfaError<'a>> {
        use DfaError::*;

        match self.alphabet {
            Some(slice) => {
                for c in slice.iter() {
                    if !c.is_ascii() {
                        return Err(NonAsciiChar(*c));
                    }
                }

                unsafe {
                    Ok(Box::from_raw(std::slice::from_raw_parts_mut(
                        slice.as_ptr() as *mut char,
                        slice.len(),
                    )))
                }
            }
            None => Err(AlphabetNotDefined),
        }
    }

    #[inline]
    fn generate_initial_state(
        &self,
        state_map: &HashMap<&str, Rc<str>>,
    ) -> Result<Rc<str>, DfaError<'a>> {
        use DfaError::*;

        match self.initial_state {
            Some(initial_state_name) => {
                let Some(initial_state_rc) = state_map.get(initial_state_name) else {
                    return Err(InvalidInitialState(initial_state_name));
                };

                Ok(Rc::clone(initial_state_rc))
            }
            None => Err(InitialStateNotDefined),
        }
    }

    #[inline]
    fn generate_accept_states(
        &self,
        state_map: &HashMap<&str, Rc<str>>,
    ) -> Result<Box<[Rc<str>]>, DfaError<'a>> {
        use DfaError::*;

        match self.accept_states {
            Some(slice) => {
                let mut rc_vec = Vec::new();

                for state_name in slice {
                    match state_map.get(state_name) {
                        Some(state_name_rc) => rc_vec.push(Rc::clone(state_name_rc)),
                        None => return Err(InvalidAcceptState(state_name)),
                    }
                }

                Ok(rc_vec.into_boxed_slice())
            }
            None => Err(AcceptStatesNotDefined),
        }
    }

    // endregion
}

// endregion

// region: Deterministic Finite Automata State

#[derive(Debug)]
pub struct DfaState<'a> {
    name: Rc<str>,
    transitions: HashMap<char, &'a str>,
}

impl<'a> DfaState<'a> {
    pub fn new(name: &str) -> Self {
        Self {
            name: Rc::from(name),
            transitions: HashMap::new(),
        }
    }

    pub fn transition(mut self, input: char, new_state: &'a str) -> Self {
        self.transitions.insert(input, new_state);
        self
    }
}

// endregion

// region: Deterministic Finite Automata Errors

use thiserror::Error;

#[derive(Debug, Error)]
pub enum DfaError<'a> {
    #[error("the alphabet for the dfa was not defined")]
    AlphabetNotDefined,
    #[error("'{0}' is not an ASCII character")]
    NonAsciiChar(char),
    #[error("the states of the dfa were not defined")]
    StatesNotDefined,
    #[error("'{0}' is not a part of the defined alphabet, and cannot be used in a transition")]
    InvalidCharTransition(char),
    #[error("the state '{0}' does not have a transition for the alphabet character '{1}'")]
    StateMissingTransition(&'a str, char),
    #[error("'{0}' is not one of the defined states, and cannot be used as a destination for a transition")]
    InvalidTransitionDestination(&'a str),
    #[error("the initial state was not defined")]
    InitialStateNotDefined,
    #[error("'{0}' is not one of the defined states, and cannot be used as the initial state")]
    InvalidInitialState(&'a str),
    #[error("the accept states were not defined")]
    AcceptStatesNotDefined,
    #[error("'{0}' is not one of the defined states, and cannot be used as an accept state")]
    InvalidAcceptState(&'a str),
    #[error("'{0}' is not a part of the defined alphabet")]
    InvalidInputChar(char),
}

// endregion

// region: Tests

#[cfg(test)]
mod dfa_builder {}

#[cfg(test)]
mod public_api {}

// endregion
